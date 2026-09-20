{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Env
  ( EnvConfig(..)
  , EnvError(..)
  , renderEnvError
  , CliOptions(..)
  , OutputFormat(..)
  , defaultCliOptions
  , parseEnvContent
  , parseLineTwoModel
  , parseCliArgs
  , CliFlag(..)
  , cliFlags
  , cliHelpText
  , cliUsageHint
  , StartupIntent(..)
  , startupIntent
  , headlessEmitsBanners
  , headlessVerbose
  , formatPrintResult
  , formatUsd
  , resolvePermissionMode
  , resolveMaxBudgetUsd
  , resolveWorkingDirs
  , resolveEffortLevel
  , resolveConfigWith
  , resolveConfigWithSettings
  , resolveEnvConfig
  , loadEnvConfig
  , loadProjectInstructions
  , loadProjectInstructionsFile
  , buildSystemPrompt
  , buildSystemPromptWithAppend
  ) where

import Hach.Settings
  ( Settings(..)
  , SettingsError
  , defaultSettings
  , loadLayeredSettings
  , renderSettingsError
  )
import Hach.Types (AgentResult(..), EffortLevel, PermissionMode(..), parseEffortLevel)
import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Parsed environment configuration for running the agent harness.
data EnvConfig = EnvConfig
  { envApiKey   :: !Text
  , envModel    :: !Text
  , envSettings :: !Settings
  } deriving (Show, Eq)

-- | Output format for CLI.
data OutputFormat = OutputText | OutputJson
  deriving (Show, Eq)

-- | CLI options parsed from command line arguments.
data CliOptions = CliOptions
  { optModel                :: !(Maybe Text)
  , optPrompt               :: !(Maybe Text)
  , optNoTui                :: !Bool
  , optPrint                :: !Bool
  , optOutputFormat         :: !OutputFormat
  , optContinue             :: !Bool
  , optResume               :: !Bool
  , optSessionId            :: !(Maybe Text)
  , optMaxTurns             :: !(Maybe Int)
  , optMaxBudgetUsd         :: !(Maybe Double)
  , optAppendSystemPrompt   :: !(Maybe Text)
  , optAddDir               :: ![FilePath]
  , optWorktree             :: !(Maybe Text)
  , optInit                 :: !Bool
  , optExec                 :: !(Maybe Text)
  , optPermissionMode       :: !(Maybe PermissionMode)
  , optDangerouslySkipPerms :: !Bool
  , optVersion              :: !Bool
  , optHelp                 :: !Bool
  } deriving (Show, Eq)

-- | Default empty CLI options.
defaultCliOptions :: CliOptions
defaultCliOptions = CliOptions
  { optModel                = Nothing
  , optPrompt               = Nothing
  , optNoTui                = False
  , optPrint                = False
  , optOutputFormat         = OutputText
  , optContinue             = False
  , optResume               = False
  , optSessionId            = Nothing
  , optMaxTurns             = Nothing
  , optMaxBudgetUsd         = Nothing
  , optAppendSystemPrompt   = Nothing
  , optAddDir               = []
  , optWorktree             = Nothing
  , optInit                 = False
  , optExec                 = Nothing
  , optPermissionMode       = Nothing
  , optDangerouslySkipPerms = False
  , optVersion              = False
  , optHelp                 = False
  }

-- | Parse output format string.
parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat s = case map toLower s of
  "json" -> Just OutputJson
  "text" -> Just OutputText
  _      -> Nothing

-- | Parse permission mode from string.
parsePermMode :: String -> Maybe PermissionMode
parsePermMode s = case map toLower s of
  "default"            -> Just ModeDefault
  "acceptedits"        -> Just ModeAcceptEdits
  "accept_edits"       -> Just ModeAcceptEdits
  "accept-edits"       -> Just ModeAcceptEdits
  "plan"               -> Just ModePlan
  "auto"               -> Just ModeAuto
  "dontask"            -> Just ModeDontAsk
  "dont_ask"           -> Just ModeDontAsk
  "dont-ask"           -> Just ModeDontAsk
  "bypasspermissions"  -> Just ModeBypassPermissions
  "bypass_permissions" -> Just ModeBypassPermissions
  "bypass-permissions" -> Just ModeBypassPermissions
  _                    -> Nothing

-- | Effective permission mode for a run. Precedence:
-- @--dangerously-skip-permissions@ > @--permission-mode@ flag > layered
-- settings > 'ModeDefault'.
resolvePermissionMode
  :: Maybe PermissionMode   -- ^ @--permission-mode@ flag
  -> Bool                   -- ^ @--dangerously-skip-permissions@
  -> Settings               -- ^ Layered settings
  -> PermissionMode
resolvePermissionMode mFlag skipPerms settings
  | skipPerms = ModeBypassPermissions
  | otherwise = fromMaybe ModeDefault (mFlag <|> setPermissionMode settings)

-- | Spending ceiling for a run. The CLI flag overrides settings.json.
-- Non-finite or negative settings values are ignored.
resolveMaxBudgetUsd
  :: Maybe Double
  -> Settings
  -> Maybe Double
resolveMaxBudgetUsd mFlag settings =
  mFlag <|> (setMaxBudgetUsd settings >>= finiteNonNegativeUsd)

finiteNonNegativeUsd :: Double -> Maybe Double
finiteNonNegativeUsd d
  | d >= 0 && not (isNaN d) && not (isInfinite d) = Just d
  | otherwise = Nothing

-- | Additional directories tools may reach, beyond the primary workspace
-- root. @--add-dir@ flags and the @working_directories@ setting accumulate
-- rather than override: a repeated directory is kept once, flags first.
resolveWorkingDirs
  :: [FilePath]             -- ^ @--add-dir@ flags, in command-line order
  -> Settings               -- ^ Layered settings
  -> [FilePath]
resolveWorkingDirs flagDirs settings =
  ordNubPaths (filter (not . null) (flagDirs ++ setWorkingDirs settings))

-- | Order-preserving unique: first occurrence wins.
ordNubPaths :: [FilePath] -> [FilePath]
ordNubPaths = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert x seen) xs

formatUsd :: Double -> Text
formatUsd d = T.pack (printf "$%.2f" d)

-- | Resolve `effort_level` from layered settings. Unset stays unset so the
-- OpenRouter request omits `reasoning`. Unsupported values are an error.
resolveEffortLevel :: Settings -> Either String (Maybe EffortLevel)
resolveEffortLevel settings =
  case setEffortLevel settings of
    Nothing -> Right Nothing
    Just raw -> Just <$> parseEffortLevel raw

-- | What the CLI should do after parsing. '--exec' is a real command to run,
-- not a prompt for the headless agent; '--init' initialises the workspace
-- guidelines file and exits; '--version' outranks both, and '--help'
-- outranks everything.
data StartupIntent
  = IntentHelp
  | IntentVersion
  | IntentExec !Text
  | IntentInit
  | IntentTui
  | IntentHeadless
  deriving (Show, Eq)

-- | Map parsed options onto a startup intent. '--exec' must not fall through
-- to the headless agent loop just because it also sets 'optNoTui'. '--init'
-- must not fall through to the TUI or headless loop (Issue #118).
startupIntent :: CliOptions -> StartupIntent
startupIntent CliOptions{..}
  | optHelp = IntentHelp
  | optVersion = IntentVersion
  | Just cmd <- optExec = IntentExec cmd
  | optInit = IntentInit
  | optNoTui = IntentHeadless
  | otherwise = IntentTui

-- | Decorative startup banners belong to interactive/--no-tui headless
-- runs. '--print' / '-p' is scripted: stdout is only the final answer.
headlessEmitsBanners :: CliOptions -> Bool
headlessEmitsBanners CliOptions{..} = not optPrint

-- | Intermediate turn/tool events are logged unless '--print' / '-p'.
headlessVerbose :: CliOptions -> Bool
headlessVerbose CliOptions{..} = not optPrint

-- | Format the agent result for '--print' / '-p' stdout.
formatPrintResult :: OutputFormat -> AgentResult -> Text
formatPrintResult fmt result = case fmt of
  OutputText -> case result of
    AgentCompleted ans -> ans
    AgentMaxTurnsReached turns ->
      T.pack ("Agent reached maximum turn limit of " <> show turns <> ".")
    AgentBudgetExceeded spent budget ->
      "Agent reached the spending budget of "
        <> formatUsd budget
        <> " (spent "
        <> formatUsd spent
        <> ")."
    AgentFailed err -> err
  OutputJson ->
    TE.decodeUtf8 . LBS.toStrict . Aeson.encode $ case result of
      AgentCompleted ans ->
        Aeson.object ["answer" .= ans]
      AgentMaxTurnsReached turns ->
        Aeson.object
          [ "error" .= ("max_turns" :: Text)
          , "turns" .= turns
          ]
      AgentBudgetExceeded spent budget ->
        Aeson.object
          [ "error" .= ("max_budget" :: Text)
          , "spent" .= spent
          , "budget" .= budget
          ]
      AgentFailed err ->
        Aeson.object ["error" .= err]

-- | One documented command-line option. 'cliFlags' sits beside
-- 'parseCliArgs' so the help text and the parser are kept in step; the test
-- suite checks each direction (Issue #153).
data CliFlag = CliFlag
  { cliFlagNames   :: ![String]
  , cliFlagMetavar :: !(Maybe String)
  , cliFlagSummary :: !String
  } deriving (Show, Eq)

-- | Every option 'parseCliArgs' accepts, in help-text order.
cliFlags :: [CliFlag]
cliFlags =
  [ CliFlag ["-h", "--help"] Nothing "Show this help and exit"
  , CliFlag ["-v", "--version"] Nothing "Print the version and exit"
  , CliFlag ["-m", "--model"] (Just "MODEL") "OpenRouter model to use (overrides .env)"
  , CliFlag ["--no-tui"] Nothing "Run the agent headless instead of the terminal UI"
  , CliFlag ["-p", "--print"] Nothing "Run headless and print only the final answer"
  , CliFlag ["--output-format"] (Just "text|json") "Format of the --print answer (default: text)"
  , CliFlag ["-c", "--continue"] Nothing "Continue the most recent session in this workspace"
  , CliFlag ["-r", "--resume"] Nothing "Resume the most recent session in this workspace"
  , CliFlag ["--session-id"] (Just "ID") "Resume the session with this ID"
  , CliFlag ["--max-turns"] (Just "N") "Stop the agent after N turns"
  , CliFlag ["--max-budget-usd"] (Just "USD") "Stop the agent after spending this many US dollars"
  , CliFlag ["--append-system-prompt"] (Just "TEXT") "Append TEXT to the system prompt"
  , CliFlag ["--add-dir"] (Just "DIR") "Add a working directory tools may reach (repeatable)"
  , CliFlag ["-w", "--worktree"] (Just "NAME") "Work in the git worktree NAME, creating it if needed"
  , CliFlag ["--init"] Nothing "Create a CLAUDE.md guidelines template and exit"
  , CliFlag ["--exec"] (Just "CMD") "Run the shell command CMD in the workspace and exit"
  , CliFlag ["--permission-mode"] (Just "MODE")
      "default, acceptEdits, plan, auto, dontAsk or bypassPermissions"
  , CliFlag ["--dangerously-skip-permissions"] Nothing "Skip all permission prompts"
  , CliFlag ["--"] Nothing "Treat every remaining argument as prompt text"
  ]

-- | Full '--help' output: a usage line and one aligned row per 'CliFlag'.
cliHelpText :: String
cliHelpText = unlines $
  [ "Usage: hach [options] [task prompt...]"
  , ""
  , "Options:"
  ] ++ map row cliFlags
  where
    label CliFlag{..} =
      intercalate ", " cliFlagNames ++ maybe "" (' ' :) cliFlagMetavar
    width = maximum (map (length . label) cliFlags)
    row flag = "  " ++ padRight width (label flag) ++ "  " ++ cliFlagSummary flag
    padRight n s = s ++ replicate (n - length s) ' '

-- | Usage shown after an argument error.
cliUsageHint :: String
cliUsageHint = "Usage: hach [options] [task prompt...]\nRun 'hach --help' to list every option."

-- | Parse command line arguments into 'CliOptions'.
parseCliArgs :: [String] -> Either String CliOptions
parseCliArgs args = go args defaultCliOptions []
  where
    go [] opts promptWords =
      let mPrompt = case promptWords of
            [] -> Nothing
            ws ->
              let raw = T.strip (T.pack (unwords ws))
              in if T.null raw then Nothing else Just raw
      in Right opts { optPrompt = mPrompt }

    go ("--" : rest) opts promptWords =
      go [] opts (promptWords ++ rest)

    -- '--help' short-circuits: whatever follows cannot turn it into an error.
    go (arg : _) opts _
      | arg `elem` ["--help", "-h"] = Right opts { optHelp = True }

    go ("--no-tui" : rest) opts promptWords =
      go rest opts { optNoTui = True } promptWords

    go ("--print" : rest) opts promptWords =
      go rest opts { optPrint = True, optNoTui = True } promptWords
    go ("-p" : rest) opts promptWords =
      go rest opts { optPrint = True, optNoTui = True } promptWords

    go ("--continue" : rest) opts promptWords =
      go rest opts { optContinue = True } promptWords
    go ("-c" : rest) opts promptWords =
      go rest opts { optContinue = True } promptWords

    go ("--resume" : rest) opts promptWords =
      go rest opts { optResume = True } promptWords
    go ("-r" : rest) opts promptWords =
      go rest opts { optResume = True } promptWords

    go ("--init" : rest) opts promptWords =
      go rest opts { optInit = True } promptWords

    go ("--dangerously-skip-permissions" : rest) opts promptWords =
      go rest opts { optDangerouslySkipPerms = True } promptWords

    go ("--version" : rest) opts promptWords =
      go rest opts { optVersion = True } promptWords
    go ("-v" : rest) opts promptWords =
      go rest opts { optVersion = True } promptWords

    go (arg : rest) opts promptWords
      | arg `elem` ["--model", "-m"] =
          case rest of
            (val : rest')
              | null (dropWhile isSpace val) -> Left (arg ++ " requires a non-empty argument")
              | "--" `isPrefixOf` val -> Left (arg ++ " requires a non-flag argument")
              | otherwise ->
                  go rest' opts { optModel = Just (T.strip (T.pack val)) } promptWords
            [] -> Left (arg ++ " requires an argument")

      | Just val <- stripPrefix "--model=" arg =
          if null (dropWhile isSpace val)
            then Left "--model= requires a non-empty argument"
            else go rest opts { optModel = Just (T.strip (T.pack val)) } promptWords

      | Just val <- stripPrefix "-m=" arg =
          if null (dropWhile isSpace val)
            then Left "-m= requires a non-empty argument"
            else go rest opts { optModel = Just (T.strip (T.pack val)) } promptWords

      | arg == "--output-format" =
          case rest of
            (val : rest') -> case parseOutputFormat val of
              Just fmt -> go rest' opts { optOutputFormat = fmt } promptWords
              Nothing  -> Left ("Invalid output format: " ++ val)
            [] -> Left "--output-format requires an argument"

      | Just val <- stripPrefix "--output-format=" arg =
          case parseOutputFormat val of
            Just fmt -> go rest opts { optOutputFormat = fmt } promptWords
            Nothing  -> Left ("Invalid output format: " ++ val)

      | arg == "--session-id" =
          case rest of
            (val : rest')
              | not (null (dropWhile isSpace val)) ->
                  go rest' opts { optSessionId = Just (T.strip (T.pack val)) } promptWords
              | otherwise -> Left "--session-id requires a non-empty argument"
            [] -> Left "--session-id requires an argument"

      | Just val <- stripPrefix "--session-id=" arg =
          if null (dropWhile isSpace val)
            then Left "--session-id= requires a non-empty argument"
            else go rest opts { optSessionId = Just (T.strip (T.pack val)) } promptWords

      | arg == "--max-turns" =
          case rest of
            (val : rest') -> case readMaybe val of
              Just n | n > 0 -> go rest' opts { optMaxTurns = Just n } promptWords
              _              -> Left "--max-turns requires a positive integer"
            [] -> Left "--max-turns requires an argument"

      | Just val <- stripPrefix "--max-turns=" arg =
          case readMaybe val of
            Just n | n > 0 -> go rest opts { optMaxTurns = Just n } promptWords
            _              -> Left "--max-turns= requires a positive integer"

      | arg == "--max-budget-usd" =
          case rest of
            (val : rest') -> case readMaybe val of
              Just d | d >= 0 -> go rest' opts { optMaxBudgetUsd = Just d } promptWords
              _               -> Left "--max-budget-usd requires a positive number"
            [] -> Left "--max-budget-usd requires an argument"

      | Just val <- stripPrefix "--max-budget-usd=" arg =
          case readMaybe val of
            Just d | d >= 0 -> go rest opts { optMaxBudgetUsd = Just d } promptWords
            _               -> Left "--max-budget-usd= requires a positive number"

      | arg == "--append-system-prompt" =
          case rest of
            (val : rest') -> go rest' opts { optAppendSystemPrompt = Just (T.pack val) } promptWords
            []            -> Left "--append-system-prompt requires an argument"

      | Just val <- stripPrefix "--append-system-prompt=" arg =
          go rest opts { optAppendSystemPrompt = Just (T.pack val) } promptWords

      | arg == "--add-dir" =
          case rest of
            (val : rest') -> go rest' opts { optAddDir = optAddDir opts ++ [val] } promptWords
            []            -> Left "--add-dir requires an argument"

      | Just val <- stripPrefix "--add-dir=" arg =
          go rest opts { optAddDir = optAddDir opts ++ [val] } promptWords

      | arg `elem` ["--worktree", "-w"] =
          case rest of
            (val : rest')
              | not (null (dropWhile isSpace val)) ->
                  go rest' opts { optWorktree = Just (T.strip (T.pack val)) } promptWords
              | otherwise -> Left (arg ++ " requires a non-empty argument")
            [] -> Left (arg ++ " requires an argument")

      | Just val <- stripPrefix "--worktree=" arg =
          if null (dropWhile isSpace val)
            then Left "--worktree= requires a non-empty argument"
            else go rest opts { optWorktree = Just (T.strip (T.pack val)) } promptWords

      | Just val <- stripPrefix "-w=" arg =
          if null (dropWhile isSpace val)
            then Left "-w= requires a non-empty argument"
            else go rest opts { optWorktree = Just (T.strip (T.pack val)) } promptWords

      | arg == "--exec" =
          case rest of
            (val : rest') -> go rest' opts { optExec = Just (T.pack val), optNoTui = True } promptWords
            []            -> Left "--exec requires an argument"

      | Just val <- stripPrefix "--exec=" arg =
          go rest opts { optExec = Just (T.pack val), optNoTui = True } promptWords

      | arg == "--permission-mode" =
          case rest of
            (val : rest') -> case parsePermMode val of
              Just m  -> go rest' opts { optPermissionMode = Just m } promptWords
              Nothing -> Left ("Unknown permission mode: " ++ val)
            [] -> Left "--permission-mode requires an argument"

      | Just val <- stripPrefix "--permission-mode=" arg =
          case parsePermMode val of
            Just m  -> go rest opts { optPermissionMode = Just m } promptWords
            Nothing -> Left ("Unknown permission mode: " ++ val)

      | "-" `isPrefixOf` arg && arg /= "-" =
          Left ("Unknown flag: " ++ arg)

      | otherwise =
          go rest opts (promptWords ++ [arg])

-- | Extract the model specifically from line two of the lines of .env.
-- Follows the requirement: "always use the model on line two of the .env".
parseLineTwoModel :: [Text] -> Maybe Text
parseLineTwoModel rawLines =
  case drop 1 rawLines of
    (line2 : _) ->
      let trimmed = T.strip line2
      in if T.null trimmed || T.isPrefixOf "#" trimmed
           then Nothing
           else case T.breakOn "=" trimmed of
                  (_, val) | not (T.null val) -> Just (cleanVal (T.drop 1 val))
                  _                           -> Just (cleanVal trimmed)
    _ -> Nothing
  where
    cleanVal = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)

-- | Parse key-value pairs from .env content, respecting comments and quotes.
parseEnvContent :: Text -> Map Text Text
parseEnvContent content =
  let ls = T.lines content
      pairs = [ (T.strip k, clean (T.drop 1 v))
              | line <- ls
              , let trimmed = stripExport (T.strip line)
              , not (T.null trimmed)
              , not (T.isPrefixOf "#" trimmed)
              , let (k, v) = T.breakOn "=" trimmed
              , not (T.null v)
              ]
  in Map.fromList pairs
  where
    clean = T.dropAround (\c -> c == '"' || c == '\'' || isSpace c)
    stripExport s = fromMaybe s (T.stripPrefix "export " s)

-- | Configuration resolver with layered Settings.
resolveConfigWithSettings
  :: Maybe Text      -- ^ CLI model override (e.g. from --model flag)
  -> Maybe Text      -- ^ OS process environment OPENROUTER_API_KEY
  -> Maybe Text      -- ^ OS process environment OPENROUTER_MODEL
  -> Maybe Text      -- ^ .env file content
  -> Settings        -- ^ Layered settings
  -> Either String EnvConfig
resolveConfigWithSettings mCliModel mOsApiKey mOsModel mDotEnvContent settings =
  let mDotEnvMap   = fmap parseEnvContent mDotEnvContent
      mDotLines    = fmap T.lines mDotEnvContent
      mDotKey      = mDotEnvMap >>= Map.lookup "OPENROUTER_API_KEY"
      mDotLine2    = mDotLines >>= parseLineTwoModel
      mDotKeyModel = mDotEnvMap >>= Map.lookup "OPENROUTER_MODEL"

      -- API Key resolution: OS environment takes precedence over .env
      mResolvedApiKey =
        (mOsApiKey >>= nonBlank) `orFallback` (mDotKey >>= nonBlank)

      -- Model resolution: CLI flag > OS environment > line 2 of .env > OPENROUTER_MODEL in .env > settings
      mResolvedModel =
        (mCliModel >>= nonBlank)
          `orFallback` (mOsModel >>= nonBlank)
          `orFallback` (mDotLine2 >>= nonBlank)
          `orFallback` (mDotKeyModel >>= nonBlank)
          `orFallback` (setModel settings >>= nonBlank)
  in case (mResolvedApiKey, mResolvedModel) of
    (Just key, Just model) ->
      Right EnvConfig { envApiKey = key, envModel = model, envSettings = settings }
    (Nothing, _) ->
      Left "OPENROUTER_API_KEY is missing from both process environment and .env"
    (_, Nothing) ->
      Left "OpenRouter model not specified (use --model CLI flag, OPENROUTER_MODEL env var, line 2 of .env, or settings.json)"
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s
    orFallback (Just x) _ = Just x
    orFallback Nothing my = my

-- | Pure configuration resolver implementing precedence:
-- 1. API key: OS process environment -> .env file.
-- 2. Model: CLI flag -> OS process environment -> line 2 of .env -> OPENROUTER_MODEL in .env.
resolveConfigWith
  :: Maybe Text      -- ^ CLI model override (e.g. from --model flag)
  -> Maybe Text      -- ^ OS process environment OPENROUTER_API_KEY
  -> Maybe Text      -- ^ OS process environment OPENROUTER_MODEL
  -> Maybe Text      -- ^ .env file content
  -> Either String EnvConfig
resolveConfigWith mCliModel mOsApiKey mOsModel mDotEnvContent =
  resolveConfigWithSettings mCliModel mOsApiKey mOsModel mDotEnvContent defaultSettings

-- | Why startup configuration could not be resolved.
data EnvError
  = EnvSettingsInvalid SettingsError  -- ^ A settings file exists but could not be loaded.
  | EnvConfigUnresolved String        -- ^ API key or model could not be resolved.
  deriving (Show, Eq)

-- | Render a startup failure, with the hint that fits the cause.
renderEnvError :: EnvError -> String
renderEnvError (EnvSettingsInvalid err) =
  "Configuration error: " <> renderSettingsError err <> "\n" <>
  "Fix that file or move it aside; hach will not run with its settings ignored."
renderEnvError (EnvConfigUnresolved err) =
  "Configuration error: " <> err <> "\n" <>
  "Please set OPENROUTER_API_KEY in the environment or in .env."

-- | Resolve configuration from process environment, .env file, and layered settings.
resolveEnvConfig
  :: Maybe Text       -- ^ Optional CLI model override
  -> Maybe FilePath   -- ^ Optional path to .env file
  -> IO (Either EnvError EnvConfig)
resolveEnvConfig mCliModel mDotEnvPath = do
  mOsApiKey <- fmap (fmap T.pack) (lookupEnv "OPENROUTER_API_KEY")
  mOsModel  <- fmap (fmap T.pack) (lookupEnv "OPENROUTER_MODEL")
  mDotEnvContent <- case mDotEnvPath of
    Just path -> do
      exists <- doesFileExist path
      if exists then Just <$> TIO.readFile path else pure Nothing
    Nothing -> pure Nothing
  settingsRes <- loadLayeredSettings "."
  pure $ case settingsRes of
    Left err -> Left (EnvSettingsInvalid err)
    Right settings ->
      first EnvConfigUnresolved
        (resolveConfigWithSettings mCliModel mOsApiKey mOsModel mDotEnvContent settings)

-- | Legacy helper to load configuration specifically from a .env file.
loadEnvConfig :: FilePath -> IO (Either EnvError EnvConfig)
loadEnvConfig path = resolveEnvConfig Nothing (Just path)

-- | Load project instructions from AGENTS.md, AGENT.md, or CLAUDE.md in the workspace directory.
-- Precedence: AGENTS.md is preferred; then AGENT.md; then CLAUDE.md.
loadProjectInstructions :: FilePath -> IO (Maybe Text)
loadProjectInstructions = fmap (fmap snd) . loadProjectInstructionsFile

-- | Like 'loadProjectInstructions', also naming the file the instructions came from.
loadProjectInstructionsFile :: FilePath -> IO (Maybe (FilePath, Text))
loadProjectInstructionsFile workspace = firstExisting ["AGENTS.md", "AGENT.md", "CLAUDE.md"]
  where
    firstExisting [] = pure Nothing
    firstExisting (name : names) = do
      let fp = workspace </> name
      exists <- doesFileExist fp
      if exists then fmap ((,) name) <$> readFileUtf8 fp else firstExisting names

    readFileUtf8 fp = do
      res <- try (BS.readFile fp) :: IO (Either SomeException BS.ByteString)
      case res of
        Left _ -> pure Nothing
        Right bytes -> pure (Just (TE.decodeUtf8With (\_ _ -> Just ' ') bytes))

-- | Build the combined system prompt, appending project guidelines if present.
buildSystemPrompt :: Maybe Text -> Text
buildSystemPrompt mProjectGuidelines =
  let basePrompt =
        "You are an expert autonomous coding assistant. You have access to tools " <>
        "to inspect files, write code, run shell commands, and explore the workspace. " <>
        "Always inspect existing code before making changes, verify your work by running commands, " <>
        "and provide a concise final summary when complete."
  in case mProjectGuidelines of
    Just guidelines | not (T.null (T.strip guidelines)) ->
      basePrompt <> "\n\n# Project Guidelines:\n" <> guidelines
    _ -> basePrompt

-- | Build the combined system prompt, appending project guidelines and optional custom instructions.
buildSystemPromptWithAppend :: Maybe Text -> Maybe Text -> Text
buildSystemPromptWithAppend mProjectGuidelines mAppend =
  let basePrompt = buildSystemPrompt mProjectGuidelines
  in case mAppend of
    Just extra | not (T.null (T.strip extra)) ->
      basePrompt <> "\n\n" <> extra
    _ -> basePrompt
