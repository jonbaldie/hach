{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command-line interface: option parsing, help text, startup intent and
-- @--print@ result presentation. Runtime environment resolution lives in
-- "Hach.Env".
module Hach.CLI
  ( CliOptions(..)
  , OutputFormat(..)
  , defaultCliOptions
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
  ) where

import Hach.Types (AgentResult(..), PermissionMode(..))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Printf (printf)
import Text.Read (readMaybe)

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

formatUsd :: Double -> Text
formatUsd d = T.pack (printf "$%.2f" d)

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
