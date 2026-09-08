{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Skills
  ( SkillSource(..)
  , Skill(..)
  , mkSkill
  , SkillCatalog
  , parseSkillFile
  , mergeSkills
  , discoverSkillsFromDir
  , discoverSkills
  , parseSkillInvocations
  , injectSkillsIntoPrompt
  , expandSlashInvokedPrompt
  , skillInvocationCompletion
  , inputSlashCompletion
  , substituteArguments
  , injectDynamicContext
  ) where

import Hach.Paths (resolveWorkspacePath)
import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, nubBy, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (CreateProcess(cwd), readCreateProcessWithExitCode, shell)

-- | Source location of a discovered skill.
data SkillSource
  = SkillGlobal
  | SkillWorkspace
  deriving (Show, Eq)

-- | A parsed agent skill with frontmatter metadata.
data Skill = Skill
  { skillName                   :: !Text
  , skillDescription            :: !Text
  , skillContent                :: !Text
  , skillPath                   :: !FilePath
  , skillSource                 :: !SkillSource
  , skillAllowedTools           :: ![Text]
  , skillUserInvocable          :: !Bool
  , skillDisableModelInvocation :: !Bool
  , skillContextFork            :: !Bool
  , skillAgent                  :: !(Maybe Text)
  , skillPaths                  :: ![Text]
  } deriving (Show, Eq)

-- | Smart constructor with default metadata for backward compatibility.
mkSkill :: Text -> Text -> Text -> FilePath -> SkillSource -> Skill
mkSkill name desc content path source = Skill
  { skillName                   = name
  , skillDescription            = desc
  , skillContent                = content
  , skillPath                   = path
  , skillSource                 = source
  , skillAllowedTools           = []
  , skillUserInvocable          = True
  , skillDisableModelInvocation = False
  , skillContextFork            = False
  , skillAgent                  = Nothing
  , skillPaths                  = []
  }

-- | Catalog of available skills indexed by skill name.
type SkillCatalog = Map Text Skill

-- | Strip optional matching single or double quotes around a text value.
stripQuotes :: Text -> Text
stripQuotes t = fromMaybe t $
  (T.stripPrefix "\"" t >>= T.stripSuffix "\"") <|>
  (T.stripPrefix "'"  t >>= T.stripSuffix "'")

-- | Parse the frontmatter and body of a SKILL.md file.
parseSkillFile :: SkillSource -> FilePath -> Text -> Either String Skill
parseSkillFile source path rawText =
  let allLines = T.lines rawText
  in case allLines of
    (firstLine : rest) | T.strip firstLine == "---" ->
      case break (\l -> T.strip l == "---") rest of
        (fmLines, _delim : bodyLines) ->
          let parsedFields = [ parseKeyValue l | l <- fmLines, not (T.null (T.strip l)) ]
              mName = lookup "name" parsedFields
              mDesc = lookup "description" parsedFields
              mAllowed = lookup "allowed-tools" parsedFields
              mUserInvocable = lookup "user-invocable" parsedFields
              mDisableModel = lookup "disable-model-invocation" parsedFields
              mContext = lookup "context" parsedFields <|> lookup "context:fork" parsedFields
              mAgent = lookup "agent" parsedFields
              mPaths = lookup "paths" parsedFields

              parseBool _ (Just v) =
                let s = map toLower (T.unpack (T.strip v))
                in s `elem` ["true", "yes", "1"]
              parseBool def Nothing = def

              parseList (Just v) = [ T.strip (stripQuotes w) | w <- T.splitOn "," v, not (T.null (T.strip w)) ]
              parseList Nothing  = []
          in case mName of
            Nothing -> Left "Missing 'name' field in skill frontmatter."
            Just nm -> Right Skill
              { skillName                   = nm
              , skillDescription            = fromMaybe "" mDesc
              , skillContent                = T.strip (T.unlines bodyLines)
              , skillPath                   = path
              , skillSource                 = source
              , skillAllowedTools           = parseList mAllowed
              , skillUserInvocable          = parseBool True mUserInvocable
              , skillDisableModelInvocation = parseBool False mDisableModel
              , skillContextFork            = maybe False (\v -> T.toLower (T.strip v) `elem` ["fork", "true"]) mContext
              , skillAgent                  = mAgent
              , skillPaths                  = parseList mPaths
              }
        _ -> Left "Unterminated frontmatter delimiter (missing closing '---')."
    _ -> Left "Skill file must begin with frontmatter delimiter '---'."
  where
    parseKeyValue line =
      let (k, v) = T.breakOn ":" line
          val = if T.null v then "" else T.strip (T.drop 1 v)
      in (T.strip k, stripQuotes val)

-- | Merge global skills and workspace skills into a catalog.
-- Workspace skills take precedence over global skills with identical names.
mergeSkills :: [Skill] -> [Skill] -> SkillCatalog
mergeSkills globalSkills workspaceSkills =
  let globalMap = Map.fromList [ (skillName s, s) | s <- globalSkills ]
      workspaceMap = Map.fromList [ (skillName s, s) | s <- workspaceSkills ]
  in Map.union workspaceMap globalMap

-- | Discover skills from a given directory.
discoverSkillsFromDir :: SkillSource -> FilePath -> IO [Skill]
discoverSkillsFromDir source dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      subdirsRes <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
      case subdirsRes of
        Left _ -> pure []
        Right subdirs -> do
          skills <- mapM checkSubdir subdirs
          pure (catMaybes skills)
  where
    checkSubdir sub = do
      let skillFile = dir </> sub </> "SKILL.md"
      fileOk <- doesFileExist skillFile
      if not fileOk
        then pure Nothing
        else do
          contentRes <- try (BS.readFile skillFile) :: IO (Either SomeException BS.ByteString)
          case contentRes of
            Left _ -> pure Nothing
            Right bytes ->
              let txt = TE.decodeUtf8With (\_ _ -> Just ' ') bytes
              in case parseSkillFile source skillFile txt of
                Left _  -> pure Nothing
                Right s -> pure (Just s)

-- | Discover all skills across global and workspace, supporting .claude/skills and .agents/skills.
discoverSkills :: FilePath -> IO SkillCatalog
discoverSkills workspace = do
  homeRes <- try getHomeDirectory :: IO (Either SomeException FilePath)
  globalSkills <- case homeRes of
    Left _ -> pure []
    Right home -> do
      gClaude <- discoverSkillsFromDir SkillGlobal (home </> ".claude" </> "skills")
      gAgents <- discoverSkillsFromDir SkillGlobal (home </> ".agents" </> "skills")
      pure (gClaude ++ gAgents)
  wClaude <- discoverSkillsFromDir SkillWorkspace (workspace </> ".claude" </> "skills")
  wAgents <- discoverSkillsFromDir SkillWorkspace (workspace </> ".agents" </> "skills")
  pure (mergeSkills globalSkills (wClaude ++ wAgents))

-- | Substitute $ARGUMENTS in skill content.
substituteArguments :: Text -> Text -> Text
substituteArguments args content = T.replace "$ARGUMENTS" args content

-- | Inject dynamic context into skill content: !command lines and {{file:path}} placeholders.
injectDynamicContext :: FilePath -> Text -> IO Text
injectDynamicContext root raw = do
  let ls = T.lines raw
  expandedLines <- mapM processLine ls
  pure (T.unlines expandedLines)
  where
    processLine line
      | "!" `T.isPrefixOf` T.stripStart line = do
          let cmd = T.unpack (T.drop 1 (T.stripStart line))
              procSpec = (shell cmd) { cwd = Just root }
          res <- try (readCreateProcessWithExitCode procSpec "") :: IO (Either SomeException (ExitCode, String, String))
          pure $ case res of
            Right (ExitSuccess, out, _) -> T.stripEnd (T.pack out)
            _                           -> line
      | "{{file:" `T.isInfixOf` line = replaceFilePlaceholders line
      | otherwise = pure line

    replaceFilePlaceholders line =
      case T.breakOn "{{file:" line of
        (before, rest) | not (T.null rest) -> do
          let afterPrefix = T.drop 7 rest
          case T.breakOn "}}" afterPrefix of
            (fpText, restAfter) | not (T.null restAfter) -> do
              let trailing = T.drop 2 restAfter
              pathRes <- resolveWorkspacePath root (T.unpack (T.strip fpText))
              fileContent <- case pathRes of
                Left _ -> pure ("{{file:" <> fpText <> "}}")
                Right targetFp -> do
                  exists <- doesFileExist targetFp
                  if exists
                    then do
                      bRes <- try (BS.readFile targetFp) :: IO (Either SomeException BS.ByteString)
                      case bRes of
                        Right bs -> pure (TE.decodeUtf8With (\_ _ -> Just ' ') bs)
                        Left _   -> pure ("{{file:" <> fpText <> "}}")
                    else pure ("{{file:" <> fpText <> "}}")
              nextTrailing <- replaceFilePlaceholders trailing
              pure (before <> fileContent <> nextTrailing)
            _ -> pure line
        _ -> pure line

-- | Inspect a user message for skill invocation tokens (e.g. '/to-spec').
parseSkillInvocations :: SkillCatalog -> Text -> (Text, [Skill])
parseSkillInvocations catalog rawInput =
  let allWords = T.words rawInput
      potentialCmds = [ (w, skill)
                      | w <- allWords
                      , "/" `T.isPrefixOf` w
                      , Just skill <- [Map.lookup (T.drop 1 w) catalog]
                      , skillUserInvocable skill
                      ]
  in case potentialCmds of
       [] -> (rawInput, [])
       cmds ->
         let uniqueSkills = nubBy (\s1 s2 -> skillName s1 == skillName s2) (map snd cmds)
             invokedToks  = map fst cmds
             -- Filter exact whitespace-delimited tokens per line so a skill
             -- token that is only a prefix of another word (e.g. /foo vs
             -- /foo-bar) is left intact, and newlines / indentation survive.
             cleanLine line
               | not (any (`elem` invokedToks) (T.words line)) = line
               | otherwise =
                   let (leadingWs, rest) = T.span (\c -> c == ' ' || c == '\t') line
                       toks = T.words rest
                       filtered = filter (`notElem` invokedToks) toks
                   in if null filtered
                        then ""
                        else leadingWs <> T.unwords filtered
             rawLines = map cleanLine (T.splitOn "\n" rawInput)
             trimmedLines = dropWhileEnd (T.all isSpace) (dropWhile (T.all isSpace) rawLines)
             cleaned = T.intercalate "\n" trimmedLines
         in (cleaned, uniqueSkills)

-- | Inject skill instructions into the user prompt.
injectSkillsIntoPrompt :: [Skill] -> Text -> Text
injectSkillsIntoPrompt [] prompt = prompt
injectSkillsIntoPrompt skills prompt =
  let formattedSkills = T.unlines
        [ "<skill name=\"" <> skillName s <> "\">\n" <> skillContent s <> "\n</skill>"
        | s <- skills
        ]
  in if T.null (T.strip prompt)
       then T.strip formattedSkills
       else T.strip formattedSkills <> "\n\n" <> prompt

-- | Slash-invocation counterpart to 'executeSkill': bind leftover prompt text
-- to $ARGUMENTS and expand !command / {{file:}} before splicing skills in.
expandSlashInvokedPrompt :: FilePath -> SkillCatalog -> Text -> IO Text
expandSlashInvokedPrompt root catalog rawInput = do
  let (cleaned, invoked) = parseSkillInvocations catalog rawInput
  expandedSkills <- mapM (expandOne cleaned) invoked
  pure (injectSkillsIntoPrompt expandedSkills cleaned)
  where
    expandOne args sk = do
      let substituted = substituteArguments args (skillContent sk)
      expanded <- injectDynamicContext root substituted
      pure sk { skillContent = T.stripEnd expanded }

-- | Compute the inline completion suffix for the slash-command currently being typed.
-- Only user-invocable skills participate.
skillInvocationCompletion :: SkillCatalog -> Text -> Maybe Text
skillInvocationCompletion catalog =
  slashCommandCompletion (skillSlashNames catalog)

-- | Completion suffix against an explicit list of full slash tokens
-- (e.g. @"/clear"@, @"/goal"@).  The typed token must already start with
-- @\'/'@ and have at least one character after it; the result is the
-- remainder of the lexicographically smallest strictly-longer candidate.
slashCommandCompletion :: [Text] -> Text -> Maybe Text
slashCommandCompletion candidates input = do
  word <- trailingWord input
  guard ("/" `T.isPrefixOf` word && T.length word > 1)
  best <- listToMaybe (sort
    [ c | c <- candidates
        , word `T.isPrefixOf` c
        , T.length c > T.length word
        ])
  pure (T.drop (T.length word) best)

-- | Union of built-in slash commands and user-invocable skill names.
inputSlashCompletion :: SkillCatalog -> [Text] -> Text -> Maybe Text
inputSlashCompletion catalog builtins =
  slashCommandCompletion (builtins ++ skillSlashNames catalog)

skillSlashNames :: SkillCatalog -> [Text]
skillSlashNames catalog =
  [ "/" <> name
  | (name, skill) <- Map.toList catalog
  , skillUserInvocable skill
  ]

trailingWord :: Text -> Maybe Text
trailingWord t
  | T.null t           = Nothing
  | isSpace (T.last t) = Nothing
  | otherwise          = case T.words t of
      [] -> Nothing
      ws -> Just (last ws)
