{-# LANGUAGE OverloadedStrings #-}

module Agent.Skills
  ( SkillSource(..)
  , Skill(..)
  , SkillCatalog
  , parseSkillFile
  , mergeSkills
  , discoverSkillsFromDir
  , discoverSkills
  , parseSkillInvocations
  , injectSkillsIntoPrompt
  , skillInvocationCompletion
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath ((</>))

-- | Source location of a discovered skill.
data SkillSource
  = SkillGlobal
  | SkillWorkspace
  deriving (Show, Eq)

-- | A parsed agent skill.
data Skill = Skill
  { skillName        :: !Text
  , skillDescription :: !Text
  , skillContent     :: !Text
  , skillPath        :: !FilePath
  , skillSource      :: !SkillSource
  } deriving (Show, Eq)

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
          in case mName of
            Nothing -> Left "Missing 'name' field in skill frontmatter."
            Just nm -> Right Skill
              { skillName        = nm
              , skillDescription = fromMaybe "" mDesc
              , skillContent     = T.strip (T.unlines bodyLines)
              , skillPath        = path
              , skillSource      = source
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

-- | Discover skills from a given root directory (e.g. ~/.agents/skills).
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

-- | Discover all skills across global (~/.agents/skills) and workspace (.agents/skills).
discoverSkills :: FilePath -> IO SkillCatalog
discoverSkills workspace = do
  homeRes <- try getHomeDirectory :: IO (Either SomeException FilePath)
  globalSkills <- case homeRes of
    Left _     -> pure []
    Right home -> discoverSkillsFromDir SkillGlobal (home </> ".agents" </> "skills")
  let workspaceDir = workspace </> ".agents" </> "skills"
  workspaceSkills <- discoverSkillsFromDir SkillWorkspace workspaceDir
  pure (mergeSkills globalSkills workspaceSkills)

-- | Inspect a user message for skill invocation tokens (e.g. '/to-spec').
-- Extracts matching skills and removes the invocation token while preserving multiline formatting.
parseSkillInvocations :: SkillCatalog -> Text -> (Text, [Skill])
parseSkillInvocations catalog rawInput =
  let allWords = T.words rawInput
      potentialCmds = [ (w, skill)
                      | w <- allWords
                      , "/" `T.isPrefixOf` w
                      , Just skill <- [Map.lookup (T.drop 1 w) catalog]
                      ]
  in case potentialCmds of
       [] -> (rawInput, [])
       cmds ->
         let matchedSkills = map snd cmds
             cleanOne acc (cmdTok, _) =
               let withTrailingSpace = cmdTok <> " "
                   withLeadingSpace  = " " <> cmdTok
               in if withTrailingSpace `T.isInfixOf` acc
                    then T.replace withTrailingSpace "" acc
                    else if withLeadingSpace `T.isInfixOf` acc
                      then T.replace withLeadingSpace "" acc
                      else T.replace cmdTok "" acc
             cleaned = T.strip (foldl cleanOne rawInput cmds)
         in (cleaned, matchedSkills)

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

-- | Compute the inline completion suffix for the slash-command currently
-- being typed at the end of the input buffer.
--
-- When the trailing whitespace-delimited word of the buffer is a @/@
-- followed by a non-empty string that is a proper prefix of at least one
-- skill name in the catalog, returns the characters needed to complete
-- that word to the lexicographically smallest skill name that is strictly
-- longer than the typed prefix. An exact match never blocks extending to a
-- longer skill (so @"/go"@ can still complete to @"/goal"@ even when a
-- @go@ skill exists). Returns 'Nothing' when the trailing word is not a
-- @/@-command in progress, when no skill extends it, or when the buffer
-- ends in whitespace (the word is finished).
skillInvocationCompletion :: SkillCatalog -> Text -> Maybe Text
skillInvocationCompletion catalog input =
  case trailingWord input of
    Nothing -> Nothing
    Just word
      | not ("/" `T.isPrefixOf` word) -> Nothing
      | T.null partial                -> Nothing
      | null longer                   -> Nothing
      | otherwise                     -> Just (T.drop (T.length partial) (minimum longer))
      where
        partial = T.drop 1 word
        longer  = [ m | m <- Map.keys catalog
                     , partial `T.isPrefixOf` m
                     , T.length m > T.length partial
                     ]

-- | The whitespace-delimited word at the end of the buffer, or 'Nothing'
-- when the buffer is empty or ends in whitespace (no word in progress).
trailingWord :: Text -> Maybe Text
trailingWord t
  | T.null t           = Nothing
  | isSpace (T.last t) = Nothing
  | otherwise          = case T.words t of
      [] -> Nothing
      ws -> Just (last ws)
