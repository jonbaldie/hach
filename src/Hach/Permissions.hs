{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Permissions
  ( evalPermission
  , isProtectedPath
  , extractPathArg
  , matchGlob
  , matchStarGlob
  , matchRule
  , cyclePermissionMode
  ) where

import Hach.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (normalise, splitDirectories)

-- | Cycle through the available permission modes.
cyclePermissionMode :: PermissionMode -> PermissionMode
cyclePermissionMode = \case
  ModeDefault           -> ModeAcceptEdits
  ModeAcceptEdits       -> ModePlan
  ModePlan              -> ModeAuto
  ModeAuto              -> ModeDontAsk
  ModeDontAsk           -> ModeBypassPermissions
  ModeBypassPermissions -> ModeDefault

-- | Check if a path falls within a protected system directory (.git, .claude, .agents).
-- Standard repository files such as .gitignore, .gitattributes, .gitmodules, or .github workflows
-- are NOT inside .git and are therefore not protected.
isProtectedPath :: FilePath -> Bool
isProtectedPath fp =
  let dirs = splitDirectories (normalise fp)
      cleanDirs = [ d | d <- dirs, d /= "." && d /= "./" ]
      protected = [".git", ".claude", ".agents", ".agent"]
  in any (`elem` protected) cleanDirs

-- | Extract a target path argument from a JSON tool call argument object.
extractPathArg :: Aeson.Value -> Maybe FilePath
extractPathArg (Aeson.Object km) =
  let lookupKeys = ["path", "file", "target", "filePath"]
      findVal = listToMaybe [ T.unpack t | k <- lookupKeys, Just (Aeson.String t) <- [KM.lookup k km] ]
  in findVal
extractPathArg _ = Nothing

-- | Glob tokens. Lexing order matches the original backtracking matcher:
-- @**/@ first, then @**@, then @*@, then a literal character.
data GlobTok
  = Lit Char
  | Star        -- ^ '*'  : any run of non-'/' characters (including empty)
  | DStar       -- ^ '**' : any run of characters, including '/'
  | DStarSlash  -- ^ '**/': zero or more complete directory segments
  deriving (Eq, Show)

tokenizeGlob :: String -> [GlobTok]
tokenizeGlob []               = []
tokenizeGlob ('*':'*':'/':xs) = DStarSlash : tokenizeGlob xs
tokenizeGlob ('*':'*':xs)     = DStar : tokenizeGlob xs
tokenizeGlob ('*':xs)         = Star : tokenizeGlob xs
tokenizeGlob (c:xs)           = Lit c : tokenizeGlob xs

-- | Glob matcher supporting '*' and '**'.
--
-- @**/@ matches zero or more complete directory segments.
-- @**@ matches any characters, including @'/'@.
-- @*@ matches any characters within a directory segment (does not cross @'/'@).
--
-- Implemented as a bottom-up DP table over (target offset, token index) so
-- the work is O(|pattern| · |path|) instead of exponential backtracking.
matchGlob :: Text -> FilePath -> Bool
matchGlob pat fp = matchToks (tokenizeGlob (T.unpack pat)) fp

matchToks :: [GlobTok] -> String -> Bool
matchToks toks target =
  let n = length toks
      m = length target
      -- Cells with greater j (more of the target consumed) or greater i
      -- (more of the pattern consumed) are inserted first, so each
      -- lookup reads an already-computed dependency.
      keys = [ (j, i) | j <- [m, m-1 .. 0], i <- [n, n-1 .. 0] ]
      table = foldl' insertCell Map.empty keys
      insertCell acc (j, i) = Map.insert (j, i) (eval acc j i) acc
      eval acc j i
        | i == n = j == m
        | otherwise = case toks !! i of
            Lit c ->
              j < m && (target !! j) == c
                && Map.findWithDefault False (j + 1, i + 1) acc
            Star ->
              Map.findWithDefault False (j, i + 1) acc
                || (j < m && (target !! j) /= '/'
                      && Map.findWithDefault False (j + 1, i) acc)
            DStar ->
              Map.findWithDefault False (j, i + 1) acc
                || (j < m && Map.findWithDefault False (j + 1, i) acc)
            DStarSlash ->
              Map.findWithDefault False (j, i + 1) acc
                || case nextSlash j of
                     Just k  -> Map.findWithDefault False (k + 1, i) acc
                     Nothing -> False
      nextSlash j
        | j >= m = Nothing
        | target !! j == '/' = Just j
        | otherwise = nextSlash (j + 1)
  in Map.findWithDefault False (0, 0) table

-- | '*' matches any sequence of characters (including '/').
--
-- Linear in the combined length of pattern and string: only the most
-- recent '*' is remembered, which is sufficient when a star may consume
-- any character.
matchStarGlob :: Text -> Text -> Bool
matchStarGlob pat str = go 0 0 (-1) 0
  where
    plen = T.length pat
    slen = T.length str
    pAt i = T.index pat i
    sAt j = T.index str j

    go i j star match
      | j < slen && i < plen && pAt i == '*' =
          go (i + 1) j i j
      | j < slen && i < plen && pAt i == sAt j =
          go (i + 1) (j + 1) star match
      | j < slen && star >= 0 =
          go (star + 1) (match + 1) star (match + 1)
      | j == slen =
          eatStars i == plen
      | otherwise = False

    eatStars i
      | i < plen && pAt i == '*' = eatStars (i + 1)
      | otherwise = i

-- | Check if a single rule matches the given tool and path.
matchRule :: PermissionRule -> Text -> Maybe FilePath -> Maybe PermissionDecision
matchRule PermissionRule{..} tool mPath =
  let toolMatches = case prTool of
        Nothing -> True
        Just t  -> t == "*" || T.toLower t == T.toLower tool
      pathMatches = case (prPathGlob, mPath) of
        (Nothing, _)        -> True
        (Just glob, Just p) -> matchGlob glob p
        (Just _, Nothing)   -> False
  in if toolMatches && pathMatches
       then case prAction of
         RuleAllow -> Just PermAllow
         RuleAsk   -> Just (PermAsk ("Approval required by rule for " <> tool))
         RuleDeny  -> Just (PermDeny ("Denied by permission rule for " <> tool))
       else Nothing

-- | Evaluate permission for a tool invocation given the current mode, rules, tool name, and arguments.
evalPermission
  :: PermissionMode
  -> [PermissionRule]
  -> Text
  -> Aeson.Value
  -> PermissionDecision
evalPermission mode rules tool args
  | mode == ModeBypassPermissions = PermAllow
  | otherwise =
      let mPath = extractPathArg args
          normTool = T.toLower tool
          isWriteTool = normTool `elem` writeTools
      in if isWriteTool && maybe False isProtectedPath mPath
           then PermDeny ("Protected path: access denied to " <> maybe "" T.pack mPath)
           else case listToMaybe (concatMap (\r -> maybe [] pure (matchRule r tool mPath)) rules) of
             Just decision -> decision
             Nothing       -> evalModeDefault mode normTool isWriteTool
  where
    readOnlyTools =
      [ "read_file"
      , "list_dir"
      , "listdir"
      , "find_files"
      , "grep_search"
      , "glob"
      , "grep"
      , "webfetch"
      , "web_fetch"
      , "websearch"
      , "web_search"
      , "listagents"
      , "list_agents"
      , "tasklist"
      , "task_list"
      , "taskget"
      , "task_get"
      ]

    writeTools =
      [ "write_file"
      , "replace_file_content"
      , "edit"
      , "todowrite"
      , "todo_write"
      , "taskcreate"
      , "task_create"
      , "taskupdate"
      , "task_update"
      ]

    commandTools =
      [ "run_command"
      , "bash"
      , "taskstop"
      , "task_stop"
      , "enterworktree"
      , "enter_worktree"
      , "exitworktree"
      , "exit_worktree"
      ]

    evalModeDefault m t isWrite = case m of
      ModeBypassPermissions -> PermAllow
      ModeDontAsk           -> PermAllow
      ModePlan ->
        if t `elem` readOnlyTools
          then PermAllow
          else PermDeny "Plan mode is read-only. Tool execution denied."
      ModeAcceptEdits ->
        if t `elem` readOnlyTools || isWrite
          then PermAllow
          else if t `elem` commandTools
            then PermAsk ("Command execution requires approval: " <> tool)
            else PermAsk ("Tool execution requires approval: " <> tool)
      ModeAuto ->
        if t `elem` readOnlyTools || isWrite
          then PermAllow
          else PermAsk ("Auto mode requires approval for: " <> tool)
      ModeDefault ->
        if t `elem` readOnlyTools
          then PermAllow
          else PermAsk ("Tool execution requires approval: " <> tool)
