{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Permissions
  ( evalPermission
  , isProtectedPath
  , extractPathArg
  , matchGlob
  , matchRule
  , cyclePermissionMode
  ) where

import Hach.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
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

-- | Simple glob matcher supporting '*' wildcard and '**' recursive wildcards.
-- '**/ matches zero or more directories.
-- '*' matches any characters within a directory segment (does not cross '/').
matchGlob :: Text -> FilePath -> Bool
matchGlob pat fp =
  let patStr = T.unpack pat
      fpStr  = fp
  in matchGlobStr patStr fpStr
  where
    matchGlobStr [] [] = True
    matchGlobStr ('*':'*':'/':rest) target =
      matchGlobStr rest target || case target of
        []     -> False
        (_:ts) -> matchGlobStr ('*':'*':'/':rest) ts
    matchGlobStr ['*', '*'] _ = True
    matchGlobStr ('*':'*':rest) target =
      matchGlobStr rest target || case target of
        []     -> False
        (_:ts) -> matchGlobStr ('*':'*':rest) ts
    matchGlobStr ('*':rest) target =
      matchGlobStr rest target || case target of
        (c:ts) | c /= '/' -> matchGlobStr ('*':rest) ts
        _                 -> False
    matchGlobStr (p:ps) (t:ts)
      | p == t    = matchGlobStr ps ts
      | otherwise = False
    matchGlobStr [] (_:_) = False
    matchGlobStr (_:_) [] = False

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
      , "websearch"
      , "listagents"
      , "tasklist"
      , "taskget"
      ]

    writeTools =
      [ "write_file"
      , "replace_file_content"
      , "edit"
      , "todowrite"
      , "taskcreate"
      , "taskupdate"
      ]

    commandTools =
      [ "run_command"
      , "bash"
      , "taskstop"
      , "enterworktree"
      , "exitworktree"
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
