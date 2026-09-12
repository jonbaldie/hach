{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hach.Permissions
  ( evalPermission
  , evalPermissionForAuthority
  , isProtectedPath
  , extractPathArg
  , matchGlob
  , matchStarGlob
  , matchRule
  , cyclePermissionMode
  ) where

import Hach.Types
import Hach.Paths (isProtectedPath, matchStarGlob)
import Hach.Tools (resolveTool, resolvedToolAuthority, resolvedToolCanonicalName)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.List (tails)
import qualified Data.Map.Lazy as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Cycle through the available permission modes.
cyclePermissionMode :: PermissionMode -> PermissionMode
cyclePermissionMode = \case
  ModeDefault           -> ModeAcceptEdits
  ModeAcceptEdits       -> ModePlan
  ModePlan              -> ModeAuto
  ModeAuto              -> ModeDontAsk
  ModeDontAsk           -> ModeBypassPermissions
  ModeBypassPermissions -> ModeDefault

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
-- Implemented as a lazy DP over 'tails' of the token and path lists so
-- the work is O(|pattern| · |path|) instead of exponential backtracking.
matchGlob :: Text -> FilePath -> Bool
matchGlob pat fp = matchToks (tokenizeGlob (T.unpack pat)) fp

-- | Lazy DP over remaining-token / remaining-path suffixes.
-- Cells are addressed by the 'tails' offset, never by partial '!!'.
matchToks :: [GlobTok] -> String -> Bool
matchToks toks target = cell 0 0
  where
    tokSufs = tails toks
    tgtSufs = tails target

    table :: Map.Map (Int, Int) Bool
    table = Map.fromList
      [ ((i, j), eval i j ts tgt)
      | (i, ts)  <- zip [0..] tokSufs
      , (j, tgt) <- zip [0..] tgtSufs
      ]

    cell i j = Map.findWithDefault False (i, j) table

    eval i j ts tgt = case ts of
      [] ->
        null tgt
      Lit c : _ ->
        case tgt of
          t : _ | t == c -> cell (i + 1) (j + 1)
          _              -> False
      Star : _ ->
        cell (i + 1) j
          || case tgt of
               (c : _) | c /= '/' -> cell i (j + 1)
               _                  -> False
      DStar : _ ->
        cell (i + 1) j
          || case tgt of
               (_ : _) -> cell i (j + 1)
               []      -> False
      DStarSlash : _ ->
        cell (i + 1) j
          || case break (== '/') tgt of
               (pre, '/' : _) -> cell i (j + length pre + 1)
               _              -> False

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
  = case resolveTool (ToolCall "" tool (TE.decodeUtf8 (BSL.toStrict (Aeson.encode args)))) of
      Just (Right resolved) ->
        evalPermissionForAuthority mode rules (resolvedToolCanonicalName resolved) args (resolvedToolAuthority resolved)
      _ -> fallback
  where
    fallback = case mode of
      ModeBypassPermissions -> PermAllow
      ModeDontAsk -> PermAllow
      ModePlan -> PermDeny "Plan mode is read-only. Tool execution denied."
      ModeAuto -> PermAsk ("Auto mode requires approval for: " <> tool)
      _ -> PermAsk ("Tool execution requires approval: " <> tool)

-- | Evaluate a capability after the registry has resolved its authority.
-- Rules match the canonical tool name, so aliases cannot get a different
-- policy decision.
evalPermissionForAuthority
  :: PermissionMode
  -> [PermissionRule]
  -> Text
  -> Aeson.Value
  -> ToolAuthority
  -> PermissionDecision
evalPermissionForAuthority mode rules tool args authority
  | mode == ModeBypassPermissions = PermAllow
  | otherwise =
       let mPath = extractPathArg args
       in if authority == AuthorityWorkspaceWrite && maybe False isProtectedPath mPath
            then PermDeny ("Protected path: access denied to " <> maybe "" T.pack mPath)
            else case listToMaybe (concatMap (\r -> maybe [] pure (matchRule r tool mPath)) rules) of
              Just decision -> decision
              Nothing -> case mode of
                ModePlan | authority /= AuthorityRead -> PermDeny "Plan mode is read-only. Tool execution denied."
                ModePlan -> PermAllow
                ModeDefault | authority /= AuthorityRead -> PermAsk ("Tool execution requires approval: " <> tool)
                ModeDefault -> PermAllow
                ModeAcceptEdits | authority `elem` [AuthorityRead, AuthorityWorkspaceWrite] -> PermAllow
                ModeAcceptEdits -> PermAsk ("Tool execution requires approval: " <> tool)
                ModeAuto | authority `elem` [AuthorityRead, AuthorityWorkspaceWrite] -> PermAllow
                ModeAuto -> PermAsk ("Auto mode requires approval for: " <> tool)
                ModeDontAsk -> PermAllow
                ModeBypassPermissions -> PermAllow
