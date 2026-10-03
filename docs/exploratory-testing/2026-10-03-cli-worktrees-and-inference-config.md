# Exploratory testing pass: CLI JSON output contract, worktrees, and inference configuration

**Date:** 2026-10-03  
**Scope:** public `hach` CLI, `--print --output-format json` contract, inference provider and environment configuration, git worktree lifecycle, task tracking and background execution, and live inference  
**Build:** `main` at `3bb1a42` (`hach 0.1.13.0`), GHC 9.12.1, cabal-install 3.16.1.0  
**Host:** macOS 25.6.0, APFS (case-insensitive), `GHCRTS=-N2` for all `hach` invocations  
**Model:** `openai/gpt-5.6-luna` with high reasoning effort (standing permission in AGENTS.md)  
**Evidence:** captured in terminal output and inlined below.  

All observations came directly from the public interface: the compiled `hach` binary on the command line (`--print`, `--output-format json`, `--no-tui`, `--worktree`, `--exec`, `--provider`, `--base-url`, `--max-budget-usd`, `--max-turns`) and real live inference with `openai/gpt-5.6-luna`. Tests were run with `CLAUDE_CONFIG_DIR=/tmp/isolated-claude` to isolate from developer workstation configurations. Source code was read only to verify root causes for confirmed failures.

---

## Confirmed findings (2 issues filed)

| # | Issue | Title | Impact |
|---|---|---|---|
| 1 | [#265](https://github.com/jonbaldie/hach/issues/265) | Inference and environment configuration failure under `--print --output-format json` emits plain text and breaks JSON parsers | Headless runs with invalid inference provider, missing required API key, invalid base URL, or malformed settings print unformatted plain text to stdout, breaking JSON parsers and automated pipelines |
| 2 | [#266](https://github.com/jonbaldie/hach/issues/266) | Worktree creation failure under `--print --output-format json` emits plain text and breaks JSON parsers | Headless runs with invalid worktree names or git worktree failures print unformatted plain text to stdout, breaking JSON parsers and CI scripts |

---

### 1. Inference and environment configuration failure under `--print --output-format json` emits plain text and breaks JSON parsers (#265)

**Starting conditions.** Any workspace run in headless print mode with `--print --output-format json` (or `-p --output-format json`).

**Replay 1 (Unknown inference provider).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json --provider invalid-provider "hello"
echo "exit=$?"
```

**Replay 2 (Missing required API key).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude env -u OPENROUTER_API_KEY -u OPENAI_API_KEY GHCRTS=-N2 hach --print --output-format json "hello"
echo "exit=$?"
```

**Replay 3 (Invalid base URL).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json --provider openai-compatible -m test-model --base-url "http://localhost:8080/v1/chat/completions" "hello"
echo "exit=$?"
```

**Replay 4 (Malformed settings.json).**
```bash
mkdir -p /tmp/bad-settings/.claude && echo "{ invalid json }" > /tmp/bad-settings/.claude/settings.json
CLAUDE_CONFIG_DIR=/tmp/bad-settings GHCRTS=-N2 hach --print --output-format json "hello"
echo "exit=$?"
rm -rf /tmp/bad-settings
```

**Replay 5 (Unsupported effort level in settings.json).**
```bash
mkdir -p /tmp/bad-effort/.claude && echo '{"effort_level": "ultra"}' > /tmp/bad-effort/.claude/settings.json
CLAUDE_CONFIG_DIR=/tmp/bad-effort GHCRTS=-N2 hach --print --output-format json "hello"
echo "exit=$?"
rm -rf /tmp/bad-effort
```

**Piping to `jq`.**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json --provider invalid-provider "hello" | jq .
echo "pipe_exit=$?"
```

**Expected.**
When `--print --output-format json` is requested, error outcomes on stdout must be formatted as valid JSON objects (e.g. `{"error": "Configuration error: ..."}`) via `formatPrintResult optOutputFormat (AgentFailed err)`, consistent with the JSON contract established in #241, #258, and #259. Automated scripts checking non-zero exit codes and parsing JSON output must not receive JSON syntax errors.

**Actual.**
All replays emit unformatted plain text on stdout and exit 1:
- Replay 1 outputs:
  ```
  Configuration error: unknown provider "invalid-provider". Supported providers: openrouter, openai-compatible.
  Run hach --help for the inference configuration options.
  exit=1
  ```
- Replay 2 outputs:
  ```
  Configuration error: OPENROUTER_API_KEY is missing from both process environment and .env. Set it, or select another provider with --provider or HACH_PROVIDER.
  Run hach --help for the inference configuration options.
  exit=1
  ```
- Replay 3 outputs:
  ```
  Configuration error: Invalid base URL "http://localhost:8080/v1/chat/completions": give the API root (for example http://localhost:8080/v1), not the /chat/completions route; hach appends it.
  Run hach --help for the inference configuration options.
  exit=1
  ```
- Replay 4 outputs:
  ```
  Configuration error: Settings file /tmp/bad-settings/.claude/settings.json could not be loaded: Unexpected "invalid json }\n", expecting record key literal or }
  Fix that file or move it aside; hach will not run with its settings ignored.
  exit=1
  ```
- Replay 5 outputs:
  ```
  Configuration error: Unsupported effort_level: ultra. Supported values: max, xhigh, high, medium, low, minimal, none
  exit=1
  ```

Piping to `jq` crashes with:
```
jq: parse error: Invalid numeric literal at line 1, column 14
pipe_exit=5
```

**Code path.**
In `app/Main.hs:90-101`:
```haskell
  envRes <- resolveEnvConfig (InferenceFlags optProvider optBaseUrl optModel) (Just ".env")
  EnvConfig{..} <- case envRes of
    Left err -> do
      putStrLn (renderEnvError err)
      exitFailure
    Right cfg -> pure cfg

  effort <- case resolveEffortLevel envSettings of
    Left err -> do
      putStrLn ("Configuration error: " <> err)
      exitFailure
    Right e -> pure e
```
Both `resolveEnvConfig` and `resolveEffortLevel` call `putStrLn` to stdout directly without checking `optPrint` or formatting via `formatPrintResult optOutputFormat`.

---

### 2. Worktree creation failure under `--print --output-format json` emits plain text and breaks JSON parsers (#266)

**Starting conditions.** Any workspace run in headless print mode with `--print --output-format json` (or `-p --output-format json`) requesting worktree attachment or creation via `-w` / `--worktree`.

**Replay 1 (Invalid worktree name with directory traversal).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json -w "../escape" "hello"
echo "exit=$?"
```

**Replay 2 (Invalid worktree name with path separators).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json -w "feature/branch" "hello"
echo "exit=$?"
```

**Piping to `jq`.**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json -w "../escape" "hello" | jq .
echo "pipe_exit=$?"
```

**Expected.**
When `--print --output-format json` is specified, worktree resolution failures must be rendered on stdout as valid JSON parseable by `jq` (e.g. `{"error": "Worktree error: Invalid worktree name: ..."}`) via `formatPrintResult optOutputFormat (AgentFailed err)`.

**Actual.**
Both replays emit unformatted plain text on stdout and exit 1:
```
Worktree error: Invalid worktree name: must be alphanumeric and cannot contain path separators, leading dashes, or invalid git ref patterns.
exit=1
```

Piping to `jq` crashes with:
```
jq: parse error: Invalid numeric literal at line 1, column 10
pipe_exit=5
```

**Code path.**
In `app/Main.hs:65` and `app/Main.hs:251-260`:
```haskell
resolveStartupWorkspace :: FilePath -> Maybe T.Text -> IO FilePath
resolveStartupWorkspace cwd Nothing = pure cwd
resolveStartupWorkspace cwd (Just name) = do
  result <- Git.createWorktree cwd name
  case result of
    Left err -> do
      putStrLn ("Worktree error: " <> T.unpack err)
      exitFailure
    Right workspace -> pure workspace
```
`resolveStartupWorkspace` prints plain text directly to stdout via `putStrLn` and calls `exitFailure` before checking whether `optPrint` and `optOutputFormat` are configured.

---

## Verified working areas

1. **Full regression test suite:**
   `cabal test hach:test:hach-test --test-show-details=always` passed all 1077 examples cleanly with 0 failures.
2. **Recent JSON output contract fixes (Issues #258, #259, #241):**
   Verified that empty prompts (`< /dev/null`, `""`), missing session IDs (`--session-id non-existent`), and invalid `/goal` invocations (`/goal`, `/goal clear`) correctly emit valid JSON objects and exit with code 1 under `--print --output-format json`.
3. **Background task stdout/stderr and stdin decoupling (Issue #260):**
   Verified that background tasks spawned via `TaskCreate` run with `/dev/null` stdin (avoiding `SIGTTIN` suspension), streams are correctly read via `Monitor`, and output pipes are closed on task termination.
4. **Live model inference:**
   Successfully executed live end-to-end model calls using `openai/gpt-5.6-luna` with high reasoning effort under both plain-text mode and `--print --output-format json`.
5. **Git worktree creation, isolation, and exit:**
   Verified that `-w <name>` creates an isolated worktree under `.agents/worktrees/<name>`, executes commands inside that worktree, and that `ExitWorktree` correctly restores the root repository workspace.
6. **Append system prompt:**
   Verified that `--append-system-prompt` correctly appends instructions to the system prompt and influences model output during live runs.
7. **Budget ceiling enforcement:**
   Verified that `--max-budget-usd 0` under `--print --output-format json` emits `{"budget":0,"error":"max_budget","spent":0}` on stdout and exits 1, with notice logs directed to stderr.

---

## Unexplored areas

1. Reconnection and retry behavior when OpenRouter or OpenAI-compatible inference servers return HTTP 429 (rate limiting) with `Retry-After` headers.
2. Interaction between custom MCP server processes that restart mid-turn and Hach's tool execution state.
3. Behavior of `Hach.Memory` rule matching when nested directory symlinks create circular file paths.
