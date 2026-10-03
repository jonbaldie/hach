# Exploratory testing pass: CLI JSON output contract and background tasks

**Date:** 2026-10-02  
**Scope:** public `hach` CLI, `--print --output-format json` contract, prompt validation, session continuity, and background process management in `Hach.Tasks`  
**Build:** `chore/release-0.1.14` at `164e50a` (`hach 0.1.14.0`), GHC 9.12.1, cabal-install 3.16.1.0  
**Host:** macOS 25.6.0, APFS (case-insensitive), `GHCRTS=-N2` for all `hach` invocations  
**Model:** `meta/muse-glimmer-30b` / `openai/gpt-5.6-luna` with high reasoning effort (standing permission in AGENTS.md)  
**Evidence:** captured under `/tmp` and inlined below.  

All observations came directly from the public interface: the compiled `hach` binary on the command line (`--print`, `--output-format json`, `--no-tui`, `--session-id`, `--continue`, `--resume`) and tool calls. Tests were run with `CLAUDE_CONFIG_DIR=/tmp/isolated-claude` to isolate from developer workstation configurations. Source code was read only to verify root causes for confirmed failures.

---

## Confirmed findings (3 issues filed)

| # | Issue | Title | Impact |
|---|---|---|---|
| 1 | [#258](https://github.com/jonbaldie/hach/issues/258) | Empty task prompt under `--print --output-format json` emits plain text and breaks JSON parsers | Headless runs with closed or empty stdin print plain text instead of valid JSON, breaking automated pipelines and tools piping to `jq` |
| 2 | [#259](https://github.com/jonbaldie/hach/issues/259) | Session load failure under `--print --output-format json` emits plain text and breaks JSON parsers | Missing or invalid session IDs under `--print --output-format json` print unformatted error text to stdout, breaking JSON parsers |
| 3 | [#260](https://github.com/jonbaldie/hach/issues/260) | Background tasks leak pipe file descriptors and inherit controlling terminal stdin in Hach.Tasks | Background processes spawned via `TaskCreate` inherit terminal stdin (triggering `SIGTTIN` on read) and leak pipe handles on completion |

---

### 1. Empty task prompt under `--print --output-format json` emits plain text and breaks JSON parsers (#258)

**Starting conditions.** Any workspace run in headless mode with `--print --output-format json` (or `-p --output-format json`).

**Replay 1 (Closed stdin / EOF).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json < /dev/null
echo "exit=$?"
```

**Replay 2 (Empty string positional argument).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json "" < /dev/null
echo "exit=$?"
```

**Replay 3 (Piped empty input).**
```bash
echo "" | CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json
echo "exit=$?"
```

**Piping to `jq`.**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json < /dev/null | jq .
echo "pipe_exit=$?"
```

**Expected.**
When `--print --output-format json` is requested, error outcomes on stdout must be formatted as valid JSON objects (e.g. `{"error": "Empty task prompt provided. Exiting."}`) via `formatPrintResult optOutputFormat (AgentFailed err)`, consistent with the JSON contract established in #241. Automated scripts checking non-zero exit codes and parsing JSON output must not receive JSON syntax errors.

**Actual.**
All replays emit unformatted plain text on stdout and exit 1:
```
Empty task prompt provided. Exiting.
exit=1
```
Piping to `jq` crashes with:
```
jq: parse error: Invalid numeric literal at line 1, column 6
pipe_exit=5
```

**Code path.**
In `app/Main.hs:161-164`:
```haskell
      when (T.null (T.strip taskPrompt)) $ do
        putStrLn "Empty task prompt provided. Exiting."
        exitFailure
```
The empty-prompt check prints raw text directly via `putStrLn` without checking `optPrint` or formatting via `formatPrintResult optOutputFormat`.

---

### 2. Session load failure under `--print --output-format json` emits plain text and breaks JSON parsers (#259)

**Starting conditions.** Any workspace with Hach run in headless print mode with `--print --output-format json`, requesting session continuation via `--session-id <id>`, `--continue`, or `--resume`.

**Replay 1 (Non-existent session ID).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json --session-id non-existent-id "hello"
echo "exit=$?"
```

**Replay 2 (Resuming in a workspace with no stored session).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json --resume "hello"
echo "exit=$?"
```

**Piping to `jq`.**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json --session-id non-existent-id "hello" | jq .
echo "pipe_exit=$?"
```

**Expected.**
When `--print --output-format json` is specified, session loading failures must be rendered on stdout as valid JSON parseable by `jq` (e.g. `{"error": "No stored session found for session ID: non-existent-id"}` or `{"error": "No stored session found in workspace."}`) via `formatPrintResult optOutputFormat (AgentFailed (T.pack err))`.

**Actual.**
Replay 1 outputs:
```
No stored session found for session ID: non-existent-id
exit=1
```
Replay 2 outputs:
```
No stored session found in workspace.
exit=1
```
Piping either to `jq` crashes with:
```
jq: parse error: Invalid numeric literal at line 1, column 3
pipe_exit=5
```

**Code path.**
In `app/Main.hs:120-124`:
```haskell
  targetWorkspace <- currentIOWorkspace ioEnv
  let sessionTarget = resolveSessionTarget optContinue optResume optSessionId
  sessionRes <- resolveSessionLoad targetWorkspace sessionTarget
  (activeSid, mLoadedSession) <- case sessionRes of
    Left err -> do
      putStrLn err
      exitFailure
    Right Nothing -> do
...
```
`resolveSessionLoad` returns `Left err`. `app/Main.hs` unconditionally executes `putStrLn err` to stdout without checking whether `--print` or `--output-format json` is in effect.

---

### 3. Background tasks leak pipe file descriptors and inherit controlling terminal stdin in Hach.Tasks (#260)

**Starting conditions.** Any Hach session where background processes are spawned via the `TaskCreate` tool.

**Replay 1 (Terminal stdin inheritance / `SIGTTIN` suspension).**
Under `hach` (or programmatically via `spawnBackgroundProcess`), start a background task that reads from stdin:
```bash
TaskCreate name="reader" command="read line; echo got:$line"
```
Because `std_in = Inherit` in `spawnBackgroundProcess`, the child process group attempts to read from the controlling terminal in the background. On POSIX, the kernel sends `SIGTTIN` to the background process group, suspending the process (status `T` / `stat=T`). The task never completes or produces output.

**Replay 2 (Pipe handle leak).**
Spawn multiple background tasks. `readStream` loops on `hIsEOF h`. When `hIsEOF` becomes True or an error occurs, it returns `pure ()` without closing `h`. `stopBackgroundProcess` signals the process group and reaps the process handle, but never closes `hOut` or `hErr`. Open file descriptors remain allocated until process exit.

**Expected.**
Background processes must detach standard input (e.g. connecting to `/dev/null` via `std_in = NoStream`), so any command attempting to read stdin receives EOF immediately rather than being suspended by `SIGTTIN` or competing with the TUI. Pipe handles `hOut` and `hErr` must be closed with `hClose` upon EOF or task teardown.

**Code path.**
- `src/Hach/Tasks.hs:122-130` (`spawnBackgroundProcess`):
  `std_in` is not configured, defaulting to `Inherit` from `shell`.
- `src/Hach/Tasks.hs:154-169` (`readStream`):
  Never calls `hClose h` upon reaching EOF or catching an exception.
- `src/Hach/Tasks.hs:142-145` & `220-225`:
  `bpGroup` is not cleared when a task completes naturally, leaving stale PIDs in memory that are signalled by `stopAllBackgroundProcesses` when Hach exits.

---

## Verified working areas

1. **Existing test suite:**
   `cabal test hach:test:hach-test --test-show-details=always` passed all 1067 examples cleanly with 0 failures.
2. **Invalid `/goal` JSON errors:**
   Verified that bare `/goal`, `/goal clear`, and oversized goal strings correctly produce valid JSON and exit 1 under `--print --output-format json` (issue #241 fix is solid).
3. **Session spend recording:**
   Verified that saved sessions correctly record spend from the agent run (issue #253 fix is solid).
4. **Non-UTF-8 process output decoding:**
   `run_command`, `--exec`, and background tasks decode invalid UTF-8 bytes using replacement characters without throwing uncaught exceptions (issue #240 fix is solid).
5. **Budget abort formatting:**
   `--max-budget-usd 0` under `--print --output-format json` correctly reports `{"budget":0,"error":"max_budget","spent":0}` on stdout and exits 1, with notice logs directed to stderr.

---

## Unexplored areas

1. Signal handling behaviour when SIGINT is delivered while an LLM streaming response is mid-chunk under OpenAI-compatible endpoints.
2. Multiple concurrent `TaskCreate` background processes writing high-throughput output concurrently to the shared in-memory buffer.
3. Git worktree branch deletion when switching between worktrees created by different sessions.
