# Exploratory testing pass: hach process encoding and headless goal validation

**Date:** 2026-09-30  
**Scope:** public `hach` CLI, tool execution, process encoding, and headless `/goal` validation  
**Build:** `fix/crypton-intel-macos` at `f80d403b9631d60d148c104afdff4a356859d6b2` (`hach 0.1.12.0`), GHC 9.12.1, cabal-install 3.16.1.0  
**Host:** macOS 25.6.0, APFS (case-insensitive), `GHCRTS=-N2` for all `hach` invocations  
**Model:** `meta/muse-glimmer-30b` / `openai/gpt-5.6-luna` with high reasoning effort (standing permission in AGENTS.md)  
**Evidence:** captured under `/tmp` and inlined below.  

All observations came directly from the public interface: the compiled `hach` binary on the command line (`--exec`, `--no-tui`, `--print`, `--output-format json`). Tests were run with `CLAUDE_CONFIG_DIR=/tmp/isolated-claude` to isolate from developer workstation configurations. Source code was read only to verify root causes for confirmed failures.

---

## Confirmed findings (2 issues filed)

| # | Issue | Title | Impact |
|---|---|---|---|
| 1 | [#240](https://github.com/jonbaldie/hach/issues/240) | `run_command` and `--exec` crash on non-UTF-8 process output | Process fails with `IOException`, agent cannot inspect output or exit code, and background `Monitor` stream freezes |
| 2 | [#241](https://github.com/jonbaldie/hach/issues/241) | Invalid headless `/goal` invocation exits 0 and breaks `--output-format json` | Invalid goal invocations look like success to CI scripts, and plain-text output breaks JSON parsers |

---

### 1. `run_command` and `--exec` crash on non-UTF-8 process output (#240)

**Starting conditions.** Fresh workspace or throwaway directory `/tmp/test-nonutf8-ws`. A command that outputs non-UTF-8 bytes (such as binary inspection utilities, localized legacy encodings, or Python/Perl scripts emitting arbitrary bytes).

**Replay 1 (CLI `--exec`).**
```bash
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --exec "python3 -c 'import sys; sys.stdout.buffer.write(b\"hello \xff\xfe world\n\")'"
```

**Replay 2 (Agent tool `run_command`).**
Under `hach --no-tui --permission-mode dontAsk`, request execution of `printf 'hello \xff\xfe world\n'`.

**Replay 3 (Background task `TaskCreate` / `Monitor`).**
Spawn a background task that emits `\xff\xfe` followed by subsequent output lines like `LINE-2`.

**Expected.**
The process output is captured and decoded leniently (substituting replacement characters `\xFFFD`), preserving exit code and readable text for the caller or agent, matching the behavior of `executeReadFile`, `executeWebFetch`, and `executeGrepSearch`. Background task stream readers continue reading subsequent lines.

**Actual.**
- `--exec` crashes with:
  ```
  Process execution failed: fd:20: hGetContents': invalid argument (cannot decode byte sequence starting from 255)
  exit=1
  ```
- `run_command` returns `ToolError`:
  ```
  [Tool run_command Error]
    Process execution failed: fd:23: hGetContents': invalid argument (cannot decode byte sequence starting from 255)
  ```
- `TaskCreate` background task reader (`readStream` in `src/Hach/Tasks.hs:160-165`) catches the decode exception in `try (TIO.hGetLine h)` and terminates silently with `Left _ -> pure ()`, permanently dropping all subsequent lines from the process.

**Code path.**
- `src/Hach/Tools.hs:1267-1268` (`runGroupWithTimeout`):
  Uses `hGetContents' hOut` and `hGetContents' hErr` with system locale text handles rather than raw `ByteString` reading followed by `TE.decodeUtf8With TE.lenientDecode`.
- `src/Hach/Tasks.hs:160-165` (`readStream`):
  Uses `TIO.hGetLine h` and terminates loop execution on decode failure.

---

### 2. Invalid headless `/goal` invocation exits 0 and breaks `--output-format json` (#241)

**Starting conditions.** Any workspace run in headless mode (`--no-tui` or `--print`).

**Replay.**
```bash
# Case A: Bare /goal without condition
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json "/goal"
echo "exit=$?"

# Case B: /goal clear in headless mode
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json "/goal clear"
echo "exit=$?"

# Case C: Condition exceeding max length (> 4000 characters)
CLAUDE_CONFIG_DIR=/tmp/isolated-claude GHCRTS=-N2 hach --print --output-format json "/goal $(python3 -c 'print("x"*4001)')"
echo "exit=$?"
```

**Expected.**
Non-zero exit status (`exitFailure`). When `--output-format json` is requested, errors must be formatted as valid JSON objects (e.g. `{"error": "..."}`) rather than unformatted plain text.

**Actual.**
- Case A outputs:
  ```
  Usage: /goal <condition> or /goal clear
  Example: /goal all tests pass
  exit=0
  ```
- Case B outputs:
  ```
  No active goal to clear (headless mode has no persistent goal state).
  exit=0
  ```
- Case C outputs:
  ```
  Goal condition too long (max 4000 characters).
  exit=0
  ```
All three cases emit unformatted plain text and exit 0, violating both the CLI exit status contract and the JSON output format contract.

**Code path.**
`app/Main.hs:167-181`: The validation branches call `putStrLn` without calling `exitFailure` or checking `optOutputFormat`. Execution drops out of the `if isGoalCommand` block, returning `()` to `main` and exiting cleanly with code 0.

---

## Verified working areas

1. **Existing test suite:**
   `cabal test hach:test:hach-test --test-show-details=always` passed all 937 examples cleanly with 0 failures.
2. **Standard CLI flags:**
   `--help`, `--version`, `--session-id` (missing session correctly exits 1), and empty prompt (correctly exits 1) all handle exit codes as specified.
3. **Workspace initialisation:**
   `--init` creates `CLAUDE.md` and exits 0; re-running reports already present and exits 0.
4. **Lenient file and web decoders:**
   `executeReadFile`, `executeWebFetch`, and `executeGrepSearch` all safely handle non-UTF-8 bytes using lenient decoding or replacement characters without raising unhandled exceptions.
5. **Session isolation:**
   Workspace sessions properly load and isolate settings when provided an explicit `CLAUDE_CONFIG_DIR`.

---

## Unexplored areas

1. Interactive TUI `/goal` mode transitions when resizing terminal window during active LLM evaluation.
2. Long-running multi-turn tool sessions under memory pressure or hitting max budget constraints with fractional cent usage.
3. Git worktree collision behaviors on APFS case-sensitive volumes vs default case-insensitive volumes.
