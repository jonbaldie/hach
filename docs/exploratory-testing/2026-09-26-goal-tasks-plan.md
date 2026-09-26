# Exploratory testing pass: hach goal loop, tasks and plan mode

**Date:** 2026-09-26
**Scope:** public `hach` CLI and TUI, covering the goal loop, task tracking, and plan mode
**Build:** `main` at `1ba53f4c87d005f94d7d908134936ec3a4e0f55b` (`hach 0.1.10.0`), GHC 9.12.1, cabal-install 3.16.1.0
**Host:** macOS 25.6.0, APFS (case-insensitive), `GHCRTS=-N2` for all `hach` invocations
**Model:** `openai/gpt-5.6-luna` with `effort_level: high` (standing permission in AGENTS.md)
**Evidence:** captured under `/tmp` during the pass and removed with the scratch workspaces. Observed values are inlined below. Evidence filenames are provenance labels.

All observations came from the public interface: the `hach` binary on the command line, and the TUI driven through a real pty (`pty.fork()` at 120x40, screens rendered with `pyte`). Source was read only to explain confirmed failures. Each workspace was a throwaway git repository with its own `.env` and `.claude/settings.json`. `CLAUDE_CONFIG_DIR` pointed at a throwaway home.

This pass covers three areas the [2026-09-17](2026-09-17-cli-tui-permissions.md) and [2026-09-19](2026-09-19-sessions-hooks-skills.md) passes left unexplored or only sketched: the multi-turn `/goal` loop, task tracking, and plan mode.

---

## Confirmed findings (5 issues filed)

| # | Issue | Title | Impact |
|---|---|---|---|
| 1 | [#223](https://github.com/jonbaldie/hach/issues/223) | Unmet `/goal` run exits 0 and prints `Task completed` | A failed goal looks successful to a script |
| 2 | [#219](https://github.com/jonbaldie/hach/issues/219) | `ExitPlanMode` drops `acceptEdits`, so the following write is denied | An authorized edit never lands |
| 3 | [#220](https://github.com/jonbaldie/hach/issues/220) | `TaskStop` reports success but the background command keeps running | A cancelled command keeps running after Hach exits |
| 4 | [#221](https://github.com/jonbaldie/hach/issues/221) | Headless denial tells you to re-run with `acceptEdits` when you already did | The printed remedy cannot work |
| 5 | [#222](https://github.com/jonbaldie/hach/issues/222) | `TodoWrite` checklist is invisible to `/tasks` and `TaskList` | Saved work does not show up where the user looks |

### 1. An unmet goal exits 0 (#223)

**Starting conditions.** Fresh workspace. `unicorn.txt` does not exist.

**Replay.** `hach --no-tui --permission-mode acceptEdits --max-turns 6` with a system prompt that forbids tools, and `/goal the file unicorn.txt contains the exact text UNICORN-9`. Then `hach -c "Please continue."`

**Expected.** Non-zero exit. The summary does not say the task completed. If the goal is still active, `-c` continues the goal loop.

**Actual** (`REPRO-goal-blocked-exit.txt`, matching `j1-goal-blocked.txt`):

```
[Goal] Evaluated: GoalNotYetMet
[Goal] No progress detected. Goal still active: ...
Task completed.
Status:    GoalActive
exit=0
unicorn.txt: missing
```

`-c` printed `Starting agent loop`, answered `MANGO`, and still did not create the file (`j1-goal-continue.txt`).

A met goal in the same build exits 0 with `Status: GoalAchieved` (`j1-goal-happy.txt`, and the TUI notice `Goal achieved` in `tui-goal.txt`). The success path works. The unmet path is reported as success.

### 2. `ExitPlanMode` forgets `acceptEdits` (#219)

**Starting conditions.** `secret.txt` is `ORIGINAL`. Banner shows `Permissions: acceptEdits`.

**Replay.** Ask for `EnterPlanMode`, then `ExitPlanMode`, then `write_file` of `PWNED-AFTER-PLAN` to `secret.txt`.

**Expected.** The write is allowed, because the user selected `acceptEdits`.

**Actual** (`REPRO-plan-exit-acceptedits.txt`, matching `j3-plan-exit-acceptedits.txt`):

```
[Tool ExitPlanMode Success]
Exited plan mode. The agent is now in standard execution mode.
[Permission Denied] write_file: ... Re-run with --permission-mode acceptEdits ...
Task successfully completed!
exit=0
secret.txt: ORIGINAL
```

A `write_file` that never entered plan mode succeeded under the same flag. `--permission-mode plan` and TUI `/plan` still deny writes (`j3-plan-deny.txt`, `tui-plan.txt`: `Plan mode is read-only`, file stayed `ORIGINAL`). The defect is leaving plan mode, not entering it.

### 3. `TaskStop` does not stop the process (#220)

**Replay.** Under `--permission-mode dontAsk`, `TaskCreate` with `printf STOP-REPLAY > stop-out.txt; sleep 50`, then `TaskStop` on the returned id.

**Expected.** After success, `sleep 50` is gone.

**Actual** (`REPRO-taskstop.txt`, same shape as `j2-tasks-dontask.txt` with `sleep 90`):

```
[Tool TaskStop Success]
Stopped task bg-1
exit=0
stop-out: STOP-REPLAY
sleep 50   still in ps after Hach exited
```

`TaskCreate` did start the command, and `Monitor` / `TaskList` reported the in-process task. The leftover sleep was killed by hand after the observation.

### 4. The headless denial names a flag that is already set (#221)

**Replay.** `--permission-mode acceptEdits`, ask for `run_command` `printf ACCEPT-CMD > cmd-out.txt`.

**Expected.** The command runs, or the denial names a mode that would allow it. It must not say to pass `acceptEdits` again.

**Actual** (`REPRO-acceptedits-command.txt`; also the `run_command` denial in `j1-goal-happy.txt` and the `TaskCreate` denial in `j2-tasks-create.txt`):

```
Permissions: acceptEdits
[Permission Denied] run_command: No interactive approval available in --no-tui. Re-run with --permission-mode acceptEdits to allow writes and commands.
exit=1
cmd-out.txt: missing
```

`--permission-mode dontAsk` did run `TaskCreate`. A `write_file` under `acceptEdits` did succeed. The mode is in effect. The message is wrong about what it allows.

### 5. A saved checklist does not appear in `/tasks` (#222)

**Replay.** `TodoWrite` with `["ship-the-report"]`, then `TaskList`. Open the TUI in that workspace and run `/tasks`.

**Expected.** Both show `ship-the-report`.

**Actual** (`j2-todowrite-tasklist.txt`, `tui-tasks.txt`):

```
[Tool TodoWrite Success]
Saved 1 todo items to .claude/todos.json.
[Tool TaskList Success]
No active tasks.
```

`.claude/todos.json` is `["ship-the-report"]`. TUI `/tasks` printed `Tasks:` / `No active tasks.` A `TaskCreate` item does show up in `TaskList` in the same process. `/tasks` does not read the file `TodoWrite` writes.

---

## Rejected candidates (verified working)

- **Headless `/goal` that requires a file write.** `marker.txt` became `GOAL-MET-7741`, evaluator returned `GoalMet`, status `GoalAchieved`, exit 0 (`j1-goal-happy.txt`).
- **The same goal in the TUI.** Notice `Goal achieved: the file tui-marker.txt exists and contains exactly TUI-GOAL-26`. File contents were `TUI-GOAL-26` (`tui-goal.txt`).
- **Goal already met.** Replay read the existing file, evaluator returned `GoalMet`, file unchanged, exit 0 (`j1-goal-already-met-replay.txt`).
- **Plan mode blocks writes.** Headless `--permission-mode plan` and TUI `/plan` both denied `write_file` and left the file as `ORIGINAL`.
- **`TaskCreate` and `Monitor` under `dontAsk`.** The command ran and the task was listed.
- **`write_file` under `acceptEdits`**, when plan mode was never entered.
- **TUI `/permissions`.** Reported `acceptEdits`, matching the flag.
- **TUI quit.** Ctrl-C at idle exited 0.

## Unresolved

- **Unparseable goal-evaluator response.** One already-met run evaluated `GoalNotYetMet — Could not parse evaluator response` (6 completion tokens) and then hit `--max-turns` with the file already correct (`j1-goal-already-met.txt`). A replay of the same condition achieved the goal (`j1-goal-already-met-replay.txt`). Not confirmed.

## Usability observations (not filed as bugs)

- The session id is still not printed, headless or in the TUI, so `--resume <id>` still means reading `.agents/sessions/` by hand. Same observation as the 2026-09-19 pass.
- The goal evaluator judges the transcript only. It cannot open the file. On the successful runs the transcript did contain a `read_file` or a write result, so the verdict matched the disk. A false assistant claim was not separately tested.
- `acceptEdits` auto-approves writes and not commands. That split is visible in behaviour. The bug is the denial text claiming otherwise (#221), not the split itself.

## Journeys exercised

1. **Keep going until a condition is true.** Ordinary path: headless `/goal` that creates a marker file. Variations: TUI `/goal`, goal already met, goal that cannot be met, and `-c` after a blocked goal. Lasting effect checked on disk and in the exit status. → #223.
2. **Track work, then look at it.** Ordinary path: `TaskCreate` a background command, `Monitor`, `TaskList`. Variations: `TaskStop`, `TodoWrite` then `TaskList`, TUI `/tasks` after a saved checklist, and the same create under `acceptEdits` instead of `dontAsk`. Lasting effects checked in process listings and `.claude/todos.json`. → #220, #221, #222.
3. **Plan without changing files, then leave plan mode.** Ordinary path: `--permission-mode plan` and TUI `/plan`, both denying a write. Variation: `EnterPlanMode` then `ExitPlanMode` inside an `acceptEdits` session, then a write. Lasting effect checked in `secret.txt`. → #219.

## Blocked / unexplored areas

- **MCP and subagents.** Still not a user journey. `/mcp` says it is not implemented. The fake-success stubs are already tracked as [#188](https://github.com/jonbaldie/hach/issues/188).
- **Hierarchical memory and `@import`.** Not exercised. Already tracked as [#215](https://github.com/jonbaldie/hach/issues/215).
- **Goal-evaluator spend against `--max-budget-usd`.** Not exercised. Already tracked as [#178](https://github.com/jonbaldie/hach/issues/178).
- **Auto-compaction, Docker, and `--resume <id>`.** Not exercised.

## Limitations

- The TUI was driven through a synthetic pty. Screens were rendered with `pyte`. Wide-glyph rows in the rendered transcript are misaligned; that was not treated as a product bug. Every TUI finding was checked against the filesystem (`tui-marker.txt`, `secret.txt`, `.claude/todos.json`).
- Getting the model to call a specific tool sometimes needs an ordinary retry. Each confirmed finding is about what the harness did once that call was made, and each was replayed from a clean starting state.
- One already-met goal evaluation failed to parse and did not recur. It is unresolved, above.
