# Exploratory testing pass — hach 0.1.9.0

**Date:** 2026-09-17
**Scope:** public `hach` CLI and TUI (headless flags, permission policy, TUI slash commands, worktrees)
**Build:** hach 0.1.9.0, GHC 9.12.1, cabal-install 3.16.1.0, commit `75d850b311a6d46bbc1a10448963fc26dcb80fbc`, branch `main` (clean)
**Host:** macOS 25.6.0, APFS (case-insensitive), `GHCRTS=-N2` for all `hach` invocations
**Model:** `openai/gpt-5.6-luna`, `effort_level: high` (standing permission per AGENTS.md)
**Evidence:** captured in a scratch directory during the pass and removed with the rest of the scratch
state. Every observed value is inlined below, and each filed issue carries its own replay steps, so no
finding depends on access to the original machine. Evidence filenames are retained as provenance labels.

All observations were produced through the public interface only: the `hach` binary on the command
line, and the TUI driven through a real pty (Python `pty.fork()` + `TIOCSWINSZ` at 120x40, screens
rendered with `pyte`). Source was read to *explain* confirmed failures, never to substitute for
driving the interface. Workspaces were throwaway git repositories under `/tmp` with their own `.env`
and `.claude/settings.json`; `CLAUDE_CONFIG_DIR` pointed at a throwaway home so the real user
configuration was never read or modified. (Correction, 2026-09-19: true for settings only. User-level
skills are always read from `$HOME/.claude/skills`; see #169.)

---

## Confirmed findings (8 issues filed)

| # | Issue | Title | Impact |
|---|---|---|---|
| 1 | [#149](https://github.com/jonbaldie/hach/issues/149) | Permission deny rules bypassed by alternate spellings of the same path | **Security** — user-configured protection is not protection |
| 2 | [#150](https://github.com/jonbaldie/hach/issues/150) | `--max-budget-usd` accepted but never enforced | **Billing** — no spend guard exists |
| 3 | [#151](https://github.com/jonbaldie/hach/issues/151) | `--continue` / `--resume` / `--session-id` accepted, nothing persisted | Silent loss of conversation |
| 4 | [#152](https://github.com/jonbaldie/hach/issues/152) | `--add-dir` accepted, extra directory stays outside the root | Feature is inert |
| 5 | [#153](https://github.com/jonbaldie/hach/issues/153) | No `--help`; usage line lists 3 of ~17 flags | Discoverability |
| 6 | [#154](https://github.com/jonbaldie/hach/issues/154) | Headless default mode denies every write/command, then exits 0 with "success" | README's headless example cannot work; CI cannot detect it |
| 7 | [#155](https://github.com/jonbaldie/hach/issues/155) | Nine TUI slash commands are constant strings (`/copy`, `/tasks`, `/diff`, `/theme`, …) | Fabricated results the user acts on |
| 8 | [#156](https://github.com/jonbaldie/hach/issues/156) | `hach -w` creates worktrees inside the repo, leaving `?? .agents/` | Dirties the user's working tree |

### 1. Permission deny rules are matched on the raw path string (#149)

**Starting conditions.** Workspace with `.claude/settings.json` containing
`{"permission_rules":[{"action":"deny","tool":"write_file","path":"**/secrets.txt"}]}` and
`secrets.txt` containing `SENSITIVE-ORIGINAL`.

**Replay.** Two headless runs in `acceptEdits`, asking for `write_file` at the literal paths
`secrets.txt` then `SECRETS.TXT`.

**Expected.** Both denied — on a case-insensitive filesystem they are the same file.

**Actual** (`REPRO-rules-case.txt`, clean replay from the known starting state):

```
### repro 1a CONTROL: write_file path 'secrets.txt'
[Permission Denied] write_file: Permission denied by policy
file after 1a: SENSITIVE-ORIGINAL

### repro 1b BYPASS: write_file path 'SECRETS.TXT'
[Tool write_file Success]
file after 1b: PWNED-CASE
directory listing: secrets.txt
```

The single directory entry proves the protected file itself was overwritten. A second variant of the
same defect: with the glob `secrets.txt`, `./secrets.txt` is written while `secrets.txt` is denied.

**Repeat observations.** Three runs across two workspaces: `./` bypass confirmed, `sub/../` correctly
denied by the `**` glob, case variant bypassed. The `.git`/`.claude`/`.agents` protected-path layer is
**not** affected — `.GIT/PWN2` was correctly refused — because that layer re-checks the canonicalised
path in the tool runtime. Only user rules have the gap. Same class as the already-closed #141.

**Evidence.** `REPRO-rules-case.txt`, `j9-rule-control.txt`, `j9-rule-dotslash.txt`,
`j9-rule-dotdot.txt`, `j10-case.txt`, `j11-git-control.txt`, `j11-git-case.txt`.

### 2–4. Flags accepted and then ignored (#150, #151, #152)

The parser rejects unknown flags (`Argument error: Unknown flag: --foo`), so accepting a flag is a
promise it takes effect. Three do not:

- `--max-budget-usd 0` ran three billable turns and exited 0 (`j5-budget.txt`). Control: `--max-turns 1`
  aborted correctly with exit 1 (`j5-maxturns.txt`) — so the harness *can* stop a run and signal it.
- `-c` / `-r --session-id`: a codeword planted in run A ("Acknowledged.") was unrecoverable in runs B
  and C, which both answered `please` (`j2-a/b/c.txt`). No `.agents/sessions` directory is ever created,
  by headless or TUI.
- `--add-dir /tmp/hach-et/extra` with `--dangerously-skip-permissions`:
  `Access denied: path '/tmp/hach-et/extra/note.txt' escapes the workspace root.` (`j5-adddir2.txt`).
  Repeated under `acceptEdits` with the same result, isolating it to root resolution rather than the
  permission layer.

### 6. Headless default mode is a silent no-op (#154)

`hach --no-tui 'create a file named hello.txt containing the word hi'` denied `write_file`, denied the
`run_command` fallback, and finished with **"Task successfully completed!"** and **exit 0**;
`hello.txt` was never created (`j1-run2.txt`). The denial text is `Permission denied by policy`, with
no mention that `--no-tui` cannot prompt or that `--permission-mode acceptEdits` is the way through —
which, combined with the absence of `--help` (#153), leaves no path to the fix from inside the product.
Adding `--permission-mode acceptEdits` to the identical command wrote the file immediately.

### 7. TUI slash commands print fabricated results (#155)

`/copy` announced "Last response copied to clipboard." after a real assistant reply ("The quick brown
fox."); `pbpaste` afterwards still returned the pre-planted `SENTINEL-DO-NOT-OVERWRITE-12345`
(`j4-copy.raw`). `/tasks` reported "No active background tasks." while, in the same session, the
`task_list` tool returned `- [pending] #bg-1: sleeper` (`j8c-tasks.raw`). `/diff` reported "Git working
tree diff inspected." with an uncommitted `README.md` change present (`j3-tui1.raw`). `/theme` reported
"dark" against `{"theme":"light"}` in settings. Notices are byte-identical on every invocation, which
is what separates these from a real command with nothing to report. `Git.getGitDiff` and
`Tasks.listTasks`/`formatTaskList` already exist and are never called from these handlers.

---

## Rejected candidates (verified working)

- **Headless edit with `--permission-mode acceptEdits`** — file written, exit 0.
- **TUI approval flow** — permission dialog appeared, `y` approved, the file was actually written
  (`j7-approve-1.raw`, `j7-approve-2.raw`).
- **`-w` worktree isolation** — the file landed inside the worktree, not the main checkout. Only the
  *placement* of the worktree is a defect (#156).
- **`--max-turns`** — correctly aborts and exits 1.
- **Protected-path enforcement** — `.git/PWN` *and* `.GIT/PWN2` both refused ("Protected path"), thanks
  to the canonicalised re-check in `Hach.Tools`.
- **`sub/../secrets.txt` traversal** against a `**/secrets.txt` rule — correctly denied.
- **Workspace containment** — absolute paths outside the root are refused in every mode including
  `bypassPermissions`.
- **`--version`, `--init`, `--exec`, `--model`, `-p/--print`, `--output-format`** — behaved as named.

## Unresolved

None. Every candidate raised during the pass was either confirmed by replay from a known starting
state or rejected with evidence above.

## Usability observations (not filed as bugs)

Observations, with suggested improvements marked as such:

- The TUI approval dialog for `write_file` shows the target path but no preview of the content being
  written, so approval is granted without seeing what lands on disk. *Suggestion: show a content
  preview or diff, as the command approval path shows the command.* (Related to the already-filed #142,
  which covers clipping of long commands.)
- A tool card reads `✓ success (47 chars)`, where the count is the length of the result *message*, not
  of the file written or read. *Suggestion: label the unit, or report bytes written.*
- Eight settings keys are parsed and merged but consumed nowhere outside `Hach.Settings`
  (`max_budget_usd`, `working_dirs`, `auto_compact_limit`, `fallback_model`, `theme`, `keybindings`,
  `status_line`, `output_style`). Three of them are the CLI no-ops filed above; the remainder were not
  separately exercised through a user journey, so they are recorded here rather than filed.

## Journeys exercised

1. **Headless "ask the agent to change a file"** — ordinary path (default mode), variation
   (`acceptEdits`), lasting effect checked on disk, retry after denial observed. → #154.
2. **Interactive TUI session** — prompt, tool approval, denial, cancellation, slash commands, settings
   effects, background tasks, clipboard, quit. Lasting effects checked via the filesystem, `pbpaste`,
   and the `task_list` tool. → #155.
3. **Configuration and policy** — layered settings, `permission_rules` allow/ask/deny, protected paths,
   workspace containment, worktrees, and the full flag surface. → #149, #150, #151, #152, #153, #156.

## Blocked / unexplored areas

- **Hooks** (`setHooks`) — not exercised. Covered by the
  [2026-09-19 pass](2026-09-19-sessions-hooks-skills.md) (#165–#168).
- **MCP servers, plugins, skills discovery beyond `/goal`** — not exercised. Skills are covered by the
  [2026-09-19 pass](2026-09-19-sessions-hooks-skills.md) (#169, #170).
- **Docker paths** in the README — not exercised (host-only pass).
- **Auto-compaction and long-context behaviour** — not exercised; would need a long, expensive session.
- **Multi-turn goal loop (`/goal`)** — invoked only far enough to confirm argument handling.

## Limitations

- The TUI was driven through a synthetic pty with timed writes rather than by a human typist; screens
  were rendered with `pyte`. Timing-sensitive redraw behaviour may differ from an interactive terminal.
  Every TUI finding above was cross-checked against a non-TUI observable (the filesystem, `pbpaste`, or
  a tool result in the same session), so none rests on the renderer alone.
- Model-dependent steps (getting the model to call a specific tool with a specific literal path) may
  need ordinary retries; each confirmed finding is about what the harness did once the tool call was
  made, and each was replayed from a clean starting state.
- One observation problem was hit and repaired rather than reported: `script -q /dev/null` could not
  drive the TUI (the shell echoed input before vty attached, producing a hang). It was replaced with
  the `pty.fork()` driver before any TUI conclusion was drawn.
- Case-sensitivity findings are specific to a case-insensitive filesystem (macOS APFS default, and
  Windows). On a case-sensitive filesystem the case variant would create a separate file rather than
  overwrite the protected one — still a rule bypass, with different blast radius.
