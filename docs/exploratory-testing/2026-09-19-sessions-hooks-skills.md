# Exploratory testing pass: hach sessions, hooks and skills

**Date:** 2026-09-19
**Scope:** public `hach` CLI and TUI, covering session save and resume, hooks and the settings that carry them, skills, and TUI cancellation
**Build:** `main` at `fde12d81276cc92b72d1445cde7d2de0b9f39b83` (reports `hach 0.1.9.0`), GHC 9.12.1, cabal-install 3.16.1.0
**Host:** macOS 25.6.0, APFS (case-insensitive), `GHCRTS=-N2` for all `hach` invocations
**Model:** `openai/gpt-5.6-luna` with `effort_level: high`. One control run used `meta/muse-glimmer-30b`. Both are covered by the standing permission in CLAUDE.md.
**Evidence:** captured in a scratch directory during the pass and removed with the rest of the scratch
state. Every observed value is inlined below or in the filed issue, and each issue carries its own
replay steps. Evidence filenames are kept as provenance labels.

All observations came from the public interface: the `hach` binary on the command line, and the TUI
driven through a real pty (Python `pty.fork()` at 120x40, screens rendered with `pyte`). Source was read
only to explain confirmed failures. Each workspace was a throwaway git repository under `/tmp` with its
own `.env` and `.claude/settings.json`, and `CLAUDE_CONFIG_DIR` pointed at a throwaway home. Finding 6
shows that this does **not** isolate user-level skills, which are still read from `$HOME`.

This pass picks up two areas the [2026-09-17 pass](2026-09-17-cli-tui-permissions.md) left
unexplored (hooks and skills). It also covers session persistence, which landed in #163 after that pass.

---

## Confirmed findings (9 issues filed)

| # | Issue | Title | Impact |
|---|---|---|---|
| 1 | [#164](https://github.com/jonbaldie/hach/issues/164) | Session saved after `--max-turns` stops mid-tool-call can never be resumed | Conversation is lost; the error hides the cause |
| 2 | [#165](https://github.com/jonbaldie/hach/issues/165) | Settings file that fails to parse is silently ignored, dropping its deny rules | **Security**: protection vanishes after an ordinary edit |
| 3 | [#166](https://github.com/jonbaldie/hach/issues/166) | `hooks` only parses as an array of `[event, handlers]` pairs | The natural config does nothing, and triggers #165 |
| 4 | [#167](https://github.com/jonbaldie/hach/issues/167) | `pre_tool_use` hook bypassed by a tool alias (`run_command` vs `Bash`) | **Security**: a blocking hook is not a guard |
| 5 | [#168](https://github.com/jonbaldie/hach/issues/168) | `session_start`, `user_prompt_submit`, `stop` and `http` hooks never run | Accepted config silently does nothing |
| 6 | [#169](https://github.com/jonbaldie/hach/issues/169) | User-level skills ignore `CLAUDE_CONFIG_DIR` | User skills lost, and scratch homes are not isolated |
| 7 | [#170](https://github.com/jonbaldie/hach/issues/170) | `disable-model-invocation: true` not enforced | Human-only skills reach the model |
| 8 | [#171](https://github.com/jonbaldie/hach/issues/171) | TUI: typing during a turn goes to transcript shortcuts; `c` clears it | Conversation view wiped mid-turn |
| 9 | [#172](https://github.com/jonbaldie/hach/issues/172) | TUI: Esc does not stop a running `run_command` | An unwanted command cannot be stopped; "cancelled" is misreported |

### 1. Session left with an unanswered tool call cannot be resumed (#164)

**Starting conditions.** Fresh workspace, no sessions.

**Replay.** Run `hach --no-tui --max-turns 1 "Use the read_file tool to read README.md, then tell me what it says."`,
then run `hach --no-tui -c "Please continue."` twice.

**Expected.** The saved session can be continued.

**Actual** (`REPRO-session-dangling-tool-call.txt`, a clean replay that matches the exploratory runs `s2-*`):

```
run 1 exit=1
saved roles: ['system', 'user', 'assistant']
Agent failed with error: OpenRouter API error: Provider returned error
run 2 (-c) exit=1
Agent failed with error: OpenRouter API error: Provider returned error
run 3 (-c) exit=1
```

The saved assistant message carries `tool_calls` with no tool results. The provider's raw 400
(`s3-raw-provider-error.txt`) is `No tool output found for function call call_…`, but Hach prints only
`Provider returned error`. `meta/muse-glimmer-30b` accepted the same session (`s2-muse.txt`), so how
severe this is depends on the provider.

### 2–3. Settings that fail to parse are dropped silently, and `hooks` fails for the natural form (#165, #166)

**Starting conditions.** `.claude/settings.json` with the rule
`{"action":"deny","tool":"write_file","path":"**/secret.txt"}` and `secret.txt` containing `ORIGINAL`.

**Replay.** Run `hach --no-tui --permission-mode acceptEdits` and ask for a `write_file` to `secret.txt`.
Do this once per settings variant.

**Expected.** The deny rule applies in every variant, or Hach refuses to start and names the parse error.

**Actual** (`REPRO-settings-silent-discard.txt`):

```
[control] secret.txt after run: ORIGINAL                  {RULE}
[hooks-object] secret.txt after run: CHANGED              {RULE,"hooks":{"pre_tool_use":[...]}}
[trailing-comma] secret.txt after run: CHANGED            {RULE,}
[claude-code-event-name] secret.txt after run: CHANGED    {RULE,"hooks":[["PreToolUse",[]]]}
```

`hooks` only decodes as `[["pre_tool_use",[...]]]`. This is because `FromJSONKey HookEvent` uses the aeson
default. The object form fails, so the whole file is discarded, and the user's deny rules go with it.
With the pair form, the same `run_command` hook fires and blocks (`h3-pairs.txt`). With the object form
it does not fire (`h1-control.txt`).

### 4. Hook matcher bypassed by a tool alias (#167)

**Replay.** Set up a `pre_tool_use` hook with matcher `run_command` and handler `echo blocked; exit 2`.
Ask for the command once via `run_command` and once via `Bash`.

**Actual** (`REPRO-hook-alias-bypass.txt`, with the same result in `h4-alias.txt`):

```
[Tool Executing] run_command
[Hook PreToolUse] Blocked tool execution
[run_command] file created: no
[Tool Executing] Bash
[Tool Bash Success]
[Bash] file created: YES
```

Permission rules canonicalise the tool name, but hooks match the raw function name. This is the same
class of defect as #149.

### 5. Lifecycle hook events and `http` handlers never run (#168)

**Replay.** Configure a command hook that writes its stdin to a file for each of the seven accepted
events, plus an `http` `stop` handler pointed at a local listener. Run one headless `read_file` task.

**Actual** (`REPRO-hook-events.txt`):

```
session_start: never ran
user_prompt_submit: never ran
pre_tool_use: RAN, stdin={"path":"README.md"}
post_tool_use: RAN, stdin="hello"
stop: never ran
http-stop: never ran
```

### 6. User skills ignore `CLAUDE_CONFIG_DIR` (#169)

**Replay.** Put a `whereami` skill in `$CLAUDE_CONFIG_DIR/.claude/skills` only, then also in
`$HOME/.claude/skills`, and run `/whereami`. As a control, use a user-settings deny rule in the same
`CLAUDE_CONFIG_DIR`.

**Actual** (`REPRO-skills-config-dir.txt`): the skill in `CLAUDE_CONFIG_DIR` is not found, and
`/whereami` goes to the model as plain text. The copy in `$HOME` answers `SKILL-FROM-HOME`. The control
deny rule is honoured, so `CLAUDE_CONFIG_DIR` works for settings but not for skills.

Consequence for earlier reports: the 2026-09-17 pass's statement that pointing `CLAUDE_CONFIG_DIR` at a
throwaway home meant the real user configuration was never read is true for settings but not for skills.

### 7. `disable-model-invocation` ignored (#170)

**Replay.** Add a workspace skill `deploy-prod` with `disable-model-invocation: true`, then ask the model
to call the `Skill` tool with that name.

**Actual** (`REPRO-skill-disable-model-invocation.txt`):

```
[Tool Executing] Skill
  Arguments: {"args":"","name":"deploy-prod"}
[Tool Skill Success]
Skill 'deploy-prod' content:
SECRET-DEPLOY-STEPS: run ./deploy.sh --prod
```

### 8. Typing during a turn clears the TUI transcript (#171)

**Replay.** In the TUI, submit a prompt that runs `sleep 8`. While the tool card shows running, type
`abc`.

**Actual** (`t4-freeze.txt`, with a clean replay in `REPRO-tui-typing-clears.txt`): after submit, focus
is on `Transcript (Active)`, so `c` triggers **clear**. The prompt and tool card disappear, the view
shows `No dialogue yet.`, and the status bar's `ctx` drops from `1.2k` to `0`. When the turn finishes,
only `✦ Assistant DONE-OK` is visible.

### 9. Esc does not stop a running command (#172)

**Replay.** In the TUI, run `sleep 20; touch done.txt` via `run_command`. Press Esc while the card shows
`◌ running...`, and poll `done.txt` every 3s.

**Actual** (`REPRO-tui-esc2*.txt` and `REPRO-tui-esc3*.txt`, two clean replays with identical results):
the screen shows `● running run_command...` for the rest of the command. The card then turns to
`✖ cancelled`, with `✖ error: Turn cancelled by user`, and `done.txt` **is created**. The footer
advertises `esc cancel`. In one exploratory run (`REPRO-tui-esc.txt`), Ctrl-Q after Esc was also ignored
while the tool ran, and the driver's timeout had to kill the process.

A candidate raised while investigating this was that the command ran twice, because a probe saw a
`/bin/sh -c sleep 20` process over an unexpectedly long window. It was **rejected**: with a
`date >> count.txt` command, the file got exactly one line both headless and in the TUI
(`x-count-headless.txt`, `x-count-tui.txt`).

---

## Rejected candidates (verified working)

- **Headless session save and `-c` recall.** Run 1 was told the codeword `PAPAYA-42`. Run 2 with `-c`
  recalled it (`s1-*.txt`).
- **TUI `-c` resume of a headless session.** The TUI recalled `MANGO-77` from a headless run, and the
  saved session then held 5 messages (`t1-*.txt`).
- **Workspace skill via slash command.** `$ARGUMENTS` substitution, `{{file:…}}` inclusion and `!cmd`
  expansion all worked (`s-greet.txt`).
- **`user-invocable: false`.** Such skills are filtered from slash invocation as intended.
- **Pair-form `pre_tool_use` blocking and `post_tool_use` execution.** Both work (`h3-pairs.txt`,
  `REPRO-hook-events.txt`).
- **Ctrl-C at idle in the TUI.** Exits 0. This also served as the control showing that the pty driver
  delivers keys.
- **Command double execution.** Rejected; see finding 9.

## Unresolved

None. Every candidate raised was either confirmed by a replay from a known starting state or rejected
with evidence above.

## Usability observations (not filed as bugs)

These are observations, with suggested improvements marked as such:

- The session ID is never printed, headless or in the TUI, so `--resume <id>` / `--session-id` can only be
  used by reading `.agents/sessions/` by hand. *Suggestion: print the ID at the end of a run.*
- `.agents/sessions/` appears as `?? .agents/` in `git status`. This has the same root as #156.
- The model is never told which skills exist. It can call the `Skill` tool only if the user names the
  skill. *Suggestion: list model-invocable skills (name and description) in the system prompt. This
  depends on #170 being fixed first.*
- Hook stdin carries only the tool args (pre) or the result text (post), with no tool name or event name,
  so a hook without a matcher cannot tell which tool it is handling. This is noted in #168.
- A hook that exits non-zero with a code other than 2 fails silently. Nothing reaches the user or the log.
- The provider's raw error (`metadata.raw`) is discarded, so every provider-side failure reads
  `Provider returned error`. This is noted in #164.

## Journeys exercised

1. **Continue yesterday's conversation.** Ordinary path: headless save and `-c` recall. Variations: TUI
   resume of a headless session, a run cut short by `--max-turns`, a second model, and TUI cancel and
   quit mid-tool. Lasting effects checked in `.agents/sessions/*.jsonl` (roles and tool calls) and on
   disk. → #164, #171, #172.
2. **Guard the agent with hooks.** Ordinary path: a `pre_tool_use` block on `run_command`. Variations:
   object and pair config forms, a Claude Code event name, a trailing comma, a tool alias, every
   lifecycle event, and an `http` handler. Lasting effects checked through files that the command and
   the hooks wrote. → #165, #166, #167, #168.
3. **Package a workflow as a skill.** Ordinary path: a workspace skill via slash command with
   arguments, file inclusion and command expansion. Variations: a user-level skill under
   `CLAUDE_CONFIG_DIR` versus `$HOME`, `user-invocable: false`, and `disable-model-invocation: true`
   through the `Skill` tool. → #169, #170.

## Blocked / unexplored areas

- **`notification` and `pre_compact` hooks.** No call sites exist, so there is no user journey that
  could fire them. They are covered by #168 from source.
- **`mcp` hook handlers, and MCP servers in general.** Not exercised.
- **Skill frontmatter `allowed-tools`, `context: fork`, `agent` and `paths`.** Parsed but never read,
  per the source. Not exercised through a journey, so not filed separately; mentioned in #170.
- **`--resume <id>` and `--session-id`.** Only `-c` was exercised, because the ID is never shown
  (see observations).
- **Session cost tracking.** The source shows `siCostUsd` is never accumulated and `siCreatedAt` is
  rewritten on every save. No user-facing surface shows either value, so neither was filed.
- **Auto-compaction.** Not exercised.

## Limitations

- The TUI was driven through a synthetic pty with timed writes, and screens were rendered with `pyte`.
  Every TUI finding was cross-checked against something outside the TUI: the filesystem (`done.txt`,
  `count.txt`) or the saved session.
- Screen timestamps and the filesystem probe for finding 9 ran on separate clocks, started a moment
  apart. The ordering conclusion (Esc pressed while the card showed running, then `done.txt` created)
  holds either way. Exact seconds are approximate.
- Getting the model to call a specific tool with a specific literal sometimes needs an ordinary retry.
  Each confirmed finding concerns what the harness did once the tool call was made.
- Finding 1 depends on the provider. It reproduces on `openai/gpt-5.6-luna` via OpenRouter, but not on
  `meta/muse-glimmer-30b`.
