# Live TUI `/implement` investigation

Tested commit `4c34e445caf2750ff41e3e1c12d1b03890719b6f` on macOS with a real PTY, Vty/Brick rendering, real tools, fresh temporary Git projects, and OpenRouter's `openai/gpt-5.6-luna`. Every transmitted request specified `reasoning.effort: high`. Production source was unchanged. Nine sessions made 69 API calls; OpenRouter response usage reported a total cost of $0.01383331.

## Confirmed findings

### P1: Default TUI permissions deny actions without asking

A new TUI session in `default` mode activates `/implement`, reads files, then denies edits and shell commands with `Execution denied by permission policy.` No approval interaction is offered. The requested implementation remains unchanged. Two full runs exhausted their 12-turn limit; another returned a refusal.

The reduced sequence is one submission: `/implement Write hello.txt containing hello.` In three clean six-turn runs (`minimal-2`, `minimal-3`, `minimal-4`), `write_file` was denied and `hello.txt` was absent. No earlier slash command or focus change is necessary. Removing the submission removes the trigger.

`src/Hach/Interpreter/IO.hs:302-310` maps `PermAsk` directly to `False`. Its comment describes headless behavior, but the TUI uses the same interpreter. The default permission policy asks for writes and commands; the TUI never resolves those asks.

With `permission_mode: dontAsk`, the full clamp implementation completed, all five original, unchanged tests passed when independently rerun, and the model reviewed and committed its change locally. This comparison isolates the permission blocker from skill activation and file tooling.

### P2: Configured reasoning effort is omitted from API requests

Each fresh project configured `.agents/settings.json` with `effort_level: high`. The first request produced by Hach contained only `messages`, `model`, `tools`, and `tool_choice`; neither `reasoning` nor `reasoning_effort` was present. This reproduced in all runs, including the three reduced cases. One submission is sufficient to observe it.

`src/Hach/Settings.hs` parses the value, but `app/Main.hs:56-64` does not pass it to the interpreter. `src/Hach/OpenRouter.hs:33-53` has no request field or serialization for it. Users cannot rely on this setting to control model effort.

To respect the requested high effort during the investigation, `Hook.hs` records the original request and then inserts `reasoning: {"effort":"high"}` before transmission. It rejects any model other than Luna. This intervention is part of the test harness, not a production fix. The payload uses OpenRouter's [documented reasoning parameter](https://openrouter.ai/docs/guides/best-practices/reasoning-tokens).

## Replay

Requires Cabal/GHC, Git, Python with `pyte`, the global user-invocable `implement` skill, and an OpenRouter key in the environment or the repository's `.env`. The harness does not log request headers or the key. It makes real billable requests and executes model-selected commands within a temporary fixture, depending on the selected permission mode.

From the repository root:

```sh
python3 -m venv /tmp/hach-tui-probe-venv
/tmp/hach-tui-probe-venv/bin/pip install pyte
/tmp/hach-tui-probe-venv/bin/python scripts/repro-tui-implement/replay.py default
/tmp/hach-tui-probe-venv/bin/python scripts/repro-tui-implement/replay.py dontAsk
```

Reduced permission reproducer:

```sh
HACH_PROBE_PROMPT='/implement Write hello.txt containing hello.' \
HACH_PROBE_MAX_TURNS=6 \
/tmp/hach-tui-probe-venv/bin/python scripts/repro-tui-implement/replay.py default
```

Each invocation builds an instrumented copy of the CLI, creates a new fixture directory, verifies a clean Git status and five initially failing tests, types the slash invocation into the TUI, waits for completion, scrolls up/down, and quits. `HACH_PROBE_OUTPUT` optionally chooses the artifact directory. An optional second positional argument chooses a unique run name; existing directories are rejected.

The CLI copy differs only in the HTTP manager hook and a fixed version value replacing Cabal's generated version module. Configuration resolution, slash expansion, TUI state transitions, permissions, and tool execution use Hach's compiled code. The hook can run twice per logical API call; count response files for billable calls. Original request files from the second hook invocation already contain the injected effort.

Artifacts include action inputs, terminal bytes, rendered screen text, structured probes after each action, source contents, Git state, independent test results, and original/transmitted requests and responses. `result.json` classifies skill expansion, omitted effort, enforced live model/effort, denied tools, and resulting files. `evidence.json` preserves compact observations and original artifact paths from this investigation.

The first harness version waited for `idle` or `error` but missed Hach's `ready` status. The completed `default-2` and `allowed-1` sessions therefore waited until the observation timeout. This was a harness defect, not a Hach hang. It was corrected before the reduced replays and the successful `allowed-2` control. The three-turn `minimal-1` trial was too short to reach a write and is not counted among the three reduced write reproductions.

Mouse events were exercised as smoke checks; this investigation makes no additional claim about viewport displacement. No provider error or crash was observed. Live model tool choices can vary; the repeated policy failures and recorded request omissions are the confirmed bugs.
