# Exploratory testing: headless reliability and permissions

**Date:** 2026-11-06 UTC

**Revision:** `e7394a5`, Hach 0.1.17.0, Linux x86-64, `GHCRTS=-N2`

**Evidence:** [journey driver](evidence/2026-11-06-headless/explore.py), [CLI results and request history](evidence/2026-11-06-headless/journeys.jsonl), [permission regression output](evidence/2026-11-06-headless/permissions.txt).

## Results

No new bugs were confirmed or filed. The replays of [#285](https://github.com/jonbaldie/hach/issues/285) and [#303](https://github.com/jonbaldie/hach/issues/303) did not reproduce those closed bugs. There are no unresolved product candidates in these paths.

| User goal | Ordinary path | Variation and lasting effects |
| --- | --- | --- |
| Initialize and isolate a workspace | `--init` created `CLAUDE.md`; `-w explore --exec` changed a tracked file in a new worktree. | A second `--init` left the template unchanged. Reopening the worktree retained the change while the root file stayed `original`. A name containing `../` failed with a JSON error. The workspace path contained spaces and `café`. |
| Recover from errors and resume a session | A compatible API fixture returned an answer. A valid `--continue` sent the prior user message and answer in the next request. | A 503 returned a JSON error and retry succeeded. Zero budget produced no API request. A final answer succeeded against a $0.01 budget when the fixture reported a $0.02 cost. This does not independently establish that Hach accounted for the cost. Twice, a deliberately corrupted saved user message caused `--continue` to fail before any request and left the damaged transcript unchanged. |
| Use interaction tools without hiding blocked writes | `SendMessage` completed in default, acceptEdits, and plan modes. | In default headless mode, `AskUserQuestion` or `SendMessage` followed by `write_file` exited 1 with a blocked message and left no file. A configured ask rule also blocked `SendMessage` while a subsequent question completed. The fixture controlled tool calls, not Hach's permissions or file effects. |

The shell command returned stdout `out`, stderr `err`, and exit code 7. A command with invalid UTF-8 bytes exited 0. The driver decodes output with replacement, so it cannot establish which process inserted replacement characters. The write-denial message says `acceptEdits` allows writes and commands, although closed [#221](https://github.com/jonbaldie/hach/issues/221) says that mode does not approve commands. That issue explicitly accepted the current write-specific message. No duplicate was filed. The corruption diagnostic identifies the file and line without reproducing the damaged message.

## Setup and limits

The test binary is the GitHub v0.1.17 Linux release. Its release tag resolves to the tested `origin/main` revision. The release archive passed its published SHA-256 checksum; the extracted binary SHA-256 was `9f50fc82d39a1d6b1a1e0ad27540acff9ce53018da8e2b3d9003b2e96616802a`. This is not a local source build. GHC and Cabal were absent, and the shared host had only 2.0 GB free, so installing an isolated frozen toolchain and dependencies was not safe. No local locked install or full test suite ran.

The drivers used fresh temporary workspaces and isolated HOME and configuration under the exploration worktree. The deterministic HTTP fixture cannot establish real-provider availability or model quality. No OpenRouter key was present in the process environment. Live coding, the interactive TUI, cancellation, MCP, and long-running tasks were not explored. The permissions run reused `scripts/repro-191.py` from this revision. The session driver is adapted from the 2026-10-29 report and uses the release binary under `.explore/bin/hach`. To replay it, set `TMPDIR` to an existing directory inside the worktree and run `python3 docs/exploratory-testing/evidence/2026-11-06-headless/explore.py shell` or replace `shell` with `reliability`. The driver appends to `journeys.jsonl`.

The fixture threads use shutdown, close, and join in `finally`; each temporary workspace is removed on completion. The drivers start their server threads just before entering the cleanup block, so an exception in that narrow interval would bypass that block. The session driver logged fixture ports 41769 and 39247 and their shutdown. The permission fixture used ephemeral ports and closed each server before the next case, but did not log their individual numbers. No containers, images, volumes, or browser sessions were started by this pass. The released binary and temporary configuration remain in the exploration worktree until it is removed after publication.
