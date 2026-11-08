# Exploratory testing: workspaces and session recovery

**Date:** 2026-10-29 UTC
**Source:** `a10c2a1`, Hach 0.1.16.0
**Environment:** Linux x86-64, GHC 9.12.1, Cabal 3.14.1.1, `GHCRTS=-N2`
**Evidence:** [driver](evidence/2026-10-29-cli/explore.py), [actions and results](evidence/2026-10-29-cli/journeys.jsonl), [locked build](evidence/2026-10-29-cli/setup-ghcup.log), [initial build attempt](evidence/2026-10-29-cli/setup.log), [missing image tag](evidence/2026-10-29-cli/setup-locked.log).

## Results

No new bugs were confirmed. The session corruption replay checked the previously reported [#285](https://github.com/jonbaldie/hach/issues/285), which is closed. No new issue was filed.

| User goal | Ordinary path | Variation and lasting effects |
| --- | --- | --- |
| Initialize a project and work in an isolated git worktree | `--init` created guidelines and `-w explore --exec` edited a tracked file. | A second `--init` left the file unchanged. Reopening the worktree retained the edit and the root file remained `original`. A path with spaces and `café` worked. A traversal name returned a JSON error. |
| Recover from an API error and protect saved sessions | A local compatible API returned a response. A 503 produced a JSON error, and a retry succeeded. | Zero budget made no request. A final answer costing more than the positive budget completed, as documented. Two damaged transcripts each produced a JSON load error, made no request, and retained their damaged bytes. |

The `--exec` command preserved stdout, stderr, and exit status 7. A command emitting invalid UTF-8 completed without a crash. The driver decodes captured output with replacement, so it cannot attribute the replacement characters to Hach. These are CLI observations, not model evaluations.

## Session corruption replay

The prior report observed silent data loss when a damaged session was resumed. This pass reproduced its starting conditions twice using a local fixture. Hach created a session, then the driver deliberately replaced one user-message JSONL line with malformed JSON. On `--continue`, Hach exited 1 with a JSON session-load error. The fixture saw zero new requests, and the transcript remained byte-for-byte unchanged. This replay rejects the old failure on this revision; it does not prove that every corruption case is safe. The original and damaged bytes, final bytes, and request counts are in the evidence file.

The positive-budget completion returned normally after the fixture reported $0.02 against a $0.01 limit. This is not a bug because the README documents final-answer overshoot. No unresolved product candidates remain in the exercised paths.

## Setup and limits

The checked-in `cabal.project.freeze` was used for `cabal build exe:hach -j2`. The first attempt used the documented `haskell:9.12-bookworm` image, which supplied GHC 9.12.4 and could not satisfy the freeze's `base == 4.21.0.0`. The exact `9.12.1-bookworm` image tag was unavailable. The successful attempt installed GHC 9.12.1 inside the worktree and used the same frozen plan. The initial mismatch is consistent with the closed compiler guidance issue [#294](https://github.com/jonbaldie/hach/issues/294); the Dockerfile itself was not built or evaluated in this pass.

Each CLI journey used a fresh temporary workspace and isolated configuration. The HTTP fixture returned deterministic responses. It cannot establish real-provider availability or model quality. No OpenRouter key was available in the environment or checkout, so a live coding journey was not attempted. Interactive TUI, cancellation, MCP, and long-running tasks were not explored. No full test suite was run.

The driver shuts down the fixture in a `finally` block and removes its temporary workspace. The container was registered with an exit trap before it started. The run removed its container and pulled image after the build. No Docker volumes were created. Build outputs and the compiler installation were confined to the worktree. The run plans to remove them with the worktree after publication.
