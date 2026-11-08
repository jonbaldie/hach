---
name: verify-path-deny
description: "Verify Hach path-based write denials after changing permission rules, path normalization, symlink resolution, or workspace write tools."
---

# Verify path-based write denials

## Property

A write tool call that reaches a target denied by a matching `permission_rules` entry must be rejected before the target changes, even when the request uses a normalized spelling or a workspace symlink.

## Harm

A user can overwrite a denied file if permission matching and the later filesystem write identify it by different paths.

## Run

From the Hach repository root, run:

```sh
GHCRTS=-N2 hach --exec 'python3 .agents/skills/verify-path-deny/scripts/check.py'
```

The script builds `exe:hach` from the checked-out source, starts it against a loopback-only OpenAI-compatible fixture, and checks files in temporary workspaces. It uses only the Python standard library and adds no package dependency. It requires Python 3.7 or newer. The recorded validation used Python 3.14.5, Cabal 3.16.1.0, GHC 9.12.1, and Hach 0.1.15.0 built at the sync commit. The GHC version matches the repository's CI toolchain in `.github/workflows/ci.yml`. Run one checker at a time.

Exit status:

- `0`: all generated denied calls returned a permission-rule denial, all targets kept their original bytes, and both allowed controls wrote.
- `1`: Hach reported a successful write for a denied call, the target bytes changed, or the target was removed or replaced by a directory. Each counterexample is printed as `VIOLATION <case id>`.
- `2`: the build, fixture, CLI run, result accounting, or a tool response outside the expected success/denial forms failed. Treat this as an invalid verification run; any confirmed counterexamples are still printed. Inspect the generated arguments and Hach tool contract before deciding whether the product property failed.

## What it covers

The checker runs the real Hach executable and tests the Cartesian product of:

- Permission modes: `acceptEdits`, `dontAsk`.
- Registered write tool spellings: `write_file`, `replace_file_content`, `Edit`, `edit`.
- Path forms: direct, `./`, an existing directory followed by `..`, a file symlink, a two-link chain, and a directory symlink.
- Matching rules: exact path and a leading `**/` glob.

That is 96 denied calls and one non-denied write control in each mode. Every call has its own file and rule. The fixture checks both Hach's returned tool result and the destination bytes. The loopback server supplies the calls and does not contact a model provider.

This is bounded verification of those generated cases, not a proof for every path or rule. It does not cover other glob forms, hard links, paths outside the workspace, concurrent filesystem changes, hooks, command tools, or other filesystem implementations.

## Current finding

At sync commit `9d84acae31831bb29e355ad29c0d7c0c066b2e80`, the checker finds 48 violations. They are the file, chained, and directory symlink cases for every write tool spelling, rule shape, and permission mode. Direct, `./`, and `..` spellings are denied. The symlink cases change `secret.txt` through an alias even though the rule names `secret.txt`.

This behavior is filed as [Hach issue #310](https://github.com/jonbaldie/hach/issues/310). Keep reporting these failures until the implementation is fixed. Do not suppress these IDs or weaken the property to make the check pass. A new violation outside those 48 cases is a regression; if a fix removes some or all known violations, review the new output against the intended policy.

## Coverage and maintenance

The check covers:

- `src/Hach/Permissions.hs`: `matchRule` and `evalPermissionForAuthorityAs`.
- `src/Hach/Tools.hs`: the `toolRegistry` write aliases and the registered write/edit executors.
- `src/Hach/Paths.hs`: workspace path resolution used by those executors.

The recorded sync commit is the source snapshot where the property was reproduced and the checker was mutation-tested. When any covered code changes, run the checker and inspect every changed case ID. Update the sync commit only after confirming the generated cases still describe the code and the outcome is understood. If the intended policy changes, update the property, generated cases, expected results, mutation check, and coverage record together.

To demonstrate that the checker detects a broken implementation, use a disposable worktree and temporarily change the `RuleDeny` branch in `Hach.Permissions.matchRule` to return `Just PermAllow`. Run the command above: it must report new `direct`, `dot`, and `parent` counterexamples in addition to the known symlink cases. Remove the mutation, rebuild, and rerun; the original set of counterexamples must return. Never keep the mutation.

For a broader audit after a source change that has not been rechecked, use `/maintain-formal-verification-skill`.
