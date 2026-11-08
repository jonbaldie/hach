## Impact

Resuming a damaged saved session silently drops malformed transcript lines, sends incomplete history to the model, and overwrites the damaged transcript with that partial history. Users lose the original evidence needed to repair the session and receive no warning that their earlier instructions were omitted.

## Build and starting conditions

- `main` at `e6f93f79969537669670001361809c9005946652`, hach 0.1.15.0, macOS arm64, GHC 9.12.1.
- Isolated workspace/configuration; `GHCRTS=-N2`.
- Public CLI and a local OpenAI-compatible HTTP fixture returning a standard successful completion. No internal Hach APIs called.
- **Fault injection disclosed:** one saved user-message line is deliberately replaced with malformed JSON, simulating a damaged session file. This report does not claim Hach caused the initial corruption.

## Replay

1. Start a compatible API server returning a normal `choices[0].message` response.
2. In a new workspace, run:
   ```sh
   hach -p --output-format json --provider openai-compatible \
     --base-url http://127.0.0.1:PORT/v1 --model fixture-model \
     'Remember an important project requirement: never delete customer data.'
   ```
3. In the generated `.agents/sessions/*.jsonl`, replace exactly the user-message line containing that requirement with `{CORRUPTED user message`, leaving metadata and other lines unchanged.
4. Run the same command with `--continue 'Continue the project.'`.
5. Inspect the outgoing request and the saved transcript.

Runnable driver: `python3 docs/exploratory-testing/evidence/2026-10-24-cli/explore.py reliability` after building `exe:hach`. The driver starts/stops its own local API fixture and repeats the corruption scenario twice in fresh workspaces.

## Expected

Reject a malformed saved transcript with a useful diagnostic before making a model request, preserving the original bytes. If partial recovery is intentional, it must be explicit, disclose omitted lines, and preserve the damaged file rather than silently rewriting it.

## Actual (2/2 fresh workspaces)

- Exit status **0**, stdout `{"answer":"fixture response"}`, stderr **empty**.
- One inference request made; its messages omit the damaged user turn without any warning.
- The same session transcript is overwritten. The damaged line is gone, the previous assistant answer is retained, and the new user/assistant turns are appended.

## Evidence / likely seam

- [Raw CLI actions and before/damaged/after bytes](https://github.com/jonbaldie/hach/blob/main/docs/exploratory-testing/evidence/2026-10-24-cli/journeys.jsonl): `corrupt-0-*` and `corrupt-1-*` records, including outgoing messages.
- [Exploratory report](https://github.com/jonbaldie/hach/blob/main/docs/exploratory-testing/2026-10-24-cli-reliability.md).
- `src/Hach/Sessions.hs`, `loadSession`: `catMaybes` silently discards `Aeson.decodeStrict` failures. A subsequent `saveRunSession` replaces the original transcript.

No product fix is included in this exploratory pass.
