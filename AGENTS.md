## Agent skills

### Issue tracker

Issues are tracked in GitHub Issues. See `docs/agents/issue-tracker.md`.

### Triage labels

Uses the default triage-label vocabulary. See `docs/agents/triage-labels.md`.

### Domain docs

Uses a single-context layout. See `docs/agents/domain.md`.

## Live API testing

For exploratory and other testing, agents have standing permission to make billable calls with the user's real OpenRouter API key using `openai/gpt-5.6-luna` or `meta/muse-glimmer-30b`, without further confirmation. Set reasoning effort to `high` or above.

## Local verification and resource limits

Fleet shares an 8-core macOS host with other repositories. Run only one Hach test or fuzz campaign at a time. The standard test command is `cabal test hach:test:hach-test --test-show-details=always`.

The `hach` and `hach-integration-test` executables use the threaded RTS and default to `-N`. Set `GHCRTS=-N2` when you run those executables locally to limit them to two capabilities. Do not export `GHCRTS=-N2` for `hach-test` unless that test suite is first built with `-threaded`; the current test binary rejects `-N2`.

Limit local Hach containers to two CPUs and 2 GiB of memory. For example:

```bash
docker run --cpus=2 --memory=2g -it --rm -v "$(pwd)":/workspace hach:dev
```
