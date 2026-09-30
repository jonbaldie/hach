# Hach: Haskell Agentic Coding Harness

[![CI](https://github.com/jonbaldie/hach/actions/workflows/ci.yml/badge.svg)](https://github.com/jonbaldie/hach/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Hach is a coding harness written in Haskell. It runs an autonomous loop between an LLM and your local workspace to read files, edit code, and run terminal commands.

## Installation

### Homebrew (macOS)

```bash
brew install jonbaldie/tap/hach
```

### Build from source

Clone the repository and build the binary with Cabal (requires GHC 9.8 or later and Cabal 3.10 or later):

```bash
git clone https://github.com/jonbaldie/hach.git
cd hach
cabal build exe:hach
```

### Docker

Build the production Docker image:

```bash
docker build -t hach .
```

Run interactively in your current directory:

```bash
docker run --cpus=2 --memory=2g -it --rm \
  -e OPENROUTER_API_KEY="$OPENROUTER_API_KEY" \
  -v "$(pwd)":/workspace \
  hach
```

Or run headless:

```bash
docker run --cpus=2 --memory=2g --rm \
  -e OPENROUTER_API_KEY="$OPENROUTER_API_KEY" \
  -v "$(pwd)":/workspace \
  hach --no-tui "Run the test suite and fix any errors"
```

To use an OpenAI-compatible API instead, pass its settings as environment variables. From inside the container, `localhost` refers to the container itself. To reach a server on the host, use `host.docker.internal` on Docker Desktop, or add `--add-host=host.docker.internal:host-gateway` on Linux:

```bash
docker run --cpus=2 --memory=2g -it --rm \
  --add-host=host.docker.internal:host-gateway \
  -e HACH_PROVIDER=openai-compatible \
  -e OPENAI_BASE_URL=http://host.docker.internal:8080/v1 \
  -e OPENAI_MODEL=local-model \
  -v "$(pwd)":/workspace \
  hach
```

### Development container

Build the development Docker image:

```bash
docker build -f dev.Dockerfile -t hach:dev .
```

Start an interactive shell or run tests inside the container:

```bash
docker run --cpus=2 --memory=2g -it --rm -v "$(pwd)":/workspace hach:dev
```

## Quickstart

Start the interactive terminal interface:

```bash
hach
```

You can pass a prompt directly:

```bash
hach "Inspect the src directory and list all modules"
```

To run without the terminal user interface, add the --no-tui flag:

```bash
hach --no-tui "Run the test suite and fix any errors"
```

To list every command-line option, run `hach --help`.

## Configuration

Hach sends Chat Completions requests to one inference API per run. It supports two providers:

- `openrouter` is the default. It uses the OpenRouter API at `https://openrouter.ai/api/v1` and needs an OpenRouter API key.
- `openai-compatible` sends requests to any API that implements the OpenAI Chat Completions interface. Examples include a hosted gateway, a self-hosted server with a bearer token, or an unauthenticated local server.

Choose the provider with `--provider`, the `HACH_PROVIDER` environment variable, or `llm_provider` in `settings.json`. Each setting is resolved in this order: command-line flag, process environment, `.env` file, then `settings.json`. API keys are read only from the environment or `.env`. Hach never stores them in settings or sessions.

The two providers use separate variables. Hach does not fall back from one provider's key or model to the other's.

| Setting | `openrouter` | `openai-compatible` |
| --- | --- | --- |
| API key | `OPENROUTER_API_KEY` (required) | `OPENAI_API_KEY` (optional; sent as a bearer token when set) |
| Model | `--model`, `OPENROUTER_MODEL`, line 2 of `.env`, `model` in settings | `--model`, `OPENAI_MODEL`, `model` in settings |
| API root | `--base-url`, else `https://openrouter.ai/api/v1` | `--base-url`, `OPENAI_BASE_URL`, `llm_base_url` in settings, else `https://api.openai.com/v1` |

Hach passes the model identifier to the API exactly as you give it. OpenRouter mode ignores `OPENAI_BASE_URL` and `llm_base_url`, so only an explicit `--base-url` changes where an OpenRouter key is sent.

### OpenRouter

Set your OpenRouter API key and a model as environment variables:

```bash
export OPENROUTER_API_KEY="your-api-key"
export OPENROUTER_MODEL="anthropic/claude-3.5-sonnet"
```

You can also put them in a `.env` file:

```text
OPENROUTER_API_KEY=your-api-key
OPENROUTER_MODEL=anthropic/claude-3.5-sonnet
```

You can pass `--model` on the command line instead:

```bash
hach --model anthropic/claude-3.5-sonnet "Your prompt"
```

### OpenAI-compatible APIs

The base URL is the API root, including any version or gateway prefix. Leave off `/chat/completions`, because Hach appends it. For example, `https://api.example.com/v1` sends requests to `https://api.example.com/v1/chat/completions`. Hach refuses a URL that already ends in `/chat/completions`, or that contains credentials, a query string or a fragment.

This example uses a hosted or self-hosted API that expects a bearer token:

```bash
export HACH_PROVIDER=openai-compatible
export OPENAI_BASE_URL="https://llm.example.com/v1"
export OPENAI_API_KEY="your-api-key"
export OPENAI_MODEL="your-model-id"
hach
```

A local server with no authentication needs no API key and no OpenRouter credentials:

```bash
hach --provider openai-compatible --base-url http://localhost:8080/v1 --model local-model
```

You can also set the provider and API root for a project in `.claude/settings.json`:

```json
{
  "llm_provider": "openai-compatible",
  "llm_base_url": "http://localhost:8080/v1",
  "model": "local-model"
}
```

Plain `http://` URLs are unencrypted. Use them only for servers on your own machine or a network you trust.

Hach sends `reasoning_effort` only when `effort_level` is set. Many models served through compatible APIs do not accept it, so leave `effort_level` unset for those models. If an API rejects the value, Hach reports the error. It does not retry the request without the setting.

Compatible APIs often do not report a cost. When the cost is missing, the terminal UI's `/cost` report says so and does not show an estimate.

### Spending limits

`--max-budget-usd` (or `max_budget_usd` in settings) is a best-effort limit, not a hard cap:

- A limit of zero stops Hach before its first model request.
- Hach checks the limit before each request, using the costs the API has reported so far. A single request can therefore take the total past the limit. A final answer that crosses the limit still completes normally.
- A request whose cost the API does not report adds nothing to the total. With such an API, a positive limit cannot bound what you spend.
- Goal-evaluation requests are not yet counted ([#178](https://github.com/jonbaldie/hach/issues/178)).

## Development and testing

Fleet shares an 8-core macOS host with other repositories. Run only one Hach test or fuzz campaign at a time. The standard test command is `cabal test hach:test:hach-test --test-show-details=always`.

The `hach` and `hach-integration-test` executables use the threaded RTS and default to `-N`. Set `GHCRTS=-N2` when you run those executables locally to limit them to two capabilities. Do not export `GHCRTS=-N2` for `hach-test` unless that test suite is first built with `-threaded`; the current test binary rejects `-N2`.

Limit local Hach containers to two CPUs and 2 GiB of memory. For example:

```bash
docker run --cpus=2 --memory=2g -it --rm -v "$(pwd)":/workspace hach:dev
```
