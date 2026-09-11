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

## Configuration

Hach connects to the OpenRouter API. You must set your API key and model before running the harness.

### API key

Set your OpenRouter API key as an environment variable or in a .env file:

```bash
export OPENROUTER_API_KEY="your-api-key"
```

In a .env file:

```text
OPENROUTER_API_KEY=your-api-key
```

### Model selection

You can set the model in three ways:

1. Pass the --model flag on the command line:

```bash
hach --model anthropic/claude-3.5-sonnet "Your prompt"
```

2. Set the OPENROUTER_MODEL environment variable:

```bash
export OPENROUTER_MODEL="anthropic/claude-3.5-sonnet"
```

3. Add the model to your .env file:

```text
OPENROUTER_MODEL=anthropic/claude-3.5-sonnet
```

## Development and testing

Fleet shares an 8-core macOS host with other repositories. Run only one Hach test or fuzz campaign at a time. The standard test command is `cabal test hach:test:hach-test --test-show-details=always`.

The `hach` and `hach-integration-test` executables use the threaded RTS and default to `-N`. Set `GHCRTS=-N2` when you run those executables locally to limit them to two capabilities. Do not export `GHCRTS=-N2` for `hach-test` unless that test suite is first built with `-threaded`; the current test binary rejects `-N2`.

Limit local Hach containers to two CPUs and 2 GiB of memory. For example:

```bash
docker run --cpus=2 --memory=2g -it --rm -v "$(pwd)":/workspace hach:dev
```
