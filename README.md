# Hach: Haskell Agentic Coding Harness (Functional Pearl)

[![CI](https://github.com/jonbaldie/hach/actions/workflows/ci.yml/badge.svg)](https://github.com/jonbaldie/hach/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A minimalist, principled agentic coding harness written in idiomatic Haskell following a **Functional Pearl** architecture.

The harness implements an autonomous interaction loop between an LLM policy (via the OpenRouter API) and a coding environment (file inspection, file creation/modification, directory listing, and shell command execution).

---

## Installation

### Homebrew (macOS)

Install pre-built native binaries via the official tap:

```bash
brew tap jonbaldie/tap
brew install hach
```

### Build from Source (Cabal)

Requires GHC 9.8+ and Cabal 3.10+:

```bash
git clone https://github.com/jonbaldie/hach.git
cd hach
cabal build exe:hach
cabal run hach
```

---

## The Pearl: Interaction as a Free Monad

Most agent frameworks in Python rely on imperative callbacks, implicit mutable state, and complicated mocking frameworks. In this harness, an agent interaction is modeled as a free monad over an interaction signature functor:

$$\text{AgentProgram } a \cong \text{Free } \text{AgentF } a$$

```haskell
-- The atomic interaction signature
data AgentF next
  = PromptLLM   ![Message] ![ToolDef] (AssistantResponse -> next)
  | ExecuteTool !ToolCall (ToolResult -> next)
  | LogEvent    !AgentEvent next
  deriving Functor

-- The Free Monad over AgentF
data AgentProgram a
  = Pure a
  | Free (AgentF (AgentProgram a))
```

### The Algebra and Catamorphism

By separating the interaction structure from its execution semantics, any interpretation is simply an algebra fold:

```haskell
data AgentAlgebra m = AgentAlgebra
  { interpPrompt :: [Message] -> [ToolDef] -> m AssistantResponse
  , interpTool   :: ToolCall -> m ToolResult
  , interpLog    :: AgentEvent -> m ()
  }

foldAgentProgram :: Monad m => AgentAlgebra m -> AgentProgram a -> m a
```

1. **`Hach.Interpreter.Pure`**: Evaluates the entire agent dialogue against an in-memory mock environment (`MockEnv`), allowing fast, deterministic testing of multi-turn tool loops without touching the network or disk.
2. **`Hach.Interpreter.IO`**: Connects the agent to the OpenRouter HTTP API and real workspace effects (`read_file`, `write_file`, `run_command`, `list_dir`).

---

## Available Tools

The harness equips the model with four standard coding tools:

- `read_file`: Reads UTF-8 file contents safely from the workspace.
- `write_file`: Writes or overwrites a file, automatically creating parent directories.
- `run_command`: Executes a shell command inside the workspace and captures exit code, stdout, and stderr.
- `list_dir`: Lists directory contents.

---

## Configuration

Configuration resolution precedence:

1. **OpenRouter API Key**:
   - Process environment variable `OPENROUTER_API_KEY`
   - `.env` file (`OPENROUTER_API_KEY=<key>`)

2. **OpenRouter Model**:
   - `--model <name>` or `--model=<name>` command-line flag
   - Process environment variable `OPENROUTER_MODEL`
   - Line 2 of `.env` (or `OPENROUTER_MODEL=<model>` in `.env`)

---

## Running the Tests

### Unit Tests (Pure Simulation & Wire Format)
```bash
cabal test hach:test:hach-test --test-show-details=always
```

### Live OpenRouter Integration Test
Runs an end-to-end multi-turn loop against the real OpenRouter API using the `.env` configuration:
```bash
cabal run hach-integration-test
```

---

## Interactive Modern TUI

The harness launches directly into a modern, full-screen **Terminal User Interface (TUI)** built with Brick:

- **Header Bar**: Displays real-time operational status (`IDLE`, `THINKING...`, `RUNNING TOOL: <name>`, `ERROR`), active model, and current/maximum turns.
- **Dialogue History Panel**: Scrollable conversation log with color-coded badges for user requests, assistant answers, and system notices.
- **Tool Activity Panel**: Interactive cards for each tool execution (`read_file`, `write_file`, `run_command`, `list_dir`) with expandable inputs and outputs.
- **Task Input Panel**: Multi-line prompt buffer with live cursor.

### Keyboard Shortcuts

| Shortcut | Action |
|---|---|
| `Tab` / `Shift+Tab` | Cycle focus between Input, History, and Tool Activity panels |
| `Enter` | Submit prompt (in Input) or toggle expand/collapse (in Tools) |
| `Ctrl+U` | Clear input buffer |
| `Up` / `Down` | Scroll conversation (in History) or select tool card (in Tools) |
| `PageUp` / `PageDown` | Fast-scroll conversation history |
| `c` | Clear conversation history (in History panel) |
| `Esc` / `Ctrl+C` | Cancel running agent turn, or close help dialog |
| `?` | Toggle shortcut help overlay |
| `Ctrl+Q` | Cleanly exit the application |

---

## Running the Harness

```bash
# Launch interactive full-screen TUI (default):
hach

# Or via Cabal in development:
cabal run hach

# Launch TUI with an initial prompt and model override:
hach --model meta/muse-glimmer-30b "Inspect the src directory"

# Run in headless streaming CLI mode (e.g. for scripting or non-TTY pipes):
hach --no-tui --model meta/muse-glimmer-30b "Say hello"
```

---

## License

This project is licensed under the [MIT License](LICENSE).
