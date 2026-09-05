# Minimal Agentic Coding Harness in Haskell (Functional Pearl)

A minimalist, principled agentic coding harness written in idiomatic Haskell following a **Functional Pearl** architecture.

The harness implements an autonomous interaction loop between an LLM policy (via the OpenRouter API) and a coding environment (file inspection, file creation/modification, directory listing, and shell command execution).

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

1. **`Agent.Interpreter.Pure`**: Evaluates the entire agent dialogue against an in-memory mock environment (`MockEnv`), allowing fast, deterministic testing of multi-turn tool loops without touching the network or disk.
2. **`Agent.Interpreter.IO`**: Connects the agent to the OpenRouter HTTP API and real workspace effects (`read_file`, `write_file`, `run_command`, `list_dir`).

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
cabal test agent:test:agent-test --test-show-details=always
```

### Live OpenRouter Integration Test
Runs an end-to-end multi-turn loop against the real OpenRouter API using the `.env` configuration:
```bash
cabal run agent-integration-test
```

---

## Running the CLI Harness

```bash
# Run with the default model (from line 2 of .env):
cabal run agent-harness -- "Inspect the src directory and summarize the codebase architecture."

# Override the model using the --model flag:
cabal run agent-harness -- --model meta/muse-glimmer-30b "Create hello.txt and verify it"

# Alternatively with equals syntax:
cabal run agent-harness -- --model=anthropic/claude-3.5-sonnet "Run the tests"

# Or run interactively (will prompt for task):
cabal run agent-harness -- --model meta/muse-glimmer-30b
```
