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

Configuration is loaded from `.env`:
- Line 1: `OPENROUTER_API_KEY=<your-key>`
- Line 2: `OPENROUTER_MODEL=<model-name>` (e.g. `meta/muse-glimmer-30b`)

The harness strictly loads the model specified on **line 2** of `.env`.

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
# Pass the task as command-line arguments:
cabal run agent-harness -- "Inspect the src directory and summarize the codebase architecture."

# Or run interactively:
cabal run agent-harness
```
