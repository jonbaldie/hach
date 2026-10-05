# Hach

A minimalist, principled agentic coding harness in Haskell.

## Language

**Hach**:
The canonical name for the project, CLI binary, Cabal package, and repository.
_Avoid_: Agent, Hatch, agent-harness

**Harness**:
The autonomous interaction runtime coordinating the policy loop between an LLM and local workspace tools.
_Avoid_: Framework, wrapper

**Free Monad Core**:
The core design pattern structuring agent interactions as a free monad over an interaction signature functor.
_Avoid_: Callback framework, reactive engine

**Tap**:
The dedicated Homebrew tap repository (`jonbaldie/homebrew-tap`) serving distribution formulae for macOS.
_Avoid_: Core formula, brew package

**Transcript**:
The unified, chronological stream of interaction events displayed in the TUI, interleaving user input, assistant prose, tool calls, and system notices.
_Avoid_: Dialogue history, conversation log

**Transcript Item**:
An individual chronological entry within the transcript (user prompt, assistant message, system message, notice, or tool card).
_Avoid_: History item, chat message

**Tool Card**:
A transcript item representing an invoked tool call, tracking its identifier, parameters, execution lifecycle, and output.
_Avoid_: Tool item, activity card

**Provider**:
The inference service a run talks to, either `openrouter` (the default) or `openai-compatible`. Each provider has its own credentials and model variables.
_Avoid_: Backend, vendor

**Inference Connection**:
The resolved provider, API root, and optional API key that every model request in a run uses. It is chosen once at startup and does not change during the run. In code, the provider is the `InferenceInterface`.
_Avoid_: Client, endpoint config
