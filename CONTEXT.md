# Hach

A minimalist, principled agentic coding harness in Haskell following a Functional Pearl architecture.

## Language

**Hach**:
The canonical name for the project, CLI binary, Cabal package, and repository.
_Avoid_: Agent, Hatch, agent-harness

**Harness**:
The autonomous interaction runtime coordinating the policy loop between an LLM and local workspace tools.
_Avoid_: Framework, wrapper

**Free Pearl**:
The core design pattern structuring agent interactions as a free monad over an interaction signature functor.
_Avoid_: Callback framework, reactive engine

**Tap**:
The dedicated Homebrew tap repository (`jonbaldie/homebrew-tap`) serving distribution formulae for macOS.
_Avoid_: Core formula, brew package
