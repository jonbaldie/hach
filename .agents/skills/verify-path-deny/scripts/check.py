#!/usr/bin/env python3
from __future__ import annotations

import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
from collections import Counter
from dataclasses import dataclass
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


MODES = ("acceptEdits", "dontAsk")
TOOL_NAMES = ("write_file", "replace_file_content", "Edit", "edit")
PATH_SHAPES = ("direct", "dot", "parent", "file-symlink", "chain-symlink", "directory-symlink")
RULE_SHAPES = ("exact", "leading-glob")
INITIAL_PREFIX = "original:"
UPDATED_PREFIX = "changed:"
CONTROL_CONTENT = "control-content"


@dataclass(frozen=True)
class Case:
    case_id: str
    mode: str
    tool_name: str
    path_shape: str
    rule_shape: str
    requested_path: str
    target_path: Path
    expected_content: str
    updated_content: str


class FixtureServer(ThreadingHTTPServer):
    daemon_threads = True

    def __init__(self, address, calls):
        super().__init__(address, FixtureHandler)
        self.calls = calls
        self.requests = []
        self.errors = []


class FixtureHandler(BaseHTTPRequestHandler):
    server: FixtureServer

    def do_POST(self):
        try:
            size = int(self.headers.get("Content-Length", "0"))
            body = json.loads(self.rfile.read(size))
            self.server.requests.append((self.path, body))
            if len(self.server.requests) == 1:
                payload = {
                    "id": "path-denial-check",
                    "object": "chat.completion",
                    "created": 0,
                    "model": "path-denial-check",
                    "choices": [{
                        "index": 0,
                        "finish_reason": "tool_calls",
                        "message": {
                            "role": "assistant",
                            "content": None,
                            "tool_calls": self.server.calls,
                        },
                    }],
                }
            elif len(self.server.requests) == 2:
                payload = {
                    "id": "path-denial-check-finished",
                    "object": "chat.completion",
                    "created": 0,
                    "model": "path-denial-check",
                    "choices": [{
                        "index": 0,
                        "finish_reason": "stop",
                        "message": {"role": "assistant", "content": "verification complete"},
                    }],
                }
            else:
                self.server.errors.append(f"unexpected model request to {self.path}")
                self.send_error(500, "unexpected model request")
                return

            encoded = json.dumps(payload).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(encoded)))
            self.end_headers()
            self.wfile.write(encoded)
        except Exception as error:
            self.server.errors.append(f"{type(error).__name__}: {error}")
            self.send_error(500, "fixture request failed")

    def log_message(self, _format, *_args):
        pass


def canonical_tool(name: str) -> str:
    return "replace_file_content" if name in ("replace_file_content", "Edit", "edit") else "write_file"


def argument_json(case: Case) -> str:
    if canonical_tool(case.tool_name) == "write_file":
        arguments = {"path": case.requested_path, "content": case.updated_content}
    else:
        arguments = {
            "path": case.requested_path,
            "old_content": case.expected_content,
            "new_content": case.updated_content,
        }
    return json.dumps(arguments, separators=(",", ":"))


def make_cases(workspace: Path, mode: str):
    cases = []
    rules = []
    calls = []
    serial = 0

    for tool_name in TOOL_NAMES:
        for path_shape in PATH_SHAPES:
            for rule_shape in RULE_SHAPES:
                serial += 1
                case_id = f"{mode}-{serial:02d}-{tool_name}-{path_shape}-{rule_shape}"
                folder = workspace / f"case-{serial:02d}"
                folder.mkdir()
                nested = folder / "nested"
                nested.mkdir()
                target = folder / "secret.txt"
                original = INITIAL_PREFIX + case_id
                updated = UPDATED_PREFIX + case_id
                target.write_text(original, encoding="utf-8")

                if path_shape == "direct":
                    requested_path = f"{folder.name}/secret.txt"
                elif path_shape == "dot":
                    requested_path = f"./{folder.name}/secret.txt"
                elif path_shape == "parent":
                    requested_path = f"{folder.name}/nested/../secret.txt"
                elif path_shape == "file-symlink":
                    (folder / "file-link.txt").symlink_to("secret.txt")
                    requested_path = f"{folder.name}/file-link.txt"
                elif path_shape == "chain-symlink":
                    (folder / "first-link.txt").symlink_to("secret.txt")
                    (folder / "chain-link.txt").symlink_to("first-link.txt")
                    requested_path = f"{folder.name}/chain-link.txt"
                elif path_shape == "directory-symlink":
                    (folder / "directory-link").symlink_to(".")
                    requested_path = f"{folder.name}/directory-link/secret.txt"
                else:
                    raise ValueError(f"unknown path shape: {path_shape}")

                case = Case(
                    case_id=case_id,
                    mode=mode,
                    tool_name=tool_name,
                    path_shape=path_shape,
                    rule_shape=rule_shape,
                    requested_path=requested_path,
                    target_path=target,
                    expected_content=original,
                    updated_content=updated,
                )
                cases.append(case)
                pattern = f"{folder.name}/secret.txt"
                if rule_shape == "leading-glob":
                    pattern = f"**/{pattern}"
                rules.append({
                    "action": "deny",
                    "tool": canonical_tool(tool_name),
                    "path": pattern,
                })
                calls.append({
                    "id": case_id,
                    "type": "function",
                    "function": {
                        "name": tool_name,
                        "arguments": argument_json(case),
                    },
                })

    control_id = f"{mode}-positive-control"
    calls.append({
        "id": control_id,
        "type": "function",
        "function": {
            "name": "write_file",
            "arguments": json.dumps(
                {"path": "public.txt", "content": CONTROL_CONTENT},
                separators=(",", ":"),
            ),
        },
    })
    settings_dir = workspace / ".claude"
    settings_dir.mkdir()
    (settings_dir / "settings.json").write_text(
        json.dumps({"permission_rules": rules}, indent=2) + "\n",
        encoding="utf-8",
    )
    return cases, calls, control_id


def clean_environment(home: Path, isolated_config: Path):
    env = os.environ.copy()
    secret_prefixes = (
        "OPENAI_", "OPENROUTER_", "ANTHROPIC_", "CLAUDE_",
        "GEMINI_", "AZURE_OPENAI_", "HACH_PROVIDER",
    )
    for key in tuple(env):
        if key == "HACH_PROVIDER" or key.startswith(secret_prefixes):
            env.pop(key, None)
    env.update({
        "HOME": str(home),
        "CLAUDE_CONFIG_DIR": str(isolated_config),
        "GHCRTS": "-N2",
        "OPENAI_API_KEY": "formal-check-local-only",
        "NO_PROXY": "127.0.0.1,localhost",
        "no_proxy": "127.0.0.1,localhost",
    })
    return env


def build_hach(repo: Path) -> Path:
    env = os.environ.copy()
    env.pop("GHCRTS", None)
    build = subprocess.run(
        ["cabal", "build", "exe:hach", "-j2"],
        cwd=repo,
        env=env,
        text=True,
        capture_output=True,
        timeout=300,
    )
    if build.returncode:
        raise RuntimeError(
            "cabal build exe:hach failed:\n" + build.stdout + build.stderr
        )
    located = subprocess.run(
        ["cabal", "list-bin", "exe:hach"],
        cwd=repo,
        env=env,
        text=True,
        capture_output=True,
        timeout=30,
    )
    if located.returncode:
        raise RuntimeError("cabal list-bin exe:hach failed:\n" + located.stderr)
    executable = Path(located.stdout.strip())
    if not executable.is_file():
        raise RuntimeError(f"built Hach executable not found: {executable}")
    return executable


def message_text(content) -> str:
    if isinstance(content, str):
        return content
    return json.dumps(content, ensure_ascii=False, sort_keys=True)


def tool_outcome(case: Case, result: str) -> str:
    if "Denied by permission rule" in result:
        return "denied"
    if canonical_tool(case.tool_name) == "write_file":
        success_prefix = "Successfully wrote "
    else:
        success_prefix = "Successfully replaced content in "
    if result.startswith(success_prefix):
        return "success"
    raise RuntimeError(
        f"{case.mode} {case.case_id}: unexpected tool result {result!r}; "
        "inspect the generated arguments and Hach tool contract before interpreting the run"
    )


def run_mode(executable: Path, temp_root: Path, mode: str):
    workspace = temp_root / f"workspace-{mode}"
    home = temp_root / f"home-{mode}"
    workspace.mkdir()
    home.mkdir()
    cases, calls, control_id = make_cases(workspace, mode)

    server = FixtureServer(("127.0.0.1", 0), calls)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    violations = []
    errors = []
    results = {}
    try:
        base_url = f"http://127.0.0.1:{server.server_port}/v1"
        env = clean_environment(home, temp_root / f"isolated-config-{mode}")
        command = [
            str(executable),
            "--print",
            "--no-tui",
            "--provider", "openai-compatible",
            "--base-url", base_url,
            "--model", "path-denial-check",
            "--permission-mode", mode,
            "Use the supplied tool calls to perform the path-denial verification.",
        ]
        try:
            run = subprocess.run(
                command,
                cwd=workspace,
                env=env,
                text=True,
                capture_output=True,
                timeout=120,
            )
            if run.returncode:
                errors.append(
                    f"Hach exited {run.returncode} in {mode}:\n{run.stdout}\n{run.stderr}"
                )
        except Exception as error:
            errors.append(
                f"{mode}: Hach execution failed: {type(error).__name__}: {error}"
            )

        if server.errors:
            errors.extend(f"{mode}: fixture error: {error}" for error in server.errors)
        if len(server.requests) != 2:
            errors.append(
                f"{mode}: expected two model requests, received {len(server.requests)}"
            )

        if len(server.requests) > 1:
            try:
                result_messages = [
                    message for message in server.requests[1][1].get("messages", [])
                    if message.get("role") == "tool"
                ]
                result_ids = Counter(
                    message.get("tool_call_id") for message in result_messages
                )
                expected_ids = {case.case_id for case in cases} | {control_id}
                if (
                    set(result_ids) != expected_ids
                    or any(count != 1 for count in result_ids.values())
                ):
                    missing = sorted(expected_ids - set(result_ids))
                    extra = sorted(set(result_ids) - expected_ids)
                    duplicates = sorted(
                        call_id for call_id, count in result_ids.items() if count != 1
                    )
                    errors.append(
                        f"{mode}: tool result IDs mismatch; missing={missing}, "
                        f"extra={extra}, duplicate-or-null={duplicates}"
                    )
                results = {
                    message["tool_call_id"]: message_text(message.get("content", ""))
                    for message in result_messages
                    if isinstance(message.get("tool_call_id"), str)
                }
            except Exception as error:
                errors.append(
                    f"{mode}: tool results could not be read: "
                    f"{type(error).__name__}: {error}"
                )

        for case in cases:
            reasons = []
            actual = None
            try:
                actual = case.target_path.read_bytes()
                if actual != case.expected_content.encode("utf-8"):
                    reasons.append("target bytes changed")
            except FileNotFoundError:
                reasons.append("target file was removed")
            except IsADirectoryError:
                actual = b"<directory>"
                reasons.append("target path became a directory")
            except OSError as error:
                errors.append(
                    f"{mode} {case.case_id}: target could not be inspected: {error}"
                )

            result = results.get(case.case_id)
            if result is not None:
                try:
                    if tool_outcome(case, result) == "success":
                        reasons.append("tool reported success despite the deny rule")
                except RuntimeError as error:
                    errors.append(str(error))
            if reasons:
                violations.append((case, "; ".join(reasons), actual))

        control = workspace / "public.txt"
        control_result = results.get(control_id)
        if control_result is None:
            errors.append(f"{mode}: positive control has no tool result")
        elif not control_result.startswith("Successfully wrote "):
            errors.append(
                f"{mode}: positive control returned an unexpected result: "
                f"{control_result!r}"
            )
        try:
            control_bytes = control.read_bytes()
            if control_bytes != CONTROL_CONTENT.encode("utf-8"):
                errors.append(f"{mode}: positive control wrote unexpected bytes")
        except OSError as error:
            errors.append(f"{mode}: positive control could not be inspected: {error}")
    finally:
        for cleanup in (
            server.shutdown,
            server.server_close,
            lambda: thread.join(timeout=5),
        ):
            try:
                cleanup()
            except Exception as error:
                errors.append(
                    f"{mode}: fixture cleanup failed: "
                    f"{type(error).__name__}: {error}"
                )
    return cases, violations, errors


def main() -> int:
    try:
        executable = build_hach(Path(__file__).resolve().parents[4])
    except Exception as error:
        print(f"INFRASTRUCTURE ERROR: {error}", file=sys.stderr)
        return 2

    all_cases = []
    violations = []
    errors = []
    with tempfile.TemporaryDirectory(prefix="hach-path-denial-") as temp:
        temp_root = Path(temp)
        for mode in MODES:
            try:
                cases, mode_violations, mode_errors = run_mode(
                    executable, temp_root, mode
                )
                all_cases.extend(cases)
                violations.extend(mode_violations)
                errors.extend(mode_errors)
            except Exception as error:
                errors.append(
                    f"{mode}: verification setup failed: "
                    f"{type(error).__name__}: {error}"
                )

    if violations:
        print(
            f"FAIL: {len(violations)} of {len(all_cases)} denied-path calls "
            "were not denied."
        )
        for case, reason, actual in violations:
            print(f"VIOLATION {case.case_id}: {reason}; observed={actual!r}")
    if errors:
        for error in errors:
            print(f"INFRASTRUCTURE ERROR: {error}", file=sys.stderr)
        return 2
    if violations:
        return 1
    print(
        f"PASS: all {len(all_cases)} denied-path calls were denied and "
        "both positive controls wrote."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
