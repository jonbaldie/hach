#!/usr/bin/env python3
"""Deterministic public-CLI regression fixture for issue #299.

Usage:
    python3 scripts/repro-299.py PATH-TO-HACH
    python3 scripts/repro-299.py --help

The loopback OpenAI-compatible fixture drives the public CLI through
TaskCreate -> run_command (waiting for the producer's ``done`` marker) ->
Monitor -> final response.  The producer flushes a fixed-width workload and
writes ``done``; that marker, rather than wall time or visibility of a final
line, is the workload-completion signal.  Provider request tool names and
selected tool-result fields are inspected, but request bodies and CLI output
are never printed (credentials therefore cannot enter diagnostics).  The
child environment removes provider credential variables and uses GHCRTS
``-N2 -s``.  RTS heap allocation is the deterministic pass/fail signal; wall
time is retained only as a comparison.

The verbose scaling cases use the same 4,000 -> 8,000-line, 100-byte-line
workloads.  ``Monitor`` is a public response capped by
``truncateToolOutput`` at 1,000 lines/30,000 characters, so it must contain
the expected start and the existing truncation notice, but it cannot expose a
``FINAL-*`` marker written after 4,000 or 8,000 lines.  The small-output case
separately verifies that the actual ``issue-299-3`` Monitor response contains
``FINAL-*`` and that the marker is not incorrectly accepted from the
``issue-299-1`` TaskCreate response.  The Haskell small-output task regression
tests cover final output; this fixture only checks the public protocol.

Recorded evidence (macOS/aarch64, GHC 9.12.1, -N2; final verification
appended to the issue run log): the small case (3 lines) allocated 14,524,728
bytes in 0.089418 seconds and observed FINAL-3 only in Monitor; the 4,000-line
case allocated 22,016,168 bytes in 0.104523 seconds; and the 8,000-line case
allocated 29,369,520 bytes in 0.115023 seconds.  Thus the measured 4,000 ->
8,000 ratios are exactly 1.334x allocation and 1.100x wall.  The gate is
``allocation < 2.75x``; it rejects the old 3.94x behavior while allowing
modest RTS noise.  This is a regression detector, not a benchmark: absolute
wall results vary by host, RTS version, scheduler and background load, and
allocation includes CLI/provider-fixture overhead. A changed GHC/architecture
may need a new baseline, but must not weaken the assertion silently.
"""

import argparse
import http.server
import json
import math
import os
from pathlib import Path
import shlex
import re
import subprocess
import sys
import tempfile
import threading
import time


SIZES = (4_000, 8_000)
SMALL_LINES = 3
LINE_BYTES = 101  # 100 x characters and a newline
MAX_ALLOC_RATIO = 2.75
TIMEOUT_SECONDS = 30
ALLOC_RE = re.compile(r"([0-9,]+) bytes allocated in the heap")
TRUNCATION_RE = re.compile(r"\[Output truncated: showing [0-9]+ lines / 30000 characters\]")


class Fixture(http.server.ThreadingHTTPServer):
    allow_reuse_address = True

    def __init__(self, address, handler, producer_command, expected_final, task_name):
        super().__init__(address, handler)
        self.producer_command = producer_command
        self.expected_final = expected_final
        self.task_name = task_name
        self.requests = []
        self.served_tools = []
        self.errors = []
        self.lock = threading.Lock()


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *_):
        # Never echo request headers or bodies into preserved diagnostics.
        pass

    def reply(self, value):
        body = json.dumps(value, separators=(",", ":")).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        try:
            length = int(self.headers.get("Content-Length", "0"))
            request = json.loads(self.rfile.read(length))
        except Exception as error:
            self.server.errors.append(f"invalid request: {type(error).__name__}")
            self.send_error(400, "invalid fixture request")
            return

        with self.server.lock:
            self.server.requests.append(request)
            count = len(self.server.requests)

        if count == 1:
            name = "TaskCreate"
            arguments = {"name": self.server.task_name, "command": self.server.producer_command}
        elif count == 2:
            name = "run_command"
            # This is marker polling, not a timing assumption: Monitor below
            # is the final drain assertion.
            arguments = {
                "command": "until test -f done; do sleep 0.01; done",
                "timeout": TIMEOUT_SECONDS,
            }
        elif count == 3:
            name = "Monitor"
            arguments = {"task_id": "bg-1"}
        elif count == 4:
            name = None
            arguments = None
        else:
            self.server.errors.append(f"unexpected provider request #{count}")
            name = None
            arguments = None

        if name is None:
            message = {"role": "assistant", "content": "finished"}
            finish_reason = "stop"
        else:
            with self.server.lock:
                self.server.served_tools.append(name)
            message = {
                "role": "assistant",
                "content": None,
                "tool_calls": [{
                    "id": f"issue-299-{count}",
                    "type": "function",
                    "function": {"name": name, "arguments": json.dumps(arguments)},
                }],
            }
            finish_reason = "tool_calls"
        self.reply({"id": "issue-299-fixture", "choices": [{
            "index": 0, "message": message, "finish_reason": finish_reason,
        }]})


def child_environment(workspace):
    # The endpoint is local and unauthenticated.  Do not pass user secrets to
    # the child, even if a developer has configured a real provider globally.
    env = {
        key: value for key, value in os.environ.items()
        if not key.startswith(("OPENAI_", "OPENROUTER_", "ANTHROPIC_"))
    }
    env.update({
        "HOME": str(workspace),
        "CLAUDE_CONFIG_DIR": str(workspace / ".claude"),
        "GHCRTS": "-N2 -s",
        "NO_PROXY": "*",
        "no_proxy": "*",
    })
    return env


def producer_command(workspace, lines):
    final = f"FINAL-{lines}"
    # Construct the full marker at runtime, keeping it out of the TaskCreate
    # response (which echoes the command).  Only Monitor's result may establish
    # final_seen.
    code = (
        "import sys; "
        f"[sys.stdout.write('x'*100+'\\n') for _ in range({lines})]; "
        "sys.stdout.write('FINAL-' + str(" + str(lines) + ") + '\\n'); "
        "sys.stdout.flush(); open('done', 'w').write('ok')"
    )
    # Include a different FINAL-looking decoy in the TaskCreate command/result.
    # It proves assertions are scoped to Monitor's issue-299-3 result rather
    # than accepting any marker-shaped text from TaskCreate.
    decoy = f"FINAL-DECOY-{lines}"
    return f"cd {shlex.quote(str(workspace))}; {shlex.quote(sys.executable)} -c {shlex.quote(code)} # {decoy}"


def tool_call(request):
    # Compatible-provider request bodies put prior assistant tool calls in the
    # message history, not necessarily in the final message.  Find the latest
    # function call without printing the request (which could contain secrets).
    found = []
    def visit(value):
        if isinstance(value, dict):
            function = value.get("function")
            if isinstance(function, dict) and "name" in function and "arguments" in function:
                try:
                    found.append((function["name"], json.loads(function["arguments"])))
                except (TypeError, ValueError):
                    pass
            for child in value.values():
                visit(child)
        elif isinstance(value, list):
            for child in value:
                visit(child)
    visit(request)
    return found[-1] if found else (None, None)


def inspect_requests(server, expected_final):
    # Provider requests are public-interface evidence: inspect only the known
    # tool names/arguments and selected tool results.  Do not print bodies:
    # they can contain arbitrary user/provider data.
    expected = ["TaskCreate", "run_command", "Monitor"]
    actual = []
    argument_errors = []
    tool_results = {}
    for request in server.requests:
        name, arguments = tool_call(request)
        if name is not None:
            actual.append(name)
            if name == "TaskCreate" and ("command" not in (arguments or {}) or not (arguments or {}).get("name", "").startswith("issue-299-")):
                argument_errors.append("TaskCreate arguments")
            if name == "run_command" and "done" not in (arguments or {}).get("command", ""):
                argument_errors.append("run_command marker")
            if name == "Monitor" and (arguments or {}).get("task_id") != "bg-1":
                argument_errors.append("Monitor task id")
        for message in request.get("messages", []):
            if isinstance(message, dict) and message.get("role") == "tool":
                call_id = message.get("tool_call_id")
                content = message.get("content")
                if isinstance(call_id, str) and isinstance(content, str):
                    tool_results.setdefault(call_id, []).append(content)

    monitor_contents = tool_results.get("issue-299-3", [])
    taskcreate_contents = tool_results.get("issue-299-1", [])
    monitor_text = "\n".join(monitor_contents)
    taskcreate_text = "\n".join(taskcreate_contents)
    evidence = {
        "monitor_start": monitor_text.startswith("Task bg-1 output:\n" + "x" * 100),
        "monitor_truncated": bool(TRUNCATION_RE.search(monitor_text)),
        "monitor_final": expected_final in monitor_text,
        "taskcreate_final": expected_final in taskcreate_text,
        "taskcreate_decoy": "FINAL-DECOY-" in taskcreate_text,
    }
    return actual == expected and not argument_errors, actual, argument_errors, evidence


def run_case(binary, lines, small=False):
    with tempfile.TemporaryDirectory(prefix="hach-299-", dir=os.environ.get("TMPDIR")) as name:
        workspace = Path(name)
        expected_final = f"FINAL-{lines}"
        task_name = "issue-299-small" if small else "issue-299-verbose"
        server = Fixture(("127.0.0.1", 0), Handler, producer_command(workspace, lines), expected_final, task_name)
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        args = [
            binary, "--provider", "openai-compatible",
            "--base-url", f"http://127.0.0.1:{server.server_port}/v1",
            "--model", "fixture", "--print", "--permission-mode", "bypassPermissions",
            "run issue-299 benchmark",
        ]
        started = time.monotonic()
        try:
            result = subprocess.run(
                args, cwd=workspace, env=child_environment(workspace),
                capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
            )
            exception = None
        except Exception as error:
            result = None
            exception = type(error).__name__
        elapsed = time.monotonic() - started
        try:
            server.shutdown()
            server.server_close()
            thread.join(timeout=5)
        finally:
            pass

        marker = (workspace / "done").read_text() if (workspace / "done").exists() else None
        stderr = result.stderr if result is not None else ""
        allocation_match = ALLOC_RE.search(stderr)
        allocated = int(allocation_match.group(1).replace(",", "")) if allocation_match else None
        requests_ok, request_tools, request_errors, evidence = inspect_requests(server, expected_final)
        # Completion uses the producer's marker; output assertions use the
        # public tool result attached to the relevant provider call id.
        print(
            f"N={lines} bytes={lines * LINE_BYTES} seconds={elapsed:.6f} "
            f"allocated={allocated} exit={result.returncode if result else 'exception'} "
            f"requests={len(server.requests)} tools={request_tools} marker={marker!r} "
            f"monitor_start={evidence['monitor_start']} monitor_truncated={evidence['monitor_truncated']} "
            f"monitor_final={evidence['monitor_final']} taskcreate_final={evidence['taskcreate_final']} "
            f"taskcreate_decoy={evidence['taskcreate_decoy']}", flush=True,
        )
        return {
            "lines": lines, "bytes": lines * LINE_BYTES, "seconds": elapsed,
            "allocated": allocated, "result": result, "exception": exception,
            "requests_ok": requests_ok, "request_tools": request_tools,
            "request_errors": request_errors, "errors": list(server.errors),
            "marker": marker, "evidence": evidence, "small": small,
        }


def fail(message, measurements):
    print(f"FAIL: {message}", file=sys.stderr, flush=True)
    for item in measurements:
        if item["errors"]:
            print(f"fixture_errors[{item['lines']}]: {item['errors']}", file=sys.stderr)
        if item["request_errors"]:
            print(f"request_errors[{item['lines']}]: {item['request_errors']}", file=sys.stderr)
        if item.get("evidence"):
            print(f"evidence[{item['lines']}]: {item['evidence']}", file=sys.stderr)
        if item["exception"]:
            print(f"exception[{item['lines']}]: {item['exception']}", file=sys.stderr)
    return 1


def main(argv=None):
    parser = argparse.ArgumentParser(
        prog=Path(sys.argv[0]).name,
        description="Run issue #299's public CLI RTS-allocation regression fixture.",
        epilog="Pass PATH-TO-HACH as the only positional argument; no provider credentials are used.",
    )
    parser.add_argument("binary", metavar="PATH-TO-HACH")
    args = parser.parse_args(argv)
    binary = str(Path(args.binary).resolve())
    if not Path(binary).is_file() or not os.access(binary, os.X_OK):
        parser.error(f"executable is not runnable: {binary}")

    small = run_case(binary, SMALL_LINES, small=True)
    if (
        small["result"] is None or small["result"].returncode != 0
        or not small["requests_ok"] or small["errors"] or small["marker"] != "ok"
        or not small["evidence"]["monitor_start"]
        or not small["evidence"]["monitor_final"]
        or small["evidence"]["taskcreate_final"]
        or not small["evidence"]["taskcreate_decoy"]
    ):
        return fail("small-output public Monitor contract failed", [small])

    measurements = [run_case(binary, lines) for lines in SIZES]
    for item in measurements:
        result = item["result"]
        if (
            result is None or result.returncode != 0 or item["allocated"] is None
            or item["requests_ok"] is False or item["errors"]
            or item["marker"] != "ok"
            or not item["evidence"]["monitor_start"]
            or not item["evidence"]["monitor_truncated"]
            or item["evidence"]["monitor_final"]
        ):
            return fail(f"CLI/fixture/Monitor contract failed for N={item['lines']}", [small] + measurements)

    times = [item["seconds"] for item in measurements]
    allocations = [item["allocated"] for item in measurements]
    wall_ratio = times[-1] / times[-2] if times[-2] else math.inf
    allocation_ratio = allocations[-1] / allocations[-2] if allocations[-2] else math.inf
    print(f"final wall doubling ratio (N={SIZES[-2]} -> {SIZES[-1]}): {wall_ratio:.3f}x", flush=True)
    print(f"final RTS allocation doubling ratio (N={SIZES[-2]} -> {SIZES[-1]}): {allocation_ratio:.3f}x", flush=True)
    if allocation_ratio >= MAX_ALLOC_RATIO:
        print(
            "BUG: public CLI reproduction shows sharply superlinear allocation "
            f"({allocation_ratio:.3f}x for a 2x workload; wall {wall_ratio:.3f}x).",
            file=sys.stderr, flush=True,
        )
        return 1
    print(
        "PASS: RTS allocation scales acceptably "
        f"({allocation_ratio:.3f}x; wall comparison {wall_ratio:.3f}x).", flush=True,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
