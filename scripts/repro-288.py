#!/usr/bin/env python3
"""Public CLI regression for #288; local model fixture, no credentials/billing.

Usage: python3 scripts/repro-288.py "$(cabal list-bin exe:hach)"
The fixture chooses calls, but only Hach can authorize/execute them, write the
marker, and decide the process exit/result. Both batched and separate turns run.
"""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class Model(BaseHTTPRequestHandler):
    def log_message(self, *_):
        pass

    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        completed = sum(m["role"] == "tool" for m in request["messages"])
        calls = self.server.calls[completed:]
        if not self.server.batched:
            calls = calls[:1]
        message = {"role": "assistant", "content": "DONE"}
        if calls:
            message.update(content=None, tool_calls=calls)
        data = json.dumps({"id": "fixture", "choices": [
            {"index": 0, "message": message, "finish_reason": "tool_calls" if calls else "stop"}
        ]}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)


def call(name, args):
    return {"id": name, "type": "function", "function": {
        "name": name, "arguments": json.dumps(args)
    }}


def run_case(binary, label, *, word="ok", batched=True, allow=True,
             mode="default", write=True, command=True, advice=None, hooks=None, deny=False):
    with tempfile.TemporaryDirectory(prefix="hach-288-", dir=os.environ.get("TMPDIR")) as tmp:
        root = Path(tmp)
        (root / ".claude").mkdir()
        rules = [{"action": "allow", "tool": "run_command"}] if allow else []
        if deny:
            rules = [{"action": "deny", "tool": "run_command"}]
        settings = {"permission_rules": rules}
        if hooks:
            settings["hooks"] = hooks
        (root / ".claude/settings.json").write_text(json.dumps(settings))
        server = ThreadingHTTPServer(("127.0.0.1", 0), Model)
        server.batched = batched
        server.calls = []
        if write:
            server.calls.append(call("write_file", {"path": "notes.txt", "content": "hi"}))
        if command:
            server.calls.append(call("run_command", {
                "command": 'printf "%s\\n" "' + word + '" > marker.txt; cat marker.txt'
            }))
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        env = {k: v for k, v in os.environ.items() if k not in ["OPENAI_API_KEY", "OPENROUTER_API_KEY"]}
        env.update(GHCRTS="-N2", HOME=tmp)
        try:
            result = subprocess.run([
                binary, "-p", "--output-format", "json", "--provider", "openai-compatible",
                "--base-url", f"http://127.0.0.1:{server.server_port}/v1", "--model", "fixture",
                "--permission-mode", mode, "--max-turns", "4",
                "Perform the requested tools in order, then say DONE."
            ], cwd=tmp, env=env, capture_output=True, text=True, timeout=30)
        finally:
            server.shutdown()
            server.server_close()
            thread.join()
        marker = (root / "marker.txt").read_text().strip() if (root / "marker.txt").exists() else None
        print(f"{label}: exit={result.returncode}, marker={marker!r}, stdout={result.stdout.strip()}")
        try:
            output = json.loads(result.stdout)
            if advice is None:
                assert result.returncode == 0 and output == {"answer": "DONE"}
                assert marker == word, "allowed command did not independently create the marker"
            else:
                assert result.returncode == 1
                assert output["error"].startswith("Task blocked:")
                assert f"--permission-mode {advice}" in output["error"]
                if advice == "dontAsk":
                    assert "acceptEdits" not in output["error"]
                assert marker is None, "denied/blocked command actually ran"
            assert not (root / "notes.txt").exists(), "headless write denial was bypassed"
        except (AssertionError, KeyError, ValueError) as error:
            raise AssertionError(f"{label}: {error}\nstderr: {result.stderr}") from error


def main():
    binary = str(Path(sys.argv[1]).resolve())
    for batched in [True, False]:
        for word in ["ok", "blocked", "denied", "permission denied"]:
            run_case(binary, f"{word!r}, batched={batched}", word=word, batched=batched)
    run_case(binary, "all denied, default", allow=False, advice="dontAsk")
    run_case(binary, "write denied, default", command=False, advice="acceptEdits")
    run_case(binary, "command denied, acceptEdits", write=False, allow=False, mode="acceptEdits", advice="dontAsk")
    # Policy and hook denials do not count as execution alongside a headless ask.
    # Their arbitrary diagnostic text must not be used to discover the outcome.
    run_case(binary, "command denied by rule", deny=True, advice="acceptEdits")
    hook = {"pre_tool_use": [{"handler": {"type": "command", "command": "echo 'arbitrary hook refusal'; exit 2"}, "matcher": "run_command"}]}
    run_case(binary, "command blocked by hook", hooks=hook, advice="acceptEdits")
    print("All public CLI permission-outcome regressions passed.")


if __name__ == "__main__":
    main()
