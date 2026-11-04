#!/usr/bin/env python3
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
        results = [m for m in request["messages"] if m["role"] == "tool"]
        if results:
            self.server.results.extend(results)
            message = {"role": "assistant", "content": "DONE"}
        else:
            message = {"role": "assistant", "content": None, "tool_calls": self.server.calls}
        data = json.dumps({"id": "fixture", "choices": [{"index": 0, "message": message,
            "finish_reason": "stop" if results else "tool_calls"}]}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)


def call(name, args):
    return {"id": name, "type": "function", "function": {
        "name": name, "arguments": json.dumps(args)}}


def run(binary, mode, interaction, write=False):
    with tempfile.TemporaryDirectory(prefix="hach-191-") as tmp:
        server = ThreadingHTTPServer(("127.0.0.1", 0), Model)
        server.results = []
        server.calls = [interaction]
        if write:
            server.calls.append(call("write_file", {"path": "notes.txt", "content": "hello"}))
        thread = threading.Thread(target=server.serve_forever, daemon=True)
        thread.start()
        env = {k: v for k, v in os.environ.items() if k not in ("OPENAI_API_KEY", "OPENROUTER_API_KEY")}
        env.update(HOME=tmp, GHCRTS="-N2")
        try:
            result = subprocess.run([
                binary, "-p", "--provider", "openai-compatible", "--base-url",
                f"http://127.0.0.1:{server.server_port}/v1", "--model", "fixture",
                "--permission-mode", mode, "--max-turns", "4", "Use the requested tools."
            ], cwd=tmp, env=env, capture_output=True, text=True, timeout=30)
        finally:
            server.shutdown()
            server.server_close()
            thread.join()
        contents = [item["content"] for item in server.results]
        print(f"{mode}, {interaction['id']}, write={write}: exit={result.returncode}, tool results={contents}", flush=True)
        if write:
            assert result.returncode == 1, result.stdout
            assert "Task blocked:" in result.stdout, result.stdout
            assert len(contents) == 2 and "No interactive approval available" in contents[1], contents
            assert not (Path(tmp) / "notes.txt").exists()
        else:
            assert result.returncode == 0, result.stderr
        expected = ("Message sent to agent agent-1: hello" if interaction["id"] == "SendMessage"
                    else "Prompted user: hello")
        assert contents[0] == expected, contents


if __name__ == "__main__":
    binary = str(Path(sys.argv[1]).resolve())
    send = call("SendMessage", {"agent_id": "agent-1", "message": "hello"})
    for mode in ("default", "acceptEdits", "plan"):
        run(binary, mode, send)
    run(binary, "default", call("AskUserQuestion", {"question": "hello"}), write=True)
    run(binary, "default", send, write=True)
    print("All interaction permission CLI regressions passed.")
