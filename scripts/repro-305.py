#!/usr/bin/env python3
"""Check both HTTP User-Agent headers against the public CLI version."""
import json
import os
import pty
import signal
import time
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer


class Fixture(BaseHTTPRequestHandler):
    def log_message(self, *_):
        pass

    def reply(self, value):
        body = json.dumps(value).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        self.server.headers_seen[self.path] = self.headers.get("User-Agent")
        self.reply({"data": []} if self.path == "/v1/models" else {"ok": True})

    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        done = any(m["role"] == "tool" for m in request["messages"])
        message = {"role": "assistant", "content": "DONE"}
        if not done:
            message.update(content=None, tool_calls=[{"id": "fetch", "type": "function", "function": {
                "name": "WebFetch", "arguments": json.dumps({"url": f"http://127.0.0.1:{self.server.server_port}/fetch"})}}])
        self.reply({"id": "fixture", "choices": [{"index": 0, "message": message,
                    "finish_reason": "stop" if done else "tool_calls"}]})


def main():
    binary = str(Path(sys.argv[1]).resolve())
    env = {k: v for k, v in os.environ.items() if k not in ("OPENAI_API_KEY", "OPENROUTER_API_KEY")}
    env.update(GHCRTS="-N2", OPENROUTER_API_KEY="fixture")
    version = subprocess.check_output([binary, "--version"], env=env, text=True).strip()
    expected = "hach/" + version.removeprefix("hach ")
    server = ThreadingHTTPServer(("127.0.0.1", 0), Fixture)
    server.headers_seen = {}
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        with tempfile.TemporaryDirectory(prefix="hach-305-", dir=os.environ.get("TMPDIR")) as tmp:
            env.update(HOME=tmp, XDG_CACHE_HOME=tmp, CLAUDE_CONFIG_DIR=tmp, TERM="xterm-256color")
            master, slave = pty.openpty()
            proc = subprocess.Popen([binary, "--provider", "openrouter", "--base-url",
                f"http://127.0.0.1:{server.server_port}/v1", "--model", "fixture"],
                cwd=tmp, env=env, stdin=slave, stdout=slave, stderr=slave, start_new_session=True)
            os.close(slave)
            try:
                for _ in range(100):
                    if "/v1/models" in server.headers_seen or proc.poll() is not None:
                        break
                    time.sleep(0.05)
            finally:
                if proc.poll() is None:
                    os.killpg(proc.pid, signal.SIGTERM)
                proc.wait(timeout=10)
                os.close(master)
            result = subprocess.run([binary, "-p", "--provider", "openrouter", "--base-url",
                f"http://127.0.0.1:{server.server_port}/v1", "--model", "fixture",
                "--permission-mode", "dontAsk", "--max-turns", "4", "Fetch the URL."],
                cwd=tmp, env=env, capture_output=True, text=True, timeout=30)
            assert result.returncode == 0, (result.stdout, result.stderr)
        errors = []
        for path in ("/v1/models", "/fetch"):
            actual = server.headers_seen.get(path)
            print(f"{path}: expected {expected}, got {actual}")
            if actual != expected:
                errors.append(path)
        assert not errors, f"User-Agent mismatch: {errors}"
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


if __name__ == "__main__":
    main()
