"""Public CLI regression for directory grep's early result limit."""
import ctypes
import http.server
import json
import os
from pathlib import Path
import resource
import select
import subprocess
import sys
import tempfile
import threading

binary = str(Path(sys.argv[1]).resolve())


class Server(http.server.ThreadingHTTPServer):
    count = 0
    messages = []
    tool = "grep_search"


class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        self.server.messages = request.get("messages", [])
        self.server.count += 1
        if self.server.count == 1:
            message = {
                "role": "assistant", "content": None,
                "tool_calls": [{
                    "id": "c1", "type": "function",
                    "function": {
                        "name": self.server.tool,
                        "arguments": json.dumps(
                            {"query": "needle", "path": "data"} if self.server.tool == "grep_search"
                            else {"pattern": "*", "path": "data"}
                        ),
                    },
                }],
            }
        else:
            message = {"role": "assistant", "content": "done"}
        body = json.dumps({"choices": [{"message": message, "finish_reason": "stop"}]}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


server = Server(("127.0.0.1", 0), Handler)
thread = threading.Thread(target=server.serve_forever, daemon=True)
thread.start()
try:
    with tempfile.TemporaryDirectory() as tmp:
        workspace = Path(tmp)
        data = workspace / "data"
        data.mkdir()
        for name in ("a", "b"):
            (data / name).touch()
        first, later = os.listdir(data)
        (data / first).write_text("needle\n" * 100)
        with (data / later).open("wb") as handle:
            handle.truncate(256 * 1024 * 1024)
        env = os.environ.copy()
        env.update(GHCRTS="-N2", HOME=str(workspace), CLAUDE_CONFIG_DIR=str(workspace))
        process = subprocess.run(
            [binary, "--provider", "openai-compatible", "--base-url",
             f"http://127.0.0.1:{server.server_port}/v1", "--model", "fixture",
             "--print", "find needle"],
            cwd=workspace, env=env, capture_output=True, text=True, timeout=30,
        )
        tool_messages = [m for m in server.messages if m.get("role") == "tool"]
        lines = tool_messages[0]["content"].splitlines() if tool_messages else []
        expected = [f"data/{first}:{i}: needle" for i in range(1, 101)]
        # macOS reports bytes; Linux reports KiB.
        rss = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
        if sys.platform != "darwin":
            rss *= 1024
        print(f"exit={process.returncode} requests={server.count} results={len(lines)} peak_rss={rss}")
        if process.returncode != 0 or server.count != 2 or lines != expected or rss >= 150 * 1024 * 1024:
            print(f"stdout={process.stdout!r} stderr={process.stderr!r}")
            sys.exit(1)

        # Linux inotify observes opening the later directory directly. Other
        # platforms still exercise grep and the shared traversal contract.
        if sys.platform.startswith("linux"):
            for path in data.iterdir():
                path.unlink()
            for i in range(200):
                (data / f"file-{i:03}").touch()
            names = os.listdir(data)
            later_dir = data / names[150]
            later_dir.unlink()
            later_dir.mkdir()
            names = os.listdir(data)
            assert names.index(later_dir.name) >= 100, names.index(later_dir.name)
            libc = ctypes.CDLL(None, use_errno=True)
            fd = libc.inotify_init1(os.O_NONBLOCK)
            if fd < 0:
                raise OSError(ctypes.get_errno(), "inotify_init1")
            try:
                if libc.inotify_add_watch(fd, os.fsencode(later_dir), 0x20) < 0:
                    raise OSError(ctypes.get_errno(), "inotify_add_watch")
                server.count = 0
                server.tool = "find_files"
                process = subprocess.run(
                    [binary, "--provider", "openai-compatible", "--base-url",
                     f"http://127.0.0.1:{server.server_port}/v1", "--model", "fixture",
                     "--print", "find files"],
                    cwd=workspace, env=env, capture_output=True, text=True, timeout=30,
                )
                tool_messages = [m for m in server.messages if m.get("role") == "tool"]
                lines = tool_messages[0]["content"].splitlines() if tool_messages else []
                visited = bool(select.select([fd], [], [], 0)[0])
                print(f"find exit={process.returncode} results={len(lines)} later_directory_opened={visited}")
                if (process.returncode != 0 or server.count != 2
                        or lines != [f"data/{name}" for name in names[:100]] or visited):
                    print(f"stdout={process.stdout!r} stderr={process.stderr!r}")
                    sys.exit(1)
            finally:
                os.close(fd)
finally:
    server.shutdown()
    server.server_close()
    thread.join()
