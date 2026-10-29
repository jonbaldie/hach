#!/usr/bin/env python3
"""Public CLI exploratory driver. No third-party dependencies; never logs API keys."""
import argparse
import hashlib
import http.server
import threading
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[4]
EVIDENCE = Path(__file__).resolve().parent
BIN = ROOT / 'dist-newstyle/build/x86_64-linux/ghc-9.12.1/hach-0.1.16.0/x/hach/build/hach/hach'
ENV = os.environ.copy()
for key in ('HACH_PROVIDER', 'OPENAI_BASE_URL', 'OPENAI_MODEL', 'OPENAI_API_KEY', 'OPENROUTER_MODEL', 'OPENROUTER_API_KEY'):
    ENV.pop(key, None)
# Read credentials only; do not source arbitrary shell or log their values.
for line in ((ROOT / '.env').read_text().splitlines() if (ROOT / '.env').exists() else []):
    if line.startswith('OPENROUTER_API_KEY='):
        ENV['OPENROUTER_API_KEY'] = line.split('=', 1)[1].strip().strip('\"\'')
if os.environ.get('OPENROUTER_API_KEY'):
    ENV['OPENROUTER_API_KEY'] = os.environ['OPENROUTER_API_KEY']
ENV['GHCRTS'] = '-N2'
SANDBOX = Path(tempfile.mkdtemp(prefix='hach-explore-20261029-', dir=os.environ['TMPDIR']))
CONFIG = SANDBOX / 'config'
(CONFIG / '.claude').mkdir(parents=True)
(CONFIG / '.claude/settings.json').write_text('{"effort_level":"high"}\n')
ENV['CLAUDE_CONFIG_DIR'] = str(CONFIG)
(EVIDENCE / 'sandbox-path.txt').write_text(str(SANDBOX) + '\n')
LOG = EVIDENCE / 'journeys.jsonl'

def record(data):
    with LOG.open('a') as f:
        f.write(json.dumps(data, ensure_ascii=False) + '\n')
    print(json.dumps(data, ensure_ascii=False), flush=True)

def run(label, args, cwd, timeout=180, env=None, hach=True):
    command = ([str(BIN)] if hach else []) + args
    try:
        p = subprocess.run(command, cwd=cwd, env=env or ENV, input='', capture_output=True, text=True, errors='replace', timeout=timeout)
        data = dict(label=label, cwd=str(cwd), args=args, exit=p.returncode, stdout=p.stdout, stderr=p.stderr)
    except subprocess.TimeoutExpired as e:
        data = dict(label=label, cwd=str(cwd), args=args, timeout=timeout, stdout=str(e.stdout), stderr=str(e.stderr))
    record(data)
    return data


def coding():
    workspace = SANDBOX / 'coding'
    workspace.mkdir()
    (workspace / 'slugify.py').write_text('def slugify(text):\n    return text\n')
    (workspace / 'test_slugify.py').write_text('''import unittest
from slugify import slugify
class SlugTests(unittest.TestCase):
    def test_case_and_spaces(self): self.assertEqual(slugify(" Hello World "), "hello-world")
    def test_punctuation(self): self.assertEqual(slugify("One...two___three"), "one-two-three")
    def test_empty(self): self.assertEqual(slugify("---"), "")
    def test_digits(self): self.assertEqual(slugify("v2 Release 42"), "v2-release-42")
if __name__ == "__main__": unittest.main()
''')
    run('coding-baseline', ['python3', '-m', 'unittest', '-v'], workspace, hach=False)
    args = ['-p', '--output-format', 'json', '--model', 'openai/gpt-5.6-luna', '--permission-mode', 'acceptEdits', '--max-turns', '10', '--max-budget-usd', '1']
    run('coding-implement', args + ['Fix slugify.py to pass the existing tests. Lowercase ASCII text; replace each run of non-alphanumeric characters with one hyphen and strip leading/trailing hyphens. Read the tests, edit the file, and run python3 -m unittest -v. Do not edit the tests. Remember the project codeword is lantern-cedar-842 for later in this conversation.'], workspace)
    run('coding-independent-tests', ['python3', '-m', 'unittest', '-v'], workspace, hach=False)
    record(dict(label='coding-result-file', content=(workspace/'slugify.py').read_text()))
    run('coding-resume', args + ['--continue', 'Without reading any files or using tools, what was the project codeword in my previous message? Reply with only that codeword.'], workspace)
    retry_args = [arg for arg in args]
    retry_args[retry_args.index('acceptEdits')] = 'dontAsk'
    run('coding-permission-retry', retry_args + ['--continue', 'Now run python3 -m unittest -v to verify the existing slugify implementation. Do not change any files. Report the actual result.'], workspace)
    run('coding-final-independent-tests', ['python3', '-m', 'unittest', '-v'], workspace, hach=False)
    # Preserve generated artifacts for independent review, excluding credentials.
    artifact_dir = EVIDENCE / 'coding-artifacts'
    if artifact_dir.exists():
        shutil.rmtree(artifact_dir)
    for path in workspace.rglob('*'):
        if path.is_file() and path.suffix in ('.py', '.json', '.jsonl'):
            target = EVIDENCE / 'coding-artifacts' / path.relative_to(workspace)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, target)


def shell():
    workspace = SANDBOX / 'repo space café'
    workspace.mkdir()
    run('git-init', ['git', 'init', '-q'], workspace, hach=False)
    (workspace/'tracked.txt').write_text('original\n')
    run('git-add', ['git', 'add', 'tracked.txt'], workspace, hach=False)
    run('git-commit', ['git', '-c', 'user.name=Exploration', '-c', 'user.email=exploration@example.invalid', 'commit', '-qm', 'fixture'], workspace, hach=False)
    run('shell-streams', ['--exec', 'printf out; printf err >&2; exit 7'], workspace)
    run('shell-init', ['--init'], workspace)
    before = (workspace/'CLAUDE.md').read_bytes()
    run('shell-init-again', ['--init'], workspace)
    record(dict(label='shell-init-preserved', unchanged=before==(workspace/'CLAUDE.md').read_bytes()))
    run('worktree-create', ['-w', 'explore', '--exec', 'pwd; printf changed > tracked.txt; cat tracked.txt'], workspace)
    record(dict(label='worktree-root-file', content=(workspace/'tracked.txt').read_text()))
    run('worktree-reopen', ['-w', 'explore', '--exec', 'pwd; cat tracked.txt; git status --short'], workspace)
    run('worktree-invalid', ['-w', '../escape', '-p', '--output-format', 'json', 'hello'], workspace)
    run('worktree-list', ['git', 'worktree', 'list', '--porcelain'], workspace, hach=False)
    run('shell-binary', ['--exec', "printf '\\377\\376ok'"], workspace)


def reliability():
    workspace = SANDBOX / 'reliability'
    workspace.mkdir()
    state = {'status': 200, 'cost': 0.02, 'requests': []}
    class Handler(http.server.BaseHTTPRequestHandler):
        def log_message(self, *_): pass
        def do_POST(self):
            body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
            state['requests'].append(body)
            if state['status'] == 200:
                payload = {'id': 'exploration', 'choices': [{'index': 0, 'message': {'role': 'assistant', 'content': 'fixture response'}, 'finish_reason': 'stop'}], 'usage': {'prompt_tokens': 10, 'completion_tokens': 3, 'total_tokens': 13, 'cost': state['cost']}}
            else:
                payload = {'error': {'message': 'temporary fixture outage', 'code': state['status']}}
            encoded = json.dumps(payload).encode()
            self.send_response(state['status'])
            self.send_header('Content-Type', 'application/json')
            self.send_header('Content-Length', str(len(encoded)))
            self.end_headers()
            self.wfile.write(encoded)
    server = http.server.ThreadingHTTPServer(('127.0.0.1', 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    args = ['-p', '--output-format', 'json', '--provider', 'openai-compatible', '--base-url', f'http://127.0.0.1:{server.server_port}/v1', '--model', 'fixture-model']
    try:
        run('budget-zero', args + ['--max-budget-usd', '0', 'hello'], workspace)
        record(dict(label='budget-zero-request-count', count=len(state['requests'])))
        run('budget-positive-final-answer', args + ['--max-budget-usd', '0.01', 'hello'], workspace)
        record(dict(label='budget-positive-request-count', count=len(state['requests'])))
        state['status'] = 503
        run('api-outage', args + ['hello'], workspace)
        state['status'] = 200
        run('api-outage-retry', args + ['hello'], workspace)
        for attempt in range(2):
            ws = workspace / f'corrupt-{attempt}'
            ws.mkdir()
            run(f'corrupt-{attempt}-create', args + ['Remember an important project requirement: never delete customer data.'], ws)
            transcript = next((ws/'.agents/sessions').glob('*.jsonl'))
            original = transcript.read_text()
            # Deliberate local-disk fault injection: turn exactly one saved user-message line into malformed JSON.
            lines = original.splitlines()
            selected = next(i for i, line in enumerate(lines) if 'never delete customer data' in line)
            lines[selected] = '{CORRUPTED user message'
            damaged = '\n'.join(lines) + '\n'
            transcript.write_text(damaged)
            before_requests = len(state['requests'])
            run(f'corrupt-{attempt}-continue', args + ['--continue', 'Continue the project.'], ws)
            after = transcript.read_text()
            record(dict(label=f'corrupt-{attempt}-effects', original=original, damaged=damaged, after=after, overwritten=after!=damaged, requests_made=len(state['requests'])-before_requests, new_requests=state['requests'][before_requests:]))
    finally:
        server.shutdown()
        server.server_close()
        thread.join()
        record(dict(label='api-fixture-stopped', requests=len(state['requests'])))


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('journey', choices=['coding', 'shell', 'reliability'])
    args = parser.parse_args()
    record(dict(label='run-start', sandbox=str(SANDBOX), binary=str(BIN), sha256=hashlib.sha256(BIN.read_bytes()).hexdigest()))
    try:
        globals()[args.journey]()
    finally:
        shutil.rmtree(SANDBOX)
        record(dict(label='cleanup', sandbox_removed=not SANDBOX.exists(), docker_resources_created=[]))
