#!/usr/bin/env python3
"""Replay the release workflow's Homebrew step without publishing anything."""
import os
from pathlib import Path
import subprocess
import tempfile
import textwrap
import unittest

ROOT = Path(__file__).resolve().parent.parent


def release_script():
    workflow = (ROOT / '.github/workflows/release.yml').read_text()
    step = workflow.split('      - name: Render and Publish Homebrew Formula\n', 1)[1]
    script = step.split('        run: |\n', 1)[1]
    return textwrap.dedent(script).replace('${{ github.ref_name }}', 'v1.2.3')


class ReleaseHomebrewTest(unittest.TestCase):
    def run_step(self, token, push_status='0'):
        with tempfile.TemporaryDirectory(prefix='hach-release-test-') as directory:
            root = Path(directory)
            (root / 'dist').mkdir()
            (root / 'dist/checksums.txt').write_text(''.join(
                f'{digit * 64}  hach_1.2.3_{platform}.tar.gz\n'
                for digit, platform in [('a', 'darwin_arm64'), ('b', 'darwin_amd64'), ('c', 'linux_amd64')]
            ))
            (root / 'bin').mkdir()
            fake_git = root / 'bin/git'
            fake_git.write_text('''#!/bin/bash
set -eu
printf '%s\\n' "$1" >> "$GIT_CALLS"
case "$1" in
  clone) mkdir -p tap_repo ;;
  diff) exit 1 ;;
  push) exit "$PUSH_STATUS" ;;
esac
''')
            fake_git.chmod(0o755)
            env = dict(os.environ, TAP_TOKEN=token, PUSH_STATUS=push_status,
                       GIT_CALLS=str(root / 'calls'), PATH=f"{root / 'bin'}:{os.environ['PATH']}")
            result = subprocess.run(['bash', '-c', release_script()], cwd=root, env=env,
                                    text=True, capture_output=True)
            calls = (root / 'calls').read_text().splitlines() if (root / 'calls').exists() else []
            formula = (root / 'Formula_hach.rb').read_text()
            return result, calls, formula

    def test_missing_token_fails_after_printing_formula(self):
        result, calls, formula = self.run_step('')
        self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn('::error::', result.stdout + result.stderr)
        self.assertIn('HOMEBREW_TAP_TOKEN', result.stdout + result.stderr)
        self.assertIn(formula, result.stdout)
        self.assertEqual(calls, [])

    def test_token_publishes_and_push_failure_fails(self):
        result, calls, formula = self.run_step('dummy-token')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn(formula, result.stdout)
        self.assertIn('clone', calls)
        self.assertIn('push', calls)
        failed, calls, _ = self.run_step('dummy-token', '42')
        self.assertEqual(failed.returncode, 42, failed.stdout + failed.stderr)
        self.assertIn('push', calls)


if __name__ == '__main__':
    unittest.main()
