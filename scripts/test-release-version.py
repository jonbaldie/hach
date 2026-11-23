#!/usr/bin/env python3
"""Exercise release tag validation without creating a release."""
from pathlib import Path
import subprocess
import sys
import tempfile
import textwrap
import os
import shutil
import unittest

ROOT = Path(__file__).resolve().parent.parent
CHECK = ROOT / "scripts/check-release-version.py"


class ReleaseVersionTest(unittest.TestCase):
    def check(self, tag, version="0.1.17.0"):
        with tempfile.TemporaryDirectory() as tmp:
            package = Path(tmp) / "hach.cabal"
            package.write_text(f"name: hach\nversion: {version}\n")
            return subprocess.run([sys.executable, str(CHECK), tag, str(package)],
                                  text=True, capture_output=True)

    def test_workflow_gate(self):
        workflow = (ROOT / ".github/workflows/release.yml").read_text()
        step = workflow.split("      - name: Validate release tag\n", 1)[1]
        command = textwrap.dedent(step.split("        run: |\n", 1)[1].split("\n      - name:", 1)[0])
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "scripts").mkdir()
            shutil.copy(CHECK, root / "scripts")
            (root / "hach.cabal").write_text("name: hach\nversion: 0.1.17.0\n")
            for tag, ref_type, expected in (("v0.1.17", "tag", 0), ("v0.1.18", "tag", 1),
                                             ("v0.1.17", "branch", 1)):
                env = dict(os.environ, RELEASE_TAG=tag, RELEASE_REF_TYPE=ref_type)
                result = subprocess.run(["bash", "-e", "-c", command], cwd=root, env=env,
                                        text=True, capture_output=True)
                self.assertEqual(result.returncode, expected, result.stdout + result.stderr)

    def test_matching_tags(self):
        for tag in ("v0.1.17", "v0.1.17.0"):
            with self.subTest(tag=tag):
                result = self.check(tag)
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_bad_tags_fail(self):
        for tag in ("v0.1.18", "v0.1.17.1", "main", "0.1.17", "v0.1.17-rc1"):
            with self.subTest(tag=tag):
                result = self.check(tag)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("error", result.stderr.lower())

    def test_nonzero_fourth_component_is_not_ignored(self):
        self.assertNotEqual(self.check("v0.1.17", "0.1.17.1").returncode, 0)
        self.assertEqual(self.check("v0.1.17.1", "0.1.17.1").returncode, 0)


if __name__ == "__main__":
    unittest.main()
