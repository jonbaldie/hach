#!/usr/bin/env python3
"""Reject release tags inconsistent with the Cabal package version."""
import re
import sys
from pathlib import Path

def main():
    tag = sys.argv[1]
    package = Path(sys.argv[2] if len(sys.argv) > 2 else "hach.cabal")
    versions = re.findall(r"(?mi)^version\s*:\s*(\S+)\s*$", package.read_text())
    if len(versions) != 1 or not re.fullmatch(r"\d+\.\d+\.\d+(?:\.\d+)?", versions[0]):
        sys.exit("::error::Expected one numeric Cabal package version")
    version = versions[0]
    allowed = {"v" + version}
    # Existing releases omit the fourth Cabal component when it is zero.
    if len(version.split(".")) == 4 and version.endswith(".0"):
        allowed.add("v" + version[:-2])
    if tag not in allowed:
        sys.exit(f"::error::Release tag {tag!r} does not match Cabal version {version}")
    print(f"Release tag {tag} matches Cabal version {version}")

if __name__ == "__main__":
    main()
