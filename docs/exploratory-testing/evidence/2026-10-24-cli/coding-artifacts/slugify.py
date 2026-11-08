import re


def slugify(text):
    """Convert text to a lowercase ASCII slug."""
    return re.sub(r"[^a-z0-9]+", "-", text.lower()).strip("-")
