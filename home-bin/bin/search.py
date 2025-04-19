#!/usr/bin/python3

import os
import sys

import subprocess as sp
from pathlib import Path

__doc__ = """search notes dirs for terms"""

home = os.environ["HOME"]

NOTES = Path(home).joinpath("Notes")
CHEATSHEETS = Path(home).joinpath("cheatsheets")
dirs = [NOTES, CHEATSHEETS]


def search(term):
    """Run a search for a term in the notes directory
    """
    global dirs 
    result = []
    for d in dirs:
        cmd = "git"
        args = ["grep", "-I", "-F", "-l", "-z", "--all-match", "--untracked"]
        for word in term:
            args.append("-e")
            args.append(word)
        args.append("--")
        if d.name == "Notes":
            args.append("*.org")
        if d.name == "cheatsheets":
            args.append("*.md")
        os.chdir(d)
        proc = sp.run([cmd, *args], stdout=sp.PIPE, stderr=sp.PIPE, text=True)
        _res = proc.stdout.split("\0")
        t = [Path(d).joinpath(r) for r in _res if r]
        result.extend(t)
    return result


if __name__ == '__main__':
    args = [a for a in sys.argv[1:]]
    results = search(args)
    for f in results:
        print(f)

