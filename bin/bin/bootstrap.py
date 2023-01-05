#!/usr/bin/python3
import os
from pathlib import Path

home = os.environ["HOME"]
home_path = Path(home)
local_sources = f"{home}/.local/src"
lsrc_path = Path(local_sources)

repositories = {
    # name: {url, version}
    "dotfiles": "https://github.com/adammccartney/dotfiles.git",
    "stow":  "https://github.com/aspiers/stow.git",
    "neovim": "https://github.com/neovim/neovim.git",
}

for k,v in repositories.items():
    if (k == "dotfiles"):
        os.system(f"git clone {v} {home_path}/{k}")
    os.system(f"git clone {v} {lsrc_path}/{k}")
