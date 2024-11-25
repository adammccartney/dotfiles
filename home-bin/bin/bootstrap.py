#!/usr/bin/python3
import os
import subprocess as subp
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

releases = {
        "lua-language-server":
        "https://github.com/LuaLS/lua-language-server/releases/download/3.6.18/lua-language-server-3.6.18-linux-x64.tar.gz"
}


def clone_repos():
    for k,v in repositories.items():
        if (k == "dotfiles"):
            os.system(f"git clone {v} {home_path}/{k}")
        os.system(f"git clone {v} {lsrc_path}/{k}")


def untar_release(archive):
    "Upack the archive"
    cmd = f"tar -zxf {archive}"
    subp.run(cmd, shell=True)
    return str(archive)


def mv_contents(basename):
    "Move the contents into the dir of callsite"
    cmd = f"mv {basename}*/* ."
    subp.run(cmd, shell=True)

def cleanup(basename):
    Path(basename).unlink()

def get_releases(releases):
    for k,v in releases.items():
        target_dir = f"{home_path}/.local/src/{k}"
        if not Path(target_dir).exists():
            Path(target_dir).mkdir()
            os.chdir(target_dir)
            cmd = f"curl -LJO {v}" 
            subp.run(cmd, shell=True)
            files = [p for p in Path('.').iterdir() if p.is_file()]
            archive = files[0]
            archive = archive.absolute()
            untar_release(archive)
            mv_contents(k) # assume the archive name contains key


if __name__ == '__main__':
    clone_repos()
    get_releases(releases)
