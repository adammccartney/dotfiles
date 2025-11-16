#!/bin/sh

# build deps
sudo dnf install gtk4-devel zig libadwaita-devel blueprint-compiler gettext


# get sources
VERSION=1.1.3
git clone https://github.com/ghostty-org/ghostty.git $HOME/.local/src/ghostty
pushd $HOME/.local/src/ghostty
git checkout tags/v1.1.3 -b v1.1.3
zig build -p $HOME/.local -Doptimize=ReleaseFast
popd

