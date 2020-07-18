#!/bin/sh

# A script that runs a script...
# Fetches all binaries for building YouCompleteMe with vim
# This nees to be run with sudo

bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"

