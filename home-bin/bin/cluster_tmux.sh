#!/bin/bash

export TMUX_TMPDIR=/home/"$USER"/shared_tmux
mkdir -p "$TMUX_TMPDIR"
rm -rf "$TMUX_TMPDIR"/tmux-*
tmux new -s it
