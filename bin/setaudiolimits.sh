#!/bin/sh
set -e
cat ~/dotfiles/etc/security/limits.d/audio.conf | tee /etc/security/limits.d/audio.conf
