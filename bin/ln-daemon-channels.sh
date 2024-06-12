#!/bin/sh

# this file links the $HOME/.config/guix/channels.scm for a user profile to the config used by the daemon

DAEMON_CHANNELS="/root/.config/guix/channels.scm"

if sudo test -L "$DAEMON_CHANNELS"; then
    sudo unlink "$DAEMON_CHANNELS"
fi
sudo ln -vs $HOME/.config/guix/channels.scm /root/.config/guix/channels.scm
