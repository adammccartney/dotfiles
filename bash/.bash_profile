# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi


if [ -d "$HOME/.guix-profile" ]; then
    GUIX_PROFILE="/home/amccartn/.guix-profile"
     . "$GUIX_PROFILE/etc/profile"
fi

if [ -f "$HOME/.local/bin/virtualenvwrapper.sh" ]; then
    . $HOME/.local/bin/virtualenvwrapper.sh
fi
