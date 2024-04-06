#!/bin/sh
#
# boostrap.sh: script to help with testing out guix home configuration locally

rm_emacs_packages () {
    rm -rf emacs/.emacs.d/{quelpa,elpa}
}

set -xe
rm_emacs_packages
