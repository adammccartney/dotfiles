#!/bin/sh

# Download the guix installer script from the git repo at commit
# then run it like a wild horse

function download_installer_script () {
    local guix_commit_sha1=$1
    rm -vf /tmp/guix-install.sh
    curl -l -f -o /tmp/guix-install.sh "https://codeberg.org/group/guix/raw/commit/${guix_commit_sha1}/etc/guix-install.sh"
    chmod +x /tmp/guix-install.sh
}

function run_installer () {
    /tmp/guix-install.sh
}

# main
GUIX_COMMIT_SHA1="ce44a0922979d2ade902638afb9b0d28b160def0"
download_installer_script ${1:-$GUIX_COMMIT_SHA1}
run_installer
