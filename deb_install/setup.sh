#!/usr/bin/env bash

# setup.sh: this is an initial setup script for a fresh debian install. The
# script needs to be run with sudo priveledges and will try to install the
# packages listed in pkglist.txt


function installed {
  cmd=$(command -v "${1}")

  [[ -n "${cmd}" ]] && [[ -f "${cmd}" ]]
  return ${?}
}

function die {
  >&2 echo "Fatal: $*"
  exit 1
}


[[ "${BASH_VERSINFO[0]}" -lt 4 ]] && die "Bash >=4 required"

deps=(curl nc dig)
for dep in "${deps[@]}"; do
  installed "${dep}" || die "Missing '${dep}'"
done


apt update 
apt upgrade -y 
apt-get install -y "$(cat ./pkglist.txt)"
