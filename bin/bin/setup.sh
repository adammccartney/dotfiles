#!/bin/sh

# This needs to be run with sudo

apt update
apt upgrade -y

apt install -y $(cat ../pkglist.txt)
