#!/bin/sh

# The following will download and compile from source the latest version of aerc

## Deps
git clone https://git.sr.ht/~sircmpwn/scdoc $HOME/.local/src/scdoc
pushd $HOME/.local/src/scdoc
PREFIX=$HOME/.local make && make install
popd

## aerc
VERSION=0.20.1
git clone https://git.sr.ht/~rjarry/aerc $HOME/.local/src/aerc
pushd $HOME/.local/src/aerc
git checkout tags/"$VERSION" -b "$VERSION"
make && sudo make install
popd

