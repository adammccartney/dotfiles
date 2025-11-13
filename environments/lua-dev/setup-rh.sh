#!/bin/sh

TARGET_USER="${TARGET_USER:-$USER}"
echo "$TARGET_USER"

if [ "$TARGET_USER" == "root" ]; then
    _home=/"$TARGET_USER"
else
    _home=/home/"$TARGET_USER"
fi


## Download source, build Lua language server
#
## deps
sudo dnf install ninja-build g++ libstdc++-static

# Repos
REPO_LUA_LS="https://github.com/LuaLS/lua-language-server"

LUA_LS_CHECKOUT="$_home"/adam/lua-language-server
mkdir -p "$LUA_LS_CHECKOUT"

git clone "$REPO_LUA_LS" "$LUA_LS_CHECKOUT"
pushd "$LUA_LS_CHECKOUT"
./make.sh
popd

ls -l "$LUA_LS_CHECKOUT"/bin/lua-language-server

