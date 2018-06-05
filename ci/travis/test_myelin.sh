#!/bin/bash
##
## A script to build and test the myelin source code
##

sudo apt-get -qq update
sudo apt-get install -y libblas-dev liblapack-dev

# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH="$HOME/.local/bin:$PATH"
curl -L "https://www.stackage.org/stack/linux-x86_64" | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
stack --no-terminal --install-ghc test
