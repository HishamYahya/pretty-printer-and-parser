#!/bin/bash

set -e

sudo apt update
sudo apt install cabal-install

cabal update
cabal install --only-dependencies
cabal build
cabal run
