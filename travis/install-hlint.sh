#!/usr/bin/env bash

set -e

if [ ! -d $HOME/hlint/bin ]; then
    mkdir -p $HOME/hlint/bin
    cabal-$CABALVER install hlint -j
    cp ~/.cabal/bin/hlint $HOME/hlint/bin
    rm -rf ~/.ghc
fi
