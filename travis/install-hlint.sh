#!/usr/bin/env bash

set -e

if [ ! -d $HOME/hlint/bin ]; then
    mkdir -p $HOME/hlint
    cd $HOME/hlint
    cabal-$CABALVER sandbox init
    cabal-$CABALVER install hlint -j
    ln -s $PWD/.cabal-sandbox/bin $PWD/bin
    cd $TRAVIS_BUILD_DIR
fi
