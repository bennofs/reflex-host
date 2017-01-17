#!/usr/bin/env bash

set -e

export green='\e[0;32m'
export red='\e[0;31m'
export nc='\e[0m' # No Color

function begin_steps {
  export STEP_FAILED=0
}

function end_steps {
  return $STEP_FAILED
}

function step {
  local result=0
  echo -e "${green}$1 ...${nc}"
  bash -e /dev/stdin || result=1
  export STEP_FAILED=$(( $STEP_FAILED || $result ))
  return $result
}

function step_suppress {
  echo -ne "${green}$1 ... ${nc}"
  tmp=$(mktemp)
  if ! ( bash -e /dev/stdin &> $tmp && echo -e "${green}Done${nc}" && rm -f $tmp ); then
    echo -e "${red}Failed${nc}"
    echo "Output: "
    cat $tmp
    rm -f $tmp
    export STEP_FAILED=1
    return 1
  fi
}

export -f step step_suppress begin_steps end_steps

unset CC
export PATH=$HOME/tools/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:$PATH
if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ]; then
  gunzip $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz
fi
travis_retry cabal update

# travis docker containers report incorrect number of cores
sed -i 's/^jobs:.*$/jobs: 2/' $HOME/.cabal/config

set +e
