#!/usr/bin/env bash

green='\e[0;32m'
red='\e[0;31m'
nc='\e[0m' # No Color

function step {
  echo -e "${green}$1 ...${nc}"
  bash /dev/stdin || exit 1
}

function step_suppress {
  echo -ne "${green}$1 ... ${nc}"
  tmp=$(mktemp)
  bash /dev/stdin &> $tmp && echo -e "${green}Done${nc}" || (
    echo -e "${red}Failed${nc}"
    echo "Output: "
    cat $tmp
    exit 1
  )
}

step "Configuring project" << 'EOF'
  tmp=$(mktemp)
  cabal-$CABALVER configure --enable-tests --enable-benchmarks -v2 --ghc-options="-Wall -Werror" &> $tmp || (
    cat $tmp
    exit 1
  )
  echo "Using packages: "
  sed -nre 's/Dependency ([^ ]*) ==([^ :]*).*/\1 \2/p' $tmp | column -t | sed -e "s/^/  /"
EOF

step "Building project" << EOF
  cabal-$CABALVER build
EOF

step "Running tests" << EOF
  cabal-$CABALVER test
EOF

step "Creating source distribution" << EOF
  cabal-$CABALVER check
  cabal-$CABALVER sdist # tests that a source-distribution can be generated
EOF

step_suppress "Checking source distribution" << 'EOF'
  # The following scriptlet checks that the resulting source distribution can be built & installed
  SRC_TGZ=$(cabal-$CABALVER info . | awk '{print $2 ".tar.gz";exit}')
  cd dist/
  if [ -f "$SRC_TGZ" ]; then
    cabal-$CABALVER install --enable-tests --enable-benchmarks "$SRC_TGZ"
  else
    echo "expected '$SRC_TGZ' not found"
    exit 1
  fi    
EOF
