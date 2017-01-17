#!/usr/bin/env bash

begin_steps

if [ -n "$ROOT" ]; then
  step "Checking style with HLint" << EOF
    hlint --cpp-simple src
    if [ -d tests ]; then
      hlint --cpp-simple tests
    fi
EOF
fi

set -e
step "Configuring project" << 'EOF'
  cabal configure --enable-tests --enable-benchmarks -v2 --ghc-options="-Wall -Werror -ddump-minimal-imports" &> cabal.log || (
    cat cabal.log
    exit 1
  )
  echo "Using packages: "
  sed -nre 's/Dependency ([^ ]*) ==([^ :]*).*/\1 \2/p' cabal.log | column -t | sed -e "s/^/  /"
  echo "Flags chosen: "
  sed -nr -e ':x; /\,$/ { N; s/,\n/,/; tx }' -e 's/Flags chosen: (.*)/\1/' -e 's/, /,/gp' cabal.log | tr ',' '\n'
EOF

step "Building project" << EOF
  cabal build
EOF

set +e
if [ -n "$ROOT" ]; then
  step_suppress "Checking for unused dependencies" << EOF
    packunused --ignore-package base
EOF
fi
set -e

step "Running tests" << EOF
  cabal test
EOF

step "Creating source distribution" << EOF
  cabal check
  cabal sdist # tests that a source-distribution can be generated
EOF

pkgid=$(cabal info . | awk '{print $2; exit}')

step_suppress "Checking source distribution" << EOF
  # The following scriptlet checks that the resulting source distribution can be built & installed
  SRC_TGZ="dist/$pkgid.tar.gz"
  if [ -f "\$SRC_TGZ" ]; then
    cabal install --enable-tests --enable-benchmarks "\$SRC_TGZ"
  else
    echo "expected '\$SRC_TGZ' not found"
    exit 1
  fi
EOF

if [ -n "$ROOT" -a -n "$HACKAGE_AUTH" ]; then
  step "Uploading package candidate" << EOF
    curl https://hackage.haskell.org/packages/candidates \
      -H 'Accept: text/plain' \
      -u '$HACKAGE_AUTH' -F 'package=@dist/$pkgid.tar.gz'
EOF
fi

if [ -n "$ROOT" ]; then
  step "Generating package documentation" << EOF
    HYPERLINK_FLAG="--hyperlink-source"
    if hadddock --hyperlinked-source &> /dev/null; then
      HYPERLINK_FLAG="--haddock-option='--hyperlinked-source'"
    fi
    cabal haddock \
      --html --html-location='/package/\$pkg-\$version/docs'\
      --contents-location='/package/\$pkg-\$version'\
      \$HYPERLINK_FLAG \
      --hoogle
    cp -R dist/doc/html/* $pkgid-docs
    tar cvz --format ustar $pkgid-docs -f $pkgid-docs.tar.gz
    rm -r $pkgid-docs
EOF
fi

if [ -n "$ROOT" -a -n "$HACKAGE_AUTH" ]; then
  step "Uploading package documentation to hackage" << EOF
    curl -X PUT 'https://hackage.haskell.org/package/$pkgid/candidate/docs' \
      -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' \
      -u '$HACKAGE_AUTH' --data-binary '@$pkgid-docs.tar.gz'
EOF
fi

end_steps
