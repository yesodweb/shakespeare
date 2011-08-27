#!/bin/bash -e

# allow a CABAL env var to override
CABAL=${CABAL:-cabal}

# install testing dependencies
$CABAL install HUnit QuickCheck 'hspec >= 0.6.1 && < 0.7'

pkgs=( shakespeare
       shakespeare-css
       shakespeare-js
       shakespeare-text
       hamlet
       xml-hamlet
     )

# install each sub-respository
for pkg in "${pkgs[@]}"; do
  echo "Installing $pkg..."

  (
    cd "./$pkg"

    if ! $CABAL configure --enable-tests; then
      $CABAL install --only-dependencies
      $CABAL configure --enable-tests
    fi

    $CABAL build
    $CABAL test
    ./Setup.lhs install
  )
done
