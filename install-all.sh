#!/bin/sh

CABAL=cabal

PACKAGES="shakespeare shakespeare-css shakespeare-js hamlet xml-hamlet"
for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
      ($CABAL update && $CABAL install HUnit QuickCheck hspec && $CABAL install --only-dependencies && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit 1
    cd ..
done
