#!/bin/sh

CABAL=cabal

PACKAGES="shakespeare xml-hamlet shakespeare-css shakespeare-js hamlet"
for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
      ($CABAL install --only-dependencies && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit 1
    cd ..
done
