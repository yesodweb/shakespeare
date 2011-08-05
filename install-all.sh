#!/bin/sh

CABAL=cabal

# for Yesod we don't need xml-hamlet
# cd hamlet
#($CABAL configure --enable-tests ||
    #($CABAL install --only-dependencies && $CABAL configure --enable-tests)
#) && $CABAL build && $CABAL test && ./Setup.lhs install || exit

PACKAGES="hamlet xml-hamlet"
for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
      ($CABAL install --only-dependencies && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit
    cd ..
done
