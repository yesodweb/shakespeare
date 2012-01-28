# Origin

All these projects were born out of the hamlet html templating language. Hamlet was made, and it was good. So the same techniques were used to make a version for css and a version for javascript. These are now under shakespeare-css and shakespeare-js. The shakespeare package contains the core common code used in all other packages. xml-hamlet is a newer library similar to hamlet, but for xml. shakespeare-interpolated is for creating normal haskell strings.

# Install

Install with:

   cabal update && ./install-all.sh

# Testing

For each repo (besides shakespeare, which has no direct tests)

    cabal configure --enable-tests
    cabal build && dist/build/test/test
