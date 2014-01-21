# Origin

    hamlet/

All these projects were born out of the hamlet html templating language.
Hamlet was made, and there was much rejoicing.

    shakespeare-js/
    shakespeare-csss/

So the same techniques for interpolation were used to make a version for css and a version for javascript.

    shakespeare

The shakespeare package contains the core common code used in all other packages.

# Install

Install with:

   cabal update && ./install-all.sh

# Testing

For each repo (besides shakespeare, which has no direct tests)

    cabal configure --enable-tests
    cabal build && dist/build/test/test

There is a folder shakespeare-test which is designed to build and test everything at once, but is not fully doing that yet
