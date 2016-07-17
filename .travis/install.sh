#!/bin/bash

if [[ $TRAVIS_OS_NAME == 'osx' ]]; then

    # Install some custom requirements on OS X
    curl -sSL https://get.haskellstack.org/ | sh
    
else
    # Install some custom requirements on Linux
    cabal --version
    echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
    travis_retry cabal update
    cabal install --only-dependencies --enable-tests --enable-benchmarks
fi
