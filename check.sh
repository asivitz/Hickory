#!/bin/sh

cabal build -v0 --ghc-options="-c -fforce-recomp -fno-code -no-link -O0"
