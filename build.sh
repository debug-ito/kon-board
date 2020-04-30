#!/bin/bash

thisdir=`dirname "$0"`

echo "---- Build Haskell project" \
    && stack test \
    && echo "---- Generate Elm Bridge" \
    && stack run kon-board-gen-elm "$thisdir/kon-elm/src" \
    && echo "---- Build Elm application" \
    && ( cd kon-elm; elm make src/Main.elm --output=../static/main.js )
