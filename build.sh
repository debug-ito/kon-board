#!/bin/bash

thisdir=`dirname "$0"`

stack test \
    && stack run kon-board-gen-elm > "$thisdir/kon-elm/src/Bridge.elm"
