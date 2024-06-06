#!/usr/bin/env bash

# get dir of this script

here=$(dirname "$0")/

# specify monitor alias path for modules imported by the spec (like deep_subdict)
monitor="$here"$1".pl"
shift 1
swipl -O -p monitor="$here" "$monitor" -- "$@"
