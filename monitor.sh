#!/usr/bin/env bash

# get dir of this script
# https://stackoverflow.com/a/246128/1202636
# here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
here=/home/davide/git/RMLatDIBRIS/monitor

# specify monitor alias path for modules imported by the spec (like deep_subdict)
swipl -O -p monitor="$here" "$here"/monitor.pl -- "$@"
