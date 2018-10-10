#!/bin/bash

# Automatically attach to a Screen window associated with the current workspace

workspace="$1"

if
  which screen >/dev/null &&
    [[ -n $workspace ]]
then
  screen -x -p "$workspace" ||
    $SHELL
else
  $SHELL
fi
