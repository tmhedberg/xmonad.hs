#!/bin/bash

# Automatically attach to a Screen window associated with the current workspace

workspace="$1"

screen -x -p "$workspace" ||
  $SHELL
