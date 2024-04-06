#!/bin/bash

podman run \
  -it -p=8080:8080 \
  --rm -v=.:/mnt -w=/mnt \
  docker.io/castedo/baseprinter \
  bash -c "live-server --no-css-inject $* & cat"

# for some reason just plain
# live-server "$@"
# is not responding to ctrl-C
