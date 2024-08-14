#!/bin/bash

podman run \
  --interactive \
  --tty \
  --init \
  --publish=8080:8080 \
  --rm \
  --volume=$PWD:/mnt \
  --workdir=/mnt \
  registry.gitlab.com/perm.pub/dock/baseprinter \
  "$@"
