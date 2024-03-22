#!/bin/bash
set -e
set -x

podman run \
  --rm -v=.:/mnt -w=/mnt \
  registry.gitlab.com/perm.pub/dock/latexml-deb \
  make latexml-baseprint

podman run \
  --rm -v=.:/mnt -w=/mnt \
  docker.io/castedo/baseprinter \
  python3 -m epijats --from jats --to html _output/latexml/baseprint/article.xml _output/latexml/preview
