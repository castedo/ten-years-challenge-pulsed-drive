#!/bin/bash
set -e

OUTPUT=_output/quarto
mkdir -p $OUTPUT

set -x

podman run \
  --rm -v=.:/mnt -w=/mnt \
  registry.gitlab.com/perm.pub/dock/quarto-deb \
  quarto render

PREVIEW=html
#PREVIEW=html+pdf

podman run \
  --rm -v=.:/mnt -w=/mnt \
  docker.io/castedo/baseprinter \
  python3 -m epijats --from jats --to $PREVIEW $OUTPUT/baseprint/article.xml $OUTPUT/preview
