#!/bin/bash
set -e

mkdir -p _output/pandoc/baseprint

set -x

podman run \
  --rm -v=.:/mnt -w=/mnt \
  docker.io/castedo/baseprinter \
  pandoc --defaults=pandoc/baseprint.yaml --defaults=pandocin.yaml -o _output/pandoc/baseprint/article.xml

PREVIEW=html
#PREVIEW=html+pdf

podman run \
  --rm -v=.:/mnt -w=/mnt \
  docker.io/castedo/baseprinter \
  python3 -m epijats --from jats --to $PREVIEW _output/pandoc/baseprint/article.xml _output/pandoc/preview
