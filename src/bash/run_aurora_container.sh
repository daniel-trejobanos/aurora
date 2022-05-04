#!/usr/bin/env bash

# change to AURORA dir
AURORA_DIR="$(builtin cd ..; pwd)"
echo "${AURORA_DIR}"
docker run -it --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/ \
    aurora:no_renku
