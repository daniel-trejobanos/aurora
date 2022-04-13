#!/usr/bin/env bash

# change to AURORA dir
AURORA_DIR="$(builtin cd ..; pwd)"
echo "${AURORA_DIR}"
docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx_v6.50.src.180430 \
    gcc:7 \
    make  COMPILER=gfortran
