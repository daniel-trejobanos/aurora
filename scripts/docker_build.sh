#!/usr/bin/env bash

# change to AURORA dir
AURORA_DIR="$(builtin cd ..; pwd)"
echo "${AURORA_DIR}"
docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx6.5/src \
    gcc:7 \
    make COMPILER=gfortran
cd ${AURORA_DIR}/CAMx6.5/src
make clean
mv CAMx.v6.50.noMPI.gfortran ../build/

docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx7.1/src \
    gcc:7 \
    make COMPILER=gfortran
cd ${AURORA_DIR}/CAMx7.1/src &&
make clean &&
mv CAMx.v7.10.noMPI.gfortran ../build/
