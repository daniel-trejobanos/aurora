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
mv CAMx.v6.50.noMPI.NCF4.gfortran ../build/
patchelf --set-rpath '/usr/src/aurora/util/build/netcdf-c-4.6.1/lib/' CAMx.v6.50.noMPI.gfortran
docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx7.1/src \
    gcc:7 \
    make COMPILER=gfortran NCF=NCF4_C
cd ${AURORA_DIR}/CAMx7.1/src &&
make clean &&
mv CAMx.v7.10.noMPI.NFC4.gfortran ../build/
patchelf --set-rpath '/usr/src/aurora/util/build/netcdf-c-4.6.1/lib/' CAMx.v7.10.noMPI.NCF4.gfortran
