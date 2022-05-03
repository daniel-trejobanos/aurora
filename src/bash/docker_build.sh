#!/usr/bin/env bash

# change to AURORA dir
AURORA_DIR="$(builtin cd ../..; pwd)"
echo "${AURORA_DIR}"
docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx6.5/src \
    aurora:no_renku \
    make COMPILER=gfortran
patchelf --set-rpath '/usr/src/aurora/util/build/netcdf-c-4.6.1/lib/' /usr/src/aurora/CAMx6.5/src/CAMx.v6.50.noMPI.gfortran"
cd ${AURORA_DIR}/CAMx6.5/src
make clean
mv CAMx.v6.50.noMPI.NCF4.gfortran ../build/
docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx7.1/src \
    aurora:no_renku \
    "make COMPILER=gfortran NCF=NCF4_C && \
    patchelf --set-rpath '/usr/src/aurora/util/build/netcdf-c-4.6.1/lib/' /usr/src/aurora/CAMx7.1/src/CAMx.v7.10.noMPI.NCF4.gfortran"
cd ${AURORA_DIR}/CAMx7.1/src &&
make clean &&
mv CAMx.v7.10.noMPI.NFC4.gfortran ../build/
