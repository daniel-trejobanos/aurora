#!/usr/bin/env bash


AURORA_DIR="$(builtin cd ..; pwd)"
echo "${AURORA_DIR}"
echo "testing CAMx v6.5"
docker run --rm --name aurora \
    -v "${AURORA_DIR}":/usr/src/aurora \
    -w /usr/src/aurora/CAMx6.5/test_run_data/runfiles \
    aurora \
    /usr/src/aurora/CAMx6.5/test_run_data/runfiles/CAMx_v6.50.midwest.36.12.20020506-07.noMPI.job

# echo "testing CAMx v7.10"
# docker run --rm --name aurora \
#     -v "${AURORA_DIR}":/usr/src/aurora \
#     -w /usr/src/aurora/CAMx7.1/test_run_data/runfiles \
#     aurora \
#     /usr/src/aurora/CAMx7.1/test_run_data/runfiles/CAMx_v7.10.36.12.20160610-11.noMPI.job
