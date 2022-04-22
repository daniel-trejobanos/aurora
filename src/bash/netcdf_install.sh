#!/usr/bin/env bash

###############################################################################
#     In order to build the CAMx model with NetCDF support, some version of
#     the NetCDF development package must be available on the same system
#     where you compile/build the model. This package includes static
#     libraries used to link into the CAMx executable, as well as the FORTRAN
#     header file (netcdf.inc) needed for compiling CAMx. We recommend that
#     the NetCDF package is built from source code using the same compiler
#     that is used to compile CAMx.
#
#     This document is guidance on how to build NetCDF for use with CAMx. It
#     is not a shell script. Instead, it contains a list of the commands that
#     should be executed and the order of execution. It is assumed that this
#     will be done from the command line.
################################################################################
#
# ---- Download NetCDF source code and support packages.
#      Put these packages in a directory on your system.  We will refer to this
#      directory as {SRC_DIR}. The build of each package will be in a different
#      directory (such as /usr/local).  We will refer to the directory where the
#      packages will be built as {BLD_DIR} ----
#
SRC_DIR=/usr/src/aurora/util/src
BLD_DIR=/usr/src/aurora/util/build
#
# ---- Set the environment variable for the compilers to be used to build
#      the packages. We recommend that you use gcc for all C compilations.
#      The FORTRAN compiler should be the same compiler used to build CAMx ----
#

export FC=gfortran
export CC=gcc

#
# ---- Move to the source code directory ----
#

cd ${SRC_DIR}

#
# ---- Build the zlib library (used for compression) ----
#

tar xvzf zlib-1.2.11.tar.gz &&
cd zlib-1.2.11 &&
./configure --prefix=${BLD_DIR}/zlib-1.2.11 &&
make &&
make install &&

#
# ---- Build the HDF5 library (used for compression) ----
#

cd ${SRC_DIR} &&
tar xvzf hdf5-1.12.0.tar.gz &&
cd hdf5-1.12.0 &&
export LDFLAGS="-L${BLD_DIR}/zlib-1.2.11" &&
./configure --prefix=${BLD_DIR}/hdf5-1.12.0 --enable-fortran &&
make &&
make install &&

#
# ---- Build the NetCDF C libraries ----
#

cd ${SRC_DIR} &&
tar xvzf netcdf-c-4.6.1.tar.gz  &&
cd netcdf-c-4.6.1 &&
export CFLAGS=-I${BLD_DIR}/hdf5-1.12.0/include &&
export LDFLAGS=-L${BLD_DIR}/hdf5-1.12.0/lib &&
./configure --prefix=${BLD_DIR}/netcdf-c-4.6.1 --disable-dap &&
make &&
make install &&

#
# ---- Build the NetCDF FORTRAN libraries ----
#

cd ${SRC_DIR} &&
tar xvzf netcdf-fortran-4.4.5.tar.gz &&
cd netcdf-fortran-4.4.5 &&
export CPPFLAGS="-I${BLD_DIR}/netcdf-c-4.6.1/include" &&
export LDFLAGS="-L${BLD_DIR}/netcdf-c-4.6.1/lib" &&
export LD_LIBRARY_PATH="${BLD_DIR}/netcdf-c-4.6.1/lib" &&
./configure --prefix="${BLD_DIR}/netcdf-c-4.6.1" &&
make &&
make install &&

#
# ---- Make sure that the libraries exist ---
#

ls -l ${BLD_DIR}/netcdf-c-4.6.1/lib/libnetcdf.a &&
ls -l ${BLD_DIR}/netcdf-c-4.6.1/lib/libnetcdff.a
