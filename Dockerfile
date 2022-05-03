FROM ubuntu:18.04


USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-utils \
    vim \
    build-essential \
    patchelf \
    gfortran \
    csh
