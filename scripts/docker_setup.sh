#!/usr/bin/env bash


# These were the steps used to download the intel hpckit docker image, ifort
# compilers could not compile  CAMx correctly, thus i installed gcc inside the
# container and saved it

# mkdir docker
# docker pull intel/oneapi-hpckit
# docker run --name hpckit -it intel/oneapi-hpckit
## we installed gcc inside the image
# docker commit hpckit aurora
docker run -it --name aurora -v /Users/Daniel/git/aurora:/root aurora
