# For finding latest versions of the base image see
# https://github.com/SwissDataScienceCenter/renkulab-docker
ARG RENKU_BASE_IMAGE=renku/renkulab-py:3.9-0.11.1
FROM ${RENKU_BASE_IMAGE}

# Uncomment and adapt if code is to be included in the image
# COPY src /code/src

# Uncomment and adapt if your R or python packages require extra linux (ubuntu) software
# e.g. the following installs apt-utils and vim; each pkg on its own line, all lines
# except for the last end with backslash '\' to continue the RUN line
#
USER root
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-utils \
    emacs \
    build-essential \
    patchelf \
    gfortran \
    wget \
    csh
# RUN wget -P /usr/src/ \
#     https://camx-wp.azurewebsites.net/getmedia/CAMx_v6.50.src.180430.tgz && \
#     tar -xzvf /usr/src/CAMx_v6.50.src.180430.tgz && \
#     -C /usr/src/ && cd /usr/src && make COMPILER=gfortran
USER ${NB_USER}

# Uncomment and adapt if you want to automatically install
# python dependencies when the Docker image builds (pip or conda)
# Note: you will need to add a (pip) requirements.txt file
#       AND a (conda) environment.yml file for the below code to run,
#       but you can remove one or the other.
#
COPY requirements.txt /tmp/
RUN /opt/conda/bin/pip install -r /tmp/requirements.txt && \
    conda clean -y --all && \
    conda env export -n "root"
COPY src/ /tmp/src
RUN  pip install -e /tmp/src/python/
# RENKU_VERSION determines the version of the renku CLI
# that will be used in this image. To find the latest version,
# visit https://pypi.org/project/renku/#history.
ARG RENKU_VERSION=1.2.3

########################################################
# Do not edit this section and do not add anything below

# Install renku from pypi or from github if it's a dev version
RUN if [ -n "$RENKU_VERSION" ] ; then \
        source .renku/venv/bin/activate ; \
        currentversion=$(renku --version) ; \
        if [ "$RENKU_VERSION" != "$currentversion" ] ; then \
            pip uninstall renku -y ; \
            gitversion=$(echo "$RENKU_VERSION" | sed -n "s/^[[:digit:]]\+\.[[:digit:]]\+\.[[:digit:]]\+\(rc[[:digit:]]\+\)*\(\.dev[[:digit:]]\+\)*\(+g\([a-f0-9]\+\)\)*\(+dirty\)*$/\4/p") ; \
            if [ -n "$gitversion" ] ; then \
                pip install --force "git+https://github.com/SwissDataScienceCenter/renku-python.git@$gitversion" ;\
            else \
                pip install --force renku==${RENKU_VERSION} ;\
            fi \
        fi \
    fi

########################################################
