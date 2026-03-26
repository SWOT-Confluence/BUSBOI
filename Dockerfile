FROM ubuntu as stage0
RUN echo "America/New_York" | tee /etc/timezone \
    && apt update \
    && DEBIAN_FRONTEND=noninteractive apt install -y \
        build-essential gcc gfortran locales \
        libcurl4-gnutls-dev libfontconfig1-dev libfribidi-dev \
        libgit2-dev libharfbuzz-dev libnetcdf-dev libnetcdff-dev \
        libssl-dev libtiff5-dev libxml2-dev tzdata wget \
        software-properties-common dirmngr \
        python3 python3-dev python3-pip python3-venv python3-boto3 \
    && locale-gen en_US.UTF-8

FROM stage0 as stage1
RUN . /etc/lsb-release \
    && wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
        | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
    && add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" \
    && apt update && apt -y install r-base r-base-dev \
    && rm -rf /var/lib/apt/lists/* \
    && /usr/local/bin/Rscript -e "install.packages(c('doParallel','foreach','hydroGOF','RNetCDF','R.utils','optparse','dplyr','tidyr','optimx','ggplot2','deSolve','jsonlite','reticulate'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

FROM stage1
RUN mkdir -p /app/data/input && mkdir -p /app/data/output
COPY ./drive_BUSBOI.R /app/
COPY ./BUSBOI /app/BUSBOI
LABEL version="1.0" \
    description="Containerized BUSBOI algorithm." \
    "confluence.contact"="ntebaldi@umass.edu" \
    "algorithm.contact"="cjgleason@umass.edu"
ENTRYPOINT [ "/usr/local/bin/Rscript", "/app/drive_BUSBOI.R" ]
