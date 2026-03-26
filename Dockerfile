# STAGE 0 - R base image with system dependencies pre-installed
FROM rocker/r-ver:4.3.0 as stage0

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    libnetcdf-dev \
    libnetcdff-dev \
    python3 \
    python3-dev \
    python3-pip \
    python3-venv \
    python3-boto3 \
    && rm -rf /var/lib/apt/lists/*

# STAGE 1 - R packages
FROM stage0 as stage1
RUN /usr/local/bin/Rscript -e "install.packages(c( \
    'doParallel', \
    'foreach', \
    'hydroGOF', \
    'RNetCDF', \
    'R.utils', \
    'optparse', \
    'dplyr', \
    'tidyr', \
    'optimx', \
    'ggplot2', \
    'deSolve', \
    'jsonlite', \
    'reticulate' \
    ), dependencies=TRUE, repos='http://cran.rstudio.com/'); \
    missing <- c('doParallel','foreach','hydroGOF','RNetCDF','R.utils','optparse','dplyr','tidyr','optimx','ggplot2','deSolve','jsonlite','reticulate')[!sapply(c('doParallel','foreach','hydroGOF','RNetCDF','R.utils','optparse','dplyr','tidyr','optimx','ggplot2','deSolve','jsonlite','reticulate'), requireNamespace, quietly=TRUE)]; \
    if(length(missing)) stop(paste('Failed to install:', paste(missing, collapse=', ')))"

# STAGE 2 - Copy files
FROM stage1 as stage2
RUN mkdir -p /app/data/input && mkdir -p /app/data/output
COPY ./BUSBOI /app/BUSBOI/
COPY ./drive_BUSBOI.R /app/

# STAGE 3 - Final
FROM stage2 as stage3
LABEL version="1.0" \
    description="Containerized BUSBOI algorithm." \
    "confluence.contact"="ntebaldi@umass.edu" \
    "algorithm.contact"="cjgleason@umass.edu"
ENTRYPOINT [ "/usr/local/bin/Rscript", "/app/drive_BUSBOI.R" ]
