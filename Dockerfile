# Stage 0 - Create from rocker R image
FROM rocker/r-ver:4.2.0 AS stage0

# Stage 1 - Install system dependencies
FROM stage0 AS stage1
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y \
    libnetcdf-dev \
    netcdf-bin \
    libhdf5-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    git \
    wget \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Stage 2 - Install R packages and BUSBOI code
FROM stage1 AS stage2
RUN R -e "install.packages(c( \
    'dplyr', \
    'tidyr', \
    'RNetCDF', \
    'optimx', \
    'ggplot2', \
    'deSolve', \
    'hydroGOF', \
    'jsonlite', \
    'optparse' \
    ), repos='https://cloud.r-project.org/')"

RUN mkdir -p /app/BUSBOI
COPY ./BUSBOI /app/BUSBOI/BUSBOI
COPY ./README.md /app/BUSBOI/README.md
WORKDIR /app/BUSBOI

# Make driver executable
RUN chmod +x /app/BUSBOI/BUSBOI/drive_BUSBOI.R

# Create mount point directories
RUN mkdir -p /mnt/data/input /mnt/data/output

# Stage 3 - Execute algorithm
FROM stage2 AS stage3
LABEL version="1.0"
LABEL description="BUSBOI v1.0 discharge algorithm."
LABEL maintainer="SWOT-Confluence"
ENV CONFLUENCE_US=1
ENTRYPOINT ["/app/BUSBOI/BUSBOI/drive_BUSBOI.R"]
