#!/usr/bin/env Rscript

# BUSBOI Confluence Driver
# Reads from /mnt/data/input and writes to /mnt/data/output

suppressMessages({
    library(dplyr)
    library(tidyr)
    library(RNetCDF)
    library(optimx)
    library(ggplot2)
    library(deSolve)
    library(hydroGOF)
})

# Get BUSBOI directory
BUSBOI_DIR <- dirname(sys.frame(1)$ofile)
if(length(BUSBOI_DIR) == 0 || BUSBOI_DIR == "") {
    BUSBOI_DIR <- getwd()
}

# Load configuration first
source(file.path(BUSBOI_DIR, 'config.R'))

# Print configuration if verbose
if(VERBOSE) {
    print_config()
}

# Source all BUSBOI functions
source(file.path(BUSBOI_DIR, 'input.R'))
source(file.path(BUSBOI_DIR, 'jeff_tulip.R'))
source(file.path(BUSBOI_DIR, 'calcHgivenparams.R'))
source(file.path(BUSBOI_DIR, 'calcHgivenparams_fixedbed.R'))
source(file.path(BUSBOI_DIR, 'calcHgivenparams_bedonly.R'))
source(file.path(BUSBOI_DIR, 'GVF.R'))
source(file.path(BUSBOI_DIR, 'calculate_cum_dist.R'))
source(file.path(BUSBOI_DIR, 'calc_Sf_newH.R'))
source(file.path(BUSBOI_DIR, 'calculate_spline.R'))
source(file.path(BUSBOI_DIR, 'fit_hydraulics.R'))
source(file.path(BUSBOI_DIR, 'get_Q_prior.R'))
source(file.path(BUSBOI_DIR, 'rejection_sample.R'))
source(file.path(BUSBOI_DIR, 'Jeff_solver.R'))
source(file.path(BUSBOI_DIR, 'Jeff_solver_bedthenQ.R'))
source(file.path(BUSBOI_DIR, 'read_LSTM_ensemble.R'))
source(file.path(BUSBOI_DIR, 'run_BUSBOI.R'))
source(file.path(BUSBOI_DIR, 'Slope_empirical.R'))
source(file.path(BUSBOI_DIR, 'main_function.R'))
source(file.path(BUSBOI_DIR, 'write_output.R'))

# Read reach_id from input (look for SWOT file)
reach_files <- list.files(IN_DIR, pattern = paste0(SWOT_FILE_PATTERN, "$"), full.names = FALSE)

if(length(reach_files) == 0) {
    stop("No SWOT data file found in input directory")
}

# Extract reach_id from filename
reach_id <- gsub(SWOT_FILE_PATTERN, "", reach_files[1])

cat(paste0("Processing reach: ", reach_id, "\n"))

# Run BUSBOI with config parameters
result <- main_function(
    this_reach_id = reach_id,
    swot_base = paste0(IN_DIR, "/"),
    sos_base = paste0(IN_DIR, "/"),
    sword_base = paste0(IN_DIR, "/"),
    output_path = paste0(OUT_DIR, "/"),
    fix_bed = FIX_BED,
    GVF_on = GVF_ON,
    Q_prior = Q_PRIOR,
    tulip = TULIP
)

cat("BUSBOI processing complete\n")