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
    library(jsonlite)
    library(optparse)
})

# Parse command line arguments
option_list <- list(
    make_option(c("-r", "--reachfile"), type="character", default="reaches.json",
                help="Path to reaches JSON file [default %default]", metavar="character"),
    make_option(c("-i", "--index"), type="integer", default=0,
                help="Array index for reach to process [default %default]", metavar="integer")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)

# Get BUSBOI directory
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
SCRIPT_DIR <- dirname(script_path)
if(length(SCRIPT_DIR) == 0 || SCRIPT_DIR == "") {
    SCRIPT_DIR <- getwd()
}
BUSBOI_DIR <- file.path(SCRIPT_DIR, "BUSBOI")

source(file.path(SCRIPT_DIR, 'config.R'))

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

# Read reaches from JSON file
reach_json_path <- file.path(IN_DIR, opt$reachfile)
if(!file.exists(reach_json_path)) {
    stop(paste0("Reaches file not found: ", reach_json_path))
}

reaches <- fromJSON(reach_json_path)

# Get reach_id from index
if(opt$index < 0 || opt$index >= length(reaches)) {
    stop(paste0("Index ", opt$index, " out of bounds. Valid range: 0-", length(reaches)-1))
}

reach_id <- reaches[[opt$index + 1]]  # R is 1-indexed

cat(paste0("Processing reach [", opt$index, "]: ", reach_id, "\n"))

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
