#' BUSBOI Configuration File
#' 
#' This file contains all configurable parameters for BUSBOI runs.
#' Modify these values to change BUSBOI behavior without editing source code.

# ==============================================================================
# BUSBOI ALGORITHM PARAMETERS
# ==============================================================================
# Check for required packages
required_packages <- c("dplyr", "tidyr", "RNetCDF", "optimx", "ggplot2", "deSolve", "hydroGOF", "jsonlite", "optparse")

for(pkg in required_packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
        stop(paste0("Required package '", pkg, "' is not installed. Please install it with: install.packages('", pkg, "')"))
    }
}
# Q Prior Type
# Options: 'daily' or 'monthly'
# 'daily' - Use daily machine learning discharge priors from LSTM ensemble
# 'monthly' - Use monthly static discharge priors from SOS
Q_PRIOR <- 'monthly'

# TULIP (Two-stage Uncertainty-aware Low-flow Inference Process)
# Options: 'ON' or 'OFF'
# 'ON' - Use TULIP for improved low-flow estimation
# 'OFF' - Standard BUSBOI processing
TULIP <- 'OFF'

# Gradually Varied Flow (GVF) correction
# Options: 0 or 1
# 0 - GVF correction disabled
# 1 - GVF correction enabled (improves hydraulic geometry)
GVF_ON <- 0

# Bed elevation estimation mode
# Options: 0, 1, or 2
# 0 - 5-point bed estimation (full flexibility, one bed per node)
# 1 - 1-point bed estimation (single bed value for entire reach)
# 2 - Fixed bed (use prior bed values, no optimization)
FIX_BED <- 0

# ==============================================================================
# INPUT/OUTPUT DIRECTORIES
# ==============================================================================

# Input directory (Confluence standard)
IN_DIR <- "/mnt/data/input"

# Output directory (Confluence standard)
OUT_DIR <- "/mnt/data/output"

# ==============================================================================
# DATA FILE PATTERNS
# ==============================================================================

# SWOT data file pattern
SWOT_FILE_PATTERN <- "_SWOT.nc"

# SOS prior file patterns by continent
SOS_FILES <- c(
  'eu' = 'eu_sword_v17b_SOS_priors.nc',
  'na' = 'na_sword_v17b_SOS_priors.nc',
  'sa' = 'sa_sword_v17b_SOS_priors.nc',
  'oc' = 'oc_sword_v17b_SOS_priors.nc',
  'af' = 'af_sword_v17b_SOS_priors.nc',
  'as' = 'as_sword_v17b_SOS_priors.nc'
)

# SWORD file patterns by continent
SWORD_FILES <- c(
  'eu' = 'eu_sword_v17b.nc',
  'na' = 'na_sword_v17b.nc',
  'sa' = 'sa_sword_v17b.nc',
  'oc' = 'oc_sword_v17b.nc',
  'af' = 'af_sword_v17b.nc',
  'as' = 'as_sword_v17b.nc'
)

# Continent code to continent name mapping
CONTINENT_MAP <- list(
  '1' = 'af',  # Africa
  '2' = 'eu',  # Europe
  '3' = 'as',  # Asia (Siberia)
  '4' = 'as',  # Asia
  '5' = 'oc',  # Oceania
  '6' = 'sa',  # South America
  '7' = 'na',  # North America (North)
  '8' = 'na',  # North America (South)
  '9' = 'na'   # North America (Arctic)
)

# ==============================================================================
# NETCDF OUTPUT SETTINGS
# ==============================================================================

# Fill value for invalid/missing data in NetCDF files
NC_FILL_VALUE <- -999999999999

# Output file suffix
OUTPUT_FILE_SUFFIX <- "_busboi.nc"

# ==============================================================================
# ALGORITHM-SPECIFIC SETTINGS
# ==============================================================================

# Q prior adjustment factors (for daily prior mode)
Q_PRIOR_MIN_FACTOR <- 0.6  # Minimum Q = min(ML_Q) * this factor
Q_PRIOR_MAX_FACTOR <- 1.4  # Maximum Q = max(ML_Q) * this factor

# ==============================================================================
# LOGGING AND DIAGNOSTICS
# ==============================================================================

# Verbose output
VERBOSE <- TRUE

# Save intermediate results (RDS files for debugging)
SAVE_RDS <- TRUE

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get configuration as a named list
#' @return list of all configuration parameters
get_config <- function() {
  return(list(
    Q_PRIOR = Q_PRIOR,
    TULIP = TULIP,
    GVF_ON = GVF_ON,
    FIX_BED = FIX_BED,
    IN_DIR = IN_DIR,
    OUT_DIR = OUT_DIR,
    SWOT_FILE_PATTERN = SWOT_FILE_PATTERN,
    SOS_FILES = SOS_FILES,
    SWORD_FILES = SWORD_FILES,
    CONTINENT_MAP = CONTINENT_MAP,
    NC_FILL_VALUE = NC_FILL_VALUE,
    OUTPUT_FILE_SUFFIX = OUTPUT_FILE_SUFFIX,
    Q_PRIOR_MIN_FACTOR = Q_PRIOR_MIN_FACTOR,
    Q_PRIOR_MAX_FACTOR = Q_PRIOR_MAX_FACTOR,
    VERBOSE = VERBOSE,
    SAVE_RDS = SAVE_RDS
  ))
}

#' Print current configuration
print_config <- function() {
  cat("========================================\n")
  cat("BUSBOI Configuration\n")
  cat("========================================\n")
  cat(sprintf("Q Prior Type:      %s\n", Q_PRIOR))
  cat(sprintf("TULIP:             %s\n", TULIP))
  cat(sprintf("GVF Correction:    %s\n", ifelse(GVF_ON == 1, "ON", "OFF")))
  cat(sprintf("Bed Mode:          %s\n", 
      c("5-point", "1-point", "Fixed")[FIX_BED + 1]))
  cat(sprintf("Input Directory:   %s\n", IN_DIR))
  cat(sprintf("Output Directory:  %s\n", OUT_DIR))
  cat("========================================\n")
}
