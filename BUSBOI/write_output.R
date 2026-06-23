#' Write BUSBOI output to NetCDF file.
#'
#' discharge time series
#' posteriors: r, bed elevations, chainage, prior_Q

# Libraries
library(RNetCDF)
library(dplyr)

# Constants
FILL = -999999999999


#' Convert time strings to seconds since 2000-01-01.
#'
#' Accepts both "YYYY-MM-DDTHH:MM:SSZ" and plain "YYYY-MM-DD" formats.
#' Unparseable entries (e.g. SWOT fill strings) are replaced with FILL.
#'
#' @param obs_times character vector of time strings
#' @return numeric vector of seconds since 2000-01-01 00:00:00 UTC
times_to_seconds = function(obs_times) {
  epoch <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  secs  <- as.numeric(
    as.POSIXct(substr(obs_times, 1, 10), format = "%Y-%m-%d", tz = "UTC") - epoch,
    units = "secs"
  )
  secs[is.na(secs)] <- FILL   # replace any unparseable / fill-value entries
  secs
}


#' Read the full observation time vector from a SWOT NetCDF file.
#'
#' Returns ALL time steps present in the source file (before any BUSBOI
#' filtering), truncated to "YYYY-MM-DD" strings so they can be matched
#' directly against the filtered obs_times passed to write_output().
#'
#' @param reach_id string reach identifier
#' @param swot_dir string path to the directory containing SWOT files
#'                 (the same value as swot_base in main_function)
#' @return character vector of "YYYY-MM-DD" strings with length == SWOT nt,
#'         or NULL when the file cannot be opened (triggers graceful fallback)
read_swot_times = function(reach_id, swot_dir) {
  swot_file <- file.path(swot_dir, paste0(reach_id, "_SWOT.nc"))

  if (!file.exists(swot_file)) {
    warning(sprintf(
      "SWOT file not found for reach %s - time-padding disabled, output spans filtered times only.",
      reach_id
    ))
    return(NULL)
  }

  tryCatch({
    swot_nc   <- open.nc(swot_file)
    reach_grp <- grp.inq.nc(swot_nc, "reach")$self
    time_str  <- var.get.nc(reach_grp, "time_str")
    close.nc(swot_nc)
    substr(time_str, 1, 10)   # "YYYY-MM-DDTHH:MM:SSZ" -> "YYYY-MM-DD"
  }, error = function(e) {
    warning(sprintf(
      "Could not read time_str from SWOT file for reach %s: %s - time-padding disabled.",
      reach_id, conditionMessage(e)
    ))
    NULL
  })
}


#' Pad a vector of observed values into a full-length FILL-initialised vector.
#'
#' @param values      numeric vector of observed values (length == length(obs_indices))
#' @param obs_indices integer vector of positions in the full time dimension
#'                    where `values` should be placed
#' @param nt_length   integer total length of the padded output
#' @return numeric vector of length nt_length, filled with FILL everywhere
#'         except at obs_indices
pad_to_full_time = function(values, obs_indices, nt_length) {
  padded <- rep(FILL, nt_length)
  padded[obs_indices] <- values
  padded
}


#' Write posteriors data to NetCDF file.
#'
#' @param nc_out     NetCDF file pointer to write to
#' @param posteriors list of posteriors (Q/prior_Q/discharge_sd already padded)
#' @param is_valid   boolean indicating if data is valid
#' @param out_data   metadata including dimensions
write_posteriors = function(nc_out, posteriors, is_valid, out_data) {

  # Channel shape (r), bankfull depth (db), bankfull width (wb) - all scalars
  r = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "r"),
    grp.inq.nc(nc_out, "r")$self
  )

  var.def.nc(r, "mean", "NC_DOUBLE", NA)
  att.put.nc(r, "mean", "_FillValue", "NC_DOUBLE", FILL)

  var.def.nc(r, "db", "NC_DOUBLE", NA)
  att.put.nc(r, "db", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(r, "db", "long_name",  "NC_STRING",  "bankfull_depth_prior")
  att.put.nc(r, "db", "units",      "NC_STRING",  "meters")

  var.def.nc(r, "wb", "NC_DOUBLE", NA)
  att.put.nc(r, "wb", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(r, "wb", "long_name",  "NC_STRING",  "bankfull_width_prior")
  att.put.nc(r, "wb", "units",      "NC_STRING",  "meters")

  if (is_valid) {
    var.put.nc(r, "mean", posteriors$r)
    var.put.nc(r, "db",   posteriors$db)
    var.put.nc(r, "wb",   posteriors$wb)
  } else {
    var.put.nc(r, "mean", FILL)
    var.put.nc(r, "db",   FILL)
    var.put.nc(r, "wb",   FILL)
  }

  # Bed elevations and chainage (nx values from bed elevation vector)
  bed = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "bed"),
    grp.inq.nc(nc_out, "bed")$self
  )

  var.def.nc(bed, "elevation", "NC_DOUBLE", "nx")
  att.put.nc(bed, "elevation", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(bed, "elevation", "units",      "NC_STRING", "meters")
  att.put.nc(bed, "elevation", "long_name",  "NC_STRING", "bed_elevation_relative_to_geoid")

  var.def.nc(bed, "chainage", "NC_DOUBLE", "nx")
  att.put.nc(bed, "chainage", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(bed, "chainage", "units",      "NC_STRING", "meters")
  att.put.nc(bed, "chainage", "long_name",  "NC_STRING", "along_channel_distance")

  if (is_valid) {
    var.put.nc(bed, "elevation", posteriors$bed)
    var.put.nc(bed, "chainage",  posteriors$chainage)
  } else {
    var.put.nc(bed, "elevation", rep(FILL, out_data$nx_length))
    var.put.nc(bed, "chainage",  rep(FILL, out_data$nx_length))
  }

  # Prior Q - already padded to full nt by write_output
  if (!is.null(posteriors$prior_Q)) {
    prior_q = tryCatch(
      error = function(cond) grp.def.nc(nc_out, "prior_q"),
      grp.inq.nc(nc_out, "prior_q")$self
    )
    var.def.nc(prior_q, "q", "NC_DOUBLE", "nt")
    att.put.nc(prior_q, "q", "_FillValue", "NC_DOUBLE", FILL)
    att.put.nc(prior_q, "q", "units",      "NC_STRING", "m^3/s")
    att.put.nc(prior_q, "q", "long_name",  "NC_STRING", "prior_discharge")

    if (is_valid) {
      var.put.nc(prior_q, "q", posteriors$prior_Q)
    } else {
      var.put.nc(prior_q, "q", rep(FILL, out_data$nt_length))
    }
  }
}


#' Write discharge data to NetCDF file.
#'
#' @param nc_out     NetCDF file pointer to write to
#' @param posteriors list of posteriors (Q/discharge_sd already padded)
#' @param is_valid   boolean indicating if data is valid
#' @param nt_length  integer length of time dimension
write_discharge = function(nc_out, posteriors, is_valid, nt_length) {

  q = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "q"),
    grp.inq.nc(nc_out, "q")$self
  )

  var.def.nc(q, "q", "NC_DOUBLE", "nt")
  att.put.nc(q, "q", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(q, "q", "units",      "NC_STRING", "m^3/s")
  att.put.nc(q, "q", "long_name",  "NC_STRING", "BUSBOI_discharge")

  if (is_valid) {
    var.put.nc(q, "q", posteriors$Q)
  } else {
    var.put.nc(q, "q", rep(FILL, nt_length))
  }

  if (!is.null(posteriors$discharge_sd)) {
    var.def.nc(q, "q_sd", "NC_DOUBLE", "nt")
    att.put.nc(q, "q_sd", "_FillValue", "NC_DOUBLE", FILL)
    att.put.nc(q, "q_sd", "units",      "NC_STRING", "m^3/s")

    if (is_valid) {
      var.put.nc(q, "q_sd", posteriors$discharge_sd)
    } else {
      var.put.nc(q, "q_sd", rep(FILL, nt_length))
    }
  }
}


#' Write discharge and posteriors to NetCDF file.
#'
#' The function opens the source SWOT file to recover the full (pre-filter)
#' time dimension, then pads Q, discharge_sd, and prior_Q with FILL at every
#' time step that BUSBOI filtered out.  If the SWOT file cannot be read the
#' output falls back to spanning only the filtered obs_times (original
#' behaviour).
#'
#' @param reach_id   string reach identifier
#' @param posteriors list of posterior values (r, bed, chainage, prior_Q, Q,
#'                   discharge_sd).  These span only the obs_times that BUSBOI
#'                   actually processed.
#' @param out_dir    string path to the output directory
#' @param is_valid   boolean - TRUE when the reach produced valid estimates
#' @param obs_times  character vector of "YYYY-MM-DD" (or ISO) time strings
#'                   that BUSBOI actually ran on (filtered subset of SWOT nt).
#'                   Pass NA for fully-invalid / all-NA cases.
#' @param swot_dir   string path to the directory containing SWOT input files
#'                   (the same value passed as swot_base to main_function).
#'                   Used to read the full unfiltered time vector for padding.
#'
#' @export
write_output = function(reach_id, posteriors, out_dir, is_valid,
                        obs_times, swot_dir) {

  print('Writing BUSBOI output...')

  # Read full unfiltered SWOT time vector from the source file
  all_times <- read_swot_times(reach_id, swot_dir)

  # Detect all-NA case
  all_na = is.null(posteriors$Q) || all(is.na(posteriors$Q)) || length(posteriors$Q) == 0

  # File creation
  nc_file = file.path(out_dir, paste0(reach_id, "_busboi.nc"))
  nc_out  = create.nc(nc_file, format = "netcdf4")

  # Global attributes
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id",   "NC_STRING", as.character(reach_id))
  att.put.nc(nc_out, "NC_GLOBAL", "title",       "NC_STRING", "BUSBOI discharge estimates")
  att.put.nc(nc_out, "NC_GLOBAL", "institution", "NC_STRING", "SWOT-Confluence")
  att.put.nc(nc_out, "NC_GLOBAL", "algorithm",   "NC_STRING", "BUSBOI")
  att.put.nc(nc_out, "NC_GLOBAL", "is_valid",    "NC_INT",    as.integer(!all_na))

  # ── All-NA early exit ───────────────────────────────────────────────────────
  if (all_na) {
    # Span the full SWOT time dimension even for invalid reaches so downstream
    # tools see a consistent time axis across all reaches
    if (!is.null(all_times)) {
      nt_length <- length(all_times)
      time_secs <- times_to_seconds(all_times)
    } else {
      nt_length <- 1L
      time_secs <- FILL
    }
    nx_length <- 1L

    dim.def.nc(nc_out, "nt", nt_length)
    var.def.nc(nc_out, "nt", "NC_INT", "nt")
    att.put.nc(nc_out, "nt", "units", "NC_STRING", "time_step_index")
    var.put.nc(nc_out, "nt", seq(from = 0L, by = 1L, length.out = nt_length))

    dim.def.nc(nc_out, "nx", nx_length)
    var.def.nc(nc_out, "nx", "NC_INT", "nx")
    att.put.nc(nc_out, "nx", "units", "NC_STRING", "node_index")
    var.put.nc(nc_out, "nx", 0L)

    var.def.nc(nc_out, "time", "NC_DOUBLE", "nt")
    att.put.nc(nc_out, "time", "units",      "NC_STRING", "seconds since 2000-01-01 00:00:00.000")
    att.put.nc(nc_out, "time", "long_name",  "NC_STRING", "observation_time")
    att.put.nc(nc_out, "time", "_FillValue", "NC_DOUBLE", FILL)
    var.put.nc(nc_out, "time", time_secs)

    out_data <- list(nt_length = nt_length, nx_length = nx_length)
    write_posteriors(nc_out, posteriors, is_valid = FALSE, out_data)
    write_discharge( nc_out, posteriors, is_valid = FALSE, nt_length)

    close.nc(nc_out)
    print(paste0('All-NA result, fill-value output written to: ', nc_file))
    return(invisible(NULL))
  }

  # ── Determine full time dimension and pad time-varying posteriors ───────────
  if (!is.null(all_times)) {

    # Both vectors are "YYYY-MM-DD" - direct string match is exact and avoids
    # any epoch-conversion ambiguity
    obs_dates   <- substr(obs_times, 1, 10)   # guard: handle ISO or plain format
    obs_indices <- match(obs_dates, all_times)

    # Warn and drop any obs dates absent from the SWOT time vector
    n_unmatched <- sum(is.na(obs_indices))
    if (n_unmatched > 0) {
      warning(sprintf(
        "%d obs_time(s) for reach %s not found in the SWOT time vector and will be dropped.",
        n_unmatched, reach_id
      ))
      keep            <- !is.na(obs_indices)
      obs_indices     <- obs_indices[keep]
      posteriors$Q    <- posteriors$Q[keep]
      if (!is.null(posteriors$discharge_sd))
        posteriors$discharge_sd <- posteriors$discharge_sd[keep]
      if (!is.null(posteriors$prior_Q))
        posteriors$prior_Q      <- posteriors$prior_Q[keep]
    }

    nt_length <- length(all_times)
    time_secs <- times_to_seconds(all_times)

    # Pad: real values at matched positions, FILL everywhere else
    posteriors$Q <- pad_to_full_time(posteriors$Q, obs_indices, nt_length)

    if (!is.null(posteriors$discharge_sd))
      posteriors$discharge_sd <- pad_to_full_time(
        posteriors$discharge_sd, obs_indices, nt_length)

    if (!is.null(posteriors$prior_Q))
      posteriors$prior_Q <- pad_to_full_time(
        posteriors$prior_Q, obs_indices, nt_length)

  } else {
    # SWOT file unavailable - fall back to filtered times only
    time_secs <- times_to_seconds(obs_times)
    nt_length <- length(obs_times)
  }

  nx_length <- length(posteriors$bed)

  # ── Dimensions ──────────────────────────────────────────────────────────────
  dim.def.nc(nc_out, "nt", nt_length)
  var.def.nc(nc_out, "nt", "NC_INT", "nt")
  att.put.nc(nc_out, "nt", "units", "NC_STRING", "time_step_index")
  var.put.nc(nc_out, "nt", seq(from = 0L, by = 1L, length.out = nt_length))

  dim.def.nc(nc_out, "nx", nx_length)
  var.def.nc(nc_out, "nx", "NC_INT", "nx")
  att.put.nc(nc_out, "nx", "units", "NC_STRING", "node_index")
  var.put.nc(nc_out, "nx", seq(from = 0L, by = 1L, length.out = nx_length))

  var.def.nc(nc_out, "time", "NC_DOUBLE", "nt")
  att.put.nc(nc_out, "time", "units",      "NC_STRING", "seconds since 2000-01-01 00:00:00.000")
  att.put.nc(nc_out, "time", "long_name",  "NC_STRING", "observation_time")
  att.put.nc(nc_out, "time", "_FillValue", "NC_DOUBLE", FILL)
  var.put.nc(nc_out, "time", time_secs)

  out_data <- list(nt_length = nt_length, nx_length = nx_length)

  print('Writing posteriors...')
  write_posteriors(nc_out, posteriors, is_valid, out_data)

  print('Writing discharge...')
  write_discharge(nc_out, posteriors, is_valid, nt_length)

  close.nc(nc_out)
  print(paste0('Output written to: ', nc_file))
}
