#' Write BUSBOI output to NetCDF file.
#'
#' discharge time series
#' posteriors: r, bed elevations, chainage, prior_Q

# Libraries
library(RNetCDF)
library(dplyr)

# Constants
FILL = -999999999999

#' Format obs_times to YYYY-MM-DD strings
#'
#' @param obs_times vector of time strings in "YYYY-MM-DDTHH:MM:SSZ" format
#' @return vector of "YYYY-MM-DD" strings
format_times = function(obs_times) {
  as.character(as.Date(substr(obs_times, 1, 10)))
}

#' Write posteriors data to NetCDF file.
#'
#' @param nc_out NetCDF file pointer to write to
#' @param posteriors list of posteriors
#' @param is_valid boolean indicating if data is valid
#' @param out_data metadata including dimensions
write_posteriors = function(nc_out, posteriors, is_valid, out_data) {

  # Manning's roughness coefficient (r) - scalar, dimensionless
  r = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "r"),
    grp.inq.nc(nc_out, "r")$self
  )
  var.def.nc(r, "mean", "NC_DOUBLE", NA)
  att.put.nc(r, "mean", "_FillValue", "NC_DOUBLE", FILL)

  if(is_valid){
    var.put.nc(r, "mean", posteriors$r)
  } else {
    var.put.nc(r, "mean", FILL)
  }

  # Bed elevations and chainage (nx values from bed elevation vector)
  bed = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "bed"),
    grp.inq.nc(nc_out, "bed")$self
  )

  var.def.nc(bed, "elevation", "NC_DOUBLE", "nx")
  att.put.nc(bed, "elevation", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(bed, "elevation", "units", "NC_STRING", "meters")
  att.put.nc(bed, "elevation", "long_name", "NC_STRING", "bed_elevation_relative_to_geoid")

  var.def.nc(bed, "chainage", "NC_DOUBLE", "nx")
  att.put.nc(bed, "chainage", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(bed, "chainage", "units", "NC_STRING", "meters")
  att.put.nc(bed, "chainage", "long_name", "NC_STRING", "along_channel_distance")

  if(is_valid){
    var.put.nc(bed, "elevation", posteriors$bed)
    var.put.nc(bed, "chainage", posteriors$chainage)
  } else {
    var.put.nc(bed, "elevation", rep(FILL, out_data$nx_length))
    var.put.nc(bed, "chainage", rep(FILL, out_data$nx_length))
  }

  # Prior Q (if available)
  if(!is.null(posteriors$prior_Q)) {
    prior_q = tryCatch(
      error = function(cond) grp.def.nc(nc_out, "prior_q"),
      grp.inq.nc(nc_out, "prior_q")$self
    )
    var.def.nc(prior_q, "q", "NC_DOUBLE", "nt")
    att.put.nc(prior_q, "q", "_FillValue", "NC_DOUBLE", FILL)
    att.put.nc(prior_q, "q", "units", "NC_STRING", "m^3/s")
    att.put.nc(prior_q, "q", "long_name", "NC_STRING", "prior_discharge")

    if(is_valid){
      var.put.nc(prior_q, "q", posteriors$prior_Q)
    } else {
      var.put.nc(prior_q, "q", rep(FILL, out_data$nt_length))
    }
  }
}

#' Write discharge data to NetCDF file.
#'
#' @param nc_out NetCDF file pointer to write to
#' @param posteriors list of posteriors
#' @param is_valid boolean indicating if data is valid
#' @param nt_length integer length of time dimension
write_discharge = function(nc_out, posteriors, is_valid, nt_length) {

  q = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "q"),
    grp.inq.nc(nc_out, "q")$self
  )

  var.def.nc(q, "q", "NC_DOUBLE", "nt")
  att.put.nc(q, "q", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(q, "q", "units", "NC_STRING", "m^3/s")
  att.put.nc(q, "q", "long_name", "NC_STRING", "BUSBOI_discharge")

  if(is_valid){
    var.put.nc(q, "q", posteriors$Q)
  } else {
    var.put.nc(q, "q", rep(FILL, nt_length))
  }

  if(!is.null(posteriors$discharge_sd)) {
    var.def.nc(q, "q_sd", "NC_DOUBLE", "nt")
    att.put.nc(q, "q_sd", "_FillValue", "NC_DOUBLE", FILL)
    att.put.nc(q, "q_sd", "units", "NC_STRING", "m^3/s")

    if(is_valid){
      var.put.nc(q, "q_sd", posteriors$discharge_sd)
    } else {
      var.put.nc(q, "q_sd", rep(FILL, nt_length))
    }
  }
}

#' Write discharge and posteriors to NetCDF file.
#'
#' @param reach_id string reach identifier
#' @param posteriors list of posterior values (r, bed, chainage, prior_Q, Q, discharge_sd)
#' @param out_dir string path to output directory
#' @param is_valid boolean indicating if reach has valid data
#' @param obs_times vector of observation time strings in "YYYY-MM-DDTHH:MM:SSZ" format
#'
#' @export
write_output = function(reach_id, posteriors, out_dir, is_valid, obs_times) {

  print('Writing BUSBOI output...')

  # Detect all-NA case
  all_na = is.null(posteriors$Q) || all(is.na(posteriors$Q)) || length(posteriors$Q) == 0

  # File creation
  nc_file = file.path(out_dir, paste0(reach_id, "_busboi.nc"))
  nc_out = create.nc(nc_file, format="netcdf4")

  # Global attributes
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_STRING", as.character(reach_id))
  att.put.nc(nc_out, "NC_GLOBAL", "title", "NC_STRING", "BUSBOI discharge estimates")
  att.put.nc(nc_out, "NC_GLOBAL", "institution", "NC_STRING", "SWOT-Confluence")
  att.put.nc(nc_out, "NC_GLOBAL", "algorithm", "NC_STRING", "BUSBOI")
  att.put.nc(nc_out, "NC_GLOBAL", "is_valid", "NC_INT", as.integer(!all_na))

if(all_na) {
    nt_length = 1
    nx_length = 1
    
    dim.def.nc(nc_out, "nt", nt_length)
    var.def.nc(nc_out, "nt", "NC_INT", "nt")
    att.put.nc(nc_out, "nt", "units", "NC_STRING", "time_step_index")
    var.put.nc(nc_out, "nt", 0L)

    dim.def.nc(nc_out, "nx", nx_length)
    var.def.nc(nc_out, "nx", "NC_INT", "nx")
    att.put.nc(nc_out, "nx", "units", "NC_STRING", "node_index")
    var.put.nc(nc_out, "nx", 0L)

    var.def.nc(nc_out, "time_str", "NC_STRING", "nt")
    att.put.nc(nc_out, "time_str", "units", "NC_STRING", "YYYY-MM-DD")
    att.put.nc(nc_out, "time_str", "long_name", "NC_STRING", "observation_date")
    var.put.nc(nc_out, "time_str", "NA")

    out_data <- list(nt_length = nt_length, nx_length = nx_length)

    write_posteriors(nc_out = nc_out, posteriors = posteriors, is_valid = FALSE, out_data = out_data)
    write_discharge(nc_out = nc_out, posteriors = posteriors, is_valid = FALSE, nt_length = nt_length)

    close.nc(nc_out)
    print(paste0('All-NA result, fill-value output written to: ', nc_file))
    return(invisible(NULL))
}

  # Dimensions - nt from Q vector, nx from bed elevation vector
  nt_length = length(posteriors$Q)
  nx_length = length(posteriors$bed)

  dim.def.nc(nc_out, "nt", nt_length)
  var.def.nc(nc_out, "nt", "NC_INT", "nt")
  att.put.nc(nc_out, "nt", "units", "NC_STRING", "time_step_index")
  var.put.nc(nc_out, "nt", seq(from = 0, by = 1, length.out = nt_length))

  dim.def.nc(nc_out, "nx", nx_length)
  var.def.nc(nc_out, "nx", "NC_INT", "nx")
  att.put.nc(nc_out, "nx", "units", "NC_STRING", "node_index")
  var.put.nc(nc_out, "nx", seq(from = 0, by = 1, length.out = nx_length))

  # Time strings as variable (not dimension), formatted as YYYY-MM-DD
  var.def.nc(nc_out, "time_str", "NC_STRING", "nt")
  att.put.nc(nc_out, "time_str", "units", "NC_STRING", "YYYY-MM-DD")
  att.put.nc(nc_out, "time_str", "long_name", "NC_STRING", "observation_date")
  var.put.nc(nc_out, "time_str", format_times(obs_times))

  # Store dimensions for subfunctions
  out_data <- list(
    nt_length = nt_length,
    nx_length = nx_length
  )

  # Write posteriors
  print('Writing posteriors...')
  write_posteriors(nc_out = nc_out, posteriors = posteriors, is_valid = is_valid, out_data = out_data)

  # Write discharge
  print('Writing discharge...')
  write_discharge(nc_out = nc_out, posteriors = posteriors, is_valid = is_valid, nt_length = nt_length)

  # Close file
  close.nc(nc_out)

  print(paste0('Output written to: ', nc_file))
}