#' Write BUSBOI output to NetCDF file.
#'
#' discharge time series
#' posteriors: r, bed elevations, prior_Q

# Libraries
library(RNetCDF)
library(dplyr)

# Constants
FILL = -999999999999

#' Insert NA values back into discharge list vectors to account for invalid
#' time steps.
#'
#' @param discharge list of discharge time series to insert NA at time steps
#' @param invalid_times list of invalid time indexes
#'
#' @return list of data with NA in place of invalid nodes
concatenate_invalid = function(discharge, invalid_times) {
  # Time-level data
  for (index in invalid_times) {
    discharge = append(discharge, NA, after=index-1)
  }
  return(discharge)
}

#' Write posteriors data to NetCDF file.
#'
#' @param nc_out NetCDF file pointer to write to
#' @param posteriors list of posteriors
#' @param is_valid boolean indicating if data is valid
#' @param out_data metadata including dimensions
write_posteriors = function(nc_out, posteriors, is_valid, out_data) {

  # Manning's roughness coefficient (r)
  r = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "r"),
    grp.inq.nc(nc_out, "r")$self
  )
  var.def.nc(r, "mean", "NC_DOUBLE", "nt")
  att.put.nc(r, "mean", "_FillValue", "NC_DOUBLE", FILL)
  var.def.nc(r, "sd", "NC_DOUBLE", NA)
  att.put.nc(r, "sd", "_FillValue", "NC_DOUBLE", FILL)

  if(is_valid){
    var.put.nc(r, "mean", posteriors$r)
    var.put.nc(r, "sd", posteriors$r_sd)
  } else {
    var.put.nc(r, "mean", rep(FILL, out_data$nt_length))
    var.put.nc(r, "sd", FILL)
  }

  # Bed elevations (per node)
  bed = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "bed"),
    grp.inq.nc(nc_out, "bed")$self
  )
  var.def.nc(bed, "elevation", "NC_DOUBLE", "nx")
  att.put.nc(bed, "elevation", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(bed, "elevation", "units", "NC_STRING", "meters")
  att.put.nc(bed, "elevation", "long_name", "NC_STRING", "bed_elevation_relative_to_geoid")

  if(is_valid){
    var.put.nc(bed, "elevation", posteriors$bed)
  } else {
    var.put.nc(bed, "elevation", rep(FILL, out_data$nx_length))
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
#' @param discharge vector of discharge values
#' @param discharge_sd scalar or vector of discharge standard deviation
#' @param is_valid boolean indicating if data is valid
write_discharge = function(nc_out, discharge, discharge_sd, is_valid) {

  # Discharge group
  q = tryCatch(
    error = function(cond) grp.def.nc(nc_out, "q"),
    grp.inq.nc(nc_out, "q")$self
  )

  # Discharge values
  var.def.nc(q, "q", "NC_DOUBLE", "nt")
  att.put.nc(q, "q", "_FillValue", "NC_DOUBLE", FILL)
  att.put.nc(q, "q", "units", "NC_STRING", "m^3/s")
  att.put.nc(q, "q", "long_name", "NC_STRING", "BUSBOI_discharge")
  
  if(is_valid){
    var.put.nc(q, "q", discharge)
  } else {
    var.put.nc(q, "q", FILL)
  }

  # Discharge standard deviation (if available)
  if(!is.null(discharge_sd)) {
    var.def.nc(q, "q_sd", "NC_DOUBLE", NA)
    att.put.nc(q, "q_sd", "_FillValue", "NC_DOUBLE", FILL)
    att.put.nc(q, "q_sd", "units", "NC_STRING", "m^3/s")
    
    if(is_valid){
      var.put.nc(q, "q_sd", discharge_sd)
    } else {
      var.put.nc(q, "q_sd", FILL)
    }
  }
}

#' Write discharge and posteriors to NetCDF file.
#'
#' @param data named list of metadata (reach_id, node_ids, nt, invalid_times)
#' @param posteriors list of posterior values (r, r_sd, bed, prior_Q)
#' @param discharge vector of discharge values
#' @param discharge_sd scalar or vector of discharge standard deviation
#' @param out_dir string to output directory
#' @param is_valid boolean indicating if reach has valid data
#' @param obs_times vector of observation time strings
#'
#' @export
write_output = function(data, posteriors, discharge, discharge_sd, out_dir, is_valid, obs_times) {

  print('Writing BUSBOI output...')

  # Concatenate invalid times back into discharge
  if(length(data$invalid_times) > 0) {
    discharge = concatenate_invalid(discharge, invalid_times = data$invalid_times)
  }

  # File creation
  nc_file = file.path(out_dir, paste0(data$reach_id, "_busboi.nc"))
  nc_out = create.nc(nc_file, format="netcdf4")

  # Global attributes
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_STRING", as.character(data$reach_id))
  att.put.nc(nc_out, "NC_GLOBAL", "title", "NC_STRING", "BUSBOI discharge estimates")
  att.put.nc(nc_out, "NC_GLOBAL", "institution", "NC_STRING", "SWOT-Confluence")
  att.put.nc(nc_out, "NC_GLOBAL", "algorithm", "NC_STRING", "BUSBOI")
  
  if(!is.null(data$node_ids)) {
    att.put.nc(nc_out, "NC_GLOBAL", "node_ids", "NC_INT64", unlist(data$node_ids))
  }

  # Dimensions
  nt_length = length(data$nt)
  dim.def.nc(nc_out, "nt", nt_length)
  var.def.nc(nc_out, "nt", "NC_INT", "nt")
  att.put.nc(nc_out, "nt", "units", "NC_STRING", "time_step_index")
  var.put.nc(nc_out, "nt", data$nt)

  # Time strings
  var.def.nc(nc_out, "time_str", "NC_STRING", "nt")
  att.put.nc(nc_out, "time_str", "units", "NC_STRING", "ISO8601_date_string")
  var.put.nc(nc_out, "time_str", as.character(obs_times))

  # Node dimension (if applicable)
  if(!is.null(data$node_ids)) {
    nx_length = length(data$node_ids)
    dim.def.nc(nc_out, "nx", nx_length)
    var.def.nc(nc_out, "nx", "NC_INT", "nx")
    att.put.nc(nc_out, "nx", "units", "NC_STRING", "node_index")
    var.put.nc(nc_out, "nx", seq(from = 0, by = 1, length.out = nx_length))
  } else {
    nx_length = 0
  }

  # Store dimensions for other functions
  data$nt_length = nt_length
  data$nx_length = nx_length

  # Write posteriors
  print('Writing posteriors...')
  write_posteriors(nc_out = nc_out, posteriors = posteriors, is_valid = is_valid, out_data = data)
  
  # Write discharge
  print('Writing discharge...')
  write_discharge(nc_out = nc_out, discharge = discharge, discharge_sd = discharge_sd, is_valid = is_valid)

  # Close file
  close.nc(nc_out)
  
  print(paste0('Output written to: ', nc_file))
}