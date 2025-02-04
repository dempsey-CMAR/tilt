#' Read in current and temperature tilt meter data
#'
#' @param path File path to the current and temperature csv files.
#'
#' @param files Character vector with the names of the current and temperature
#'   csv files, including the file extension.
#'
#' @param timestamp_to_utc Logical argument indicating whether to convert the
#'   timestamp column from Halifax time to UTC. \code{cmar_header} must also be
#'   set to \code{TRUE} for this to work.
#'
#'   For the raw tilt meter data, the timestamp column is in the timezone of the
#'   deployment date (e.g., "AST" if deployed in November to March and "DST" if
#'   deployed in March to November). The timestamp does NOT account for changes
#'   in daylight savings time.
#'
#'   When \code{timestamp_to_utc = TRUE}, the timestamp is converted to UTC by
#'   adding 3 hours if the deployment date was during daylight savings, or 4
#'   hours if the deployment date was during Atlantic Standard Time.
#'
#'   The earliest timestamp is used to define the original timezone (AST/DST).
#'
#' @param cmar_header Logical argument indicating whether to replace original
#'   file headers with standard CMAR column names.
#'
#' @return Returns a data frame with tilt meter current and temperature data,
#'   joined by timestamp.
#'
#' @importFrom dplyr full_join
#'
#' @export


tilt_read_data <- function(
    path, files, timestamp_to_utc = TRUE, cmar_header = TRUE
) {

  curr <- tilt_read_current_data(
    path = path, files = files, cmar_header = cmar_header
  )

  temp <- tilt_read_temperature_data(
    path = path, files = files, cmar_header = cmar_header
  )

  if(nrow(curr) != nrow(temp)) {
    warning("number of rows in current data is not equal to number of rows in temperature data")
  }

  if(isFALSE(cmar_header)) {
    dat <- curr %>% full_join(temp, by = "ISO 8601 Time")

    if(isTRUE(timestamp_to_utc)) {
      warning("could not convert timestamp to UTC. cmar_header must be TRUE")
    }
  }

  if(isTRUE(cmar_header)) {
    dat <- curr %>% full_join(temp, by = "timestamp_ns")

    if(isTRUE(timestamp_to_utc)) {
      dat <- dat %>%
        adcp::adcp_correct_timestamp(rm = TRUE)
    }
  }

  dat

}


#' Read csv file of tilt meter current data
#'
#' @inheritParams tilt_read_data
#'
#' @return Returns data frame of tilt meter current data (speed and direction).
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% select
#'
#' @export

tilt_read_current_data <- function(path, files, cmar_header = TRUE) {

  curr_file <- files[grep("current", files, ignore.case = TRUE)]

  curr_data <- fread(
    paste(path, curr_file, sep = "/"), data.table = FALSE
  )

  if(isTRUE(cmar_header)) {
    curr_data <- curr_data %>%
      select(
        timestamp_ns = `ISO 8601 Time`,
        sea_water_speed_cm_s = `Speed (cm/s)`,
        sea_water_to_direction_degree = `Heading (degrees)`,
        velocity_n_cm_s = `Velocity-N (cm/s)`,
        velocity_e_cm_s = `Velocity-E (cm/s)`
      )
  }

  curr_data
}



#' Read csv file of tilt meter temperature data
#'
#' @inheritParams tilt_read_data
#'
#' @return Returns a data frame of tilt meter temperature data.
#'
#' @importFrom data.table fread
#' @importFrom dplyr %>% select
#'
#' @export


tilt_read_temperature_data <- function(path, files, cmar_header = TRUE) {

  temp_file <- files[grep("temperature", files, ignore.case = TRUE)]

  temp_data <- fread(
    paste(path, temp_file, sep = "/"), data.table = FALSE
  )

  if(isTRUE(cmar_header)) {
    temp_data <- temp_data %>%
      select(
        timestamp_ns = `ISO 8601 Time`,
        temperature_degree_c = `Temperature (C)`
      )
  }

  temp_data
}
