#' Add a column of axis labels for current variables
#'
#' @param dat Data frame of tilt meter data in long format.
#'
#' @param convert_to_ordered_factor Logical variable indicating whether the new
#'   \code{variable_label} column should be converted to an ordered factor.
#'   Default is \code{TRUE}.
#'
#' @return Returns \code{dat} with an additional \code{variable_label} column
#'   for use in faceted figures.
#'
#' @importFrom dplyr case_when mutate
#' @importFrom stringr str_detect
#'
#' @export
#'

tilt_create_variable_labels <- function(dat, convert_to_ordered_factor = TRUE) {

  if (!("variable" %in% colnames(dat))) {
    dat <- tilt_pivot_longer(dat)
  }

  dat <- dat %>%
    mutate(
      variable_label = case_when(
        variable == "sea_water_speed_cm_s" ~ "Sea Water Speed (cm/s)",
        variable == "sea_water_speed_m_s" ~ "Sea Water Speed (m/s)",
        variable == "sea_water_to_direction_degree" ~ "Sea Water Direction (degree)",
        variable == "velocity_n_cm_s" ~ "North Velocity (cm/s)",
        variable == "velocity_n_m_s" ~ "North Velocity (m/s)",
        variable == "velocity_e_cm_s" ~ "East Velocity (cm/s)",
        variable == "velocity_e_m_s" ~ "East Velocity (m/s)",

        variable == "temperature_degree_c" ~ "Temperature (\u00B0C)",
        TRUE ~ NA_character_
      )
    )

  if(isTRUE(convert_to_ordered_factor)) {
    dat <- dat %>%
      mutate(
        variable_label = ordered(
          variable_label,
          levels = c("Sea Water Direction (degree)",
                     "Sea Water Speed (cm/s)",
                     "North Velocity (cm/s)",
                     "East Velocity (cm/s)",
                     "Temperature (\u00B0C)"))
      )
  }

  dat
}




#' Generate file path to import raw tilt data
#'
#' Raw data must be saved in a folder path/station/yyyy-mm-dd_station.
#'
#' @param path File path to the tilt_meter folder on the Coastal Monitoring
#'   Program server.
#'
#' @param station Character string of the station name. Will be converted to
#'   lower case, and all spaces will be replaced with an underscore.
#'
#' @param depl_date Character string of the deployment date in the order
#'   yyyy-mm-dd.
#'
#' @return The file path for importing raw deployment data.
#'
#' @importFrom stringr str_replace_all
#'
#' @export

tilt_import_path <- function(station, depl_date, path = NULL) {
  if (is.null(path)) {
    path <- "R:/data_branches/current/tilt_meter/station_folders"
  }

  station <- tolower(station)
  station <- str_replace_all(station, " ", "_")

  path <- paste(
    path, station,
    paste(depl_date, station, sep = "_"),
    sep = "/"
  )

  if (isFALSE(dir.exists(path))) {
    stop("File path << ", path, " >> does not exist. Check station spelling and deployment date")
  }

  file.path(path)
}

#' Generate file path and name to export tilt meter data
#'
#' @param depl_info Row of the tilt meter metadata tracking sheet that
#'   corresponds to the deployment of interest. Must include columns
#'   \code{county}, \code{station}, \code{deployment_date}, and
#'   \code{deployment_id}.
#'
#' @param path File path to a folder named \code{county}.
#'
#' @param sub_folder Character string of the sub-folder name (inside county
#'   folder) where \code{dat} should be exported. Default is \code{sub-folder =
#'   "new"}.
#'
#' @param ext File extension. Default is \code{ext = "csv"}.
#'
#' @return A file path for exporting deployment data, including file name and
#'   extension.
#'
#' @importFrom dplyr distinct mutate
#' @importFrom stringr str_replace_all
#' @importFrom tidyr separate
#'
#' @export

tilt_export_path <- function(
    depl_info,
    path = NULL,
    sub_folder = NULL,
    ext = "rds") {

  if (is.null(path)) {
    path <- "R:/data_branches/current/tilt_meter/processed_data/deployment_data"
  }

  if (is.null(sub_folder)) sub_folder <- "new"

  depl_id <- depl_info$deployment_id
  county <- tolower(depl_info$county)
  station <- tolower(gsub(" ", "_", depl_info$station))
  depl_date <- depl_info$deployment_date

  file_name <- paste0(paste(depl_date, station, depl_id, sep = "_"), ".", ext)

  path <- file.path(paste(path, county, sub_folder, sep = "/"))

  if (isFALSE(dir.exists(path))) {
    stop("File path << ", path, " >> does not exist.\nCan't export file << ", file_name, " >>")
  }

  file.path(paste(path, file_name, sep = "/"))
}



#' Extract deployment date and station name from compiled rds file
#'
#' @param file_path Path to the file, include file name and extension (.rds).
#'   File name must include the deployment date, the station name, and the
#'   deployment id separated by "_", e.g., "2024-05-16_1442_TILT001".
#'
#' @return Returns a tibble with three columns: \code{depl_date},
#'   \code{station}, and \code{deployment_id}.
#'
#' @importFrom dplyr %>%  mutate
#' @importFrom lubridate as_date
#' @importFrom stringr str_remove str_replace_all str_trim
#' @importFrom tidyr separate
#'
#' @export

tilt_extract_deployment_info2 <- function(file_path) {
  sub(".*/", "", file_path, perl = TRUE) %>%
    data.frame() %>%
    separate(
      col = ".", into = c("depl_date", "station_deployment_id"), sep = 11
    ) %>%
    mutate(
      depl_date = str_remove(depl_date, pattern = "_"),
      station_deployment_id = str_remove(
        station_deployment_id, pattern = ".rds")
    ) %>%
    separate(
      col = "station_deployment_id",
      into = c("station", "deployment_id"), sep = -11
    ) %>%
    mutate(
      # replace first _ with space (for station names with 2 words)
      station = str_replace_all(station, pattern = "_", " "),
      # trim trailing space
      station = str_trim(station, side = "right"),
      deployment_id = str_remove(deployment_id, pattern = "_"),
    )
}

