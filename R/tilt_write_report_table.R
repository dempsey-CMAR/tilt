#' Writes deployment table for summary report
#'
#' @param metadata Deployment metadata from the tilt meter metadata tracking
#'   sheet.
#'
#' @return Returns a tibble with columns for the report table.
#'
#' @importFrom dplyr if_else mutate select tibble
#'
#' @export

tilt_write_report_table <- function(metadata) {
  metadata %>%
    mutate(
      depl_duration = as.numeric(
        difftime(retrieval_date, deployment_date, units = "days")
      )
    ) %>%
    select(
      Station = station,
      `Instrument Model` = sensor_type,
      Latitude = deployment_latitude, Longitude = deployment_longitude,
      `Deployment Date` = deployment_date, `Retrieval Date` = retrieval_date,
      `Duration (d)` = depl_duration,
      `Depth Sounding (m)` = sounding_m,
      `Sensor Height Above Sea Floor (m)` = sensor_height_above_sea_floor_m,
      `Ensemble Interval (s)` = ensemble_interval_s,
      `Averaging Interval (s)` = averaging_interval_s,
      `Observations per Interval` = pings_per_ensemble
    ) %>%
    mutate(
      `Depth Sounding (m)` = as.character(`Depth Sounding (m)`),
      `Depth Sounding (m)` = if_else(
        is.na(`Depth Sounding (m)`), "Not recorded", `Depth Sounding (m)`
      )
    )
}
