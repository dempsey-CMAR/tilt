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
      `Sample Interval (mins)` = sample_interval,
      `Depth Sounding (m)` = sounding_m
    ) %>%
    mutate(
      `Depth Sounding (m)` = as.character(`Depth Sounding (m)`),
      `Depth Sounding (m)` = if_else(
        is.na(`Depth Sounding (m)`), "Not recorded", `Depth Sounding (m)`
      )
    )
}
