#' Read tilt meter deployment metadata
#'
#' Could change this to read in the google sheet
#'
#' @param path File path to the folder with the metadata file. Default is the
#'   metadata_tracking folder on the CMAR R drive.
#'
#' @param file Name of the metadata file, including file extension. Must be a
#'   ".xlsx" file. Default is "tilt_meter_deployment_tracking.xlsx".
#'
#' @param sheet Name of the sheet to read in.
#'
#' @return Returns a data frame of the metadata tracking sheet.
#'
#' @importFrom dplyr across any_of mutate
#' @importFrom readxl read_excel
#' @importFrom sensorstrings ss_coords_from_ddm_to_dd
#' @importFrom stringr str_remove
#' @importFrom tidyr separate
#'
#' @export

tilt_read_tracking <- function(path = NULL, file = NULL, sheet = NULL) {

  if(is.null(path)) {
    path <- "R:/tracking_sheets/metadata_tracking"
  }

  if(is.null(file)) {
    file <- "tilt_meter_deployment_tracking.xlsx"
  }

  if(is.null(sheet)) {
    sheet <- "tracker"
  }

  coords_ddm <- c("deployment_latitude_n_ddm", "deployment_longitude_w_ddm",
                  "retrieval_latitude_n_ddm", "retrieval_longitude_w_ddm")

  read_excel(
    path = paste0(path, "/", file),
    sheet = sheet
  )  %>%
    # mutate(
    #   across(
    #     any_of(coords_ddm),
    #     ~ss_coords_from_ddm_to_dd(.x),
    #     .names = "{str_remove(.col, '_w_ddm|_n_ddm')}"
    #   ),
    #   deployment_longitude = -deployment_longitude,
    #   retrieval_longitude = -retrieval_longitude
    # ) %>%
    separate(
      deployment_time_utc, into = c(NA, "deployment_time_utc"), sep = " "
    ) %>%
    separate(
      retrieval_time_utc, into = c(NA, "retrieval_time_utc"), sep = " "
    )

}
