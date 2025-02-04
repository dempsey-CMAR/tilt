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
