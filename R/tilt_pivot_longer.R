#' Pivot title meter data variables longer
#'
#' @param dat Data frame of tilt meter data.
#'
#' @return Returns dat in a long format, with variable as an ordered factor.
#'
#' @importFrom dplyr any_of mutate
#' @importFrom tidyr pivot_longer
#'
#' @export
#'

tilt_pivot_longer <- function(dat) {

  vars <- c("sea_water_to_direction_degree", "sea_water_speed_cm_s" ,
            "velocity_n_cm_s", "velocity_e_cm_s", "temperature_degree_c")

  dat %>%
    pivot_longer(
      cols = any_of(vars), values_to = "value", names_to = "variable"
    ) %>%
    mutate(
      variable = ordered(
        variable,
        levels = c("sea_water_to_direction_degree", "sea_water_speed_cm_s" ,
                   "velocity_n_cm_s", "velocity_e_cm_s", "temperature_degree_c"))
    )
}
