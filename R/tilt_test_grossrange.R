#' Apply grossrange test to tilt meter variables
#'
#' @param dat Data frame of tilt meter variables in wide format.
#'
#' @param tilt_grossrange_table Data frame with at least 5 columns:
#'   \code{variable}: entries must match the names of the variables being tested
#'   in \code{dat}; \code{sensor_min}: minimum acceptable value;
#'   \code{sensor_max}: maximum accepted value ; \code{user_min}: minimum
#'   reasonable value; \code{user_max}: maximum reasonable value.
#'
#' @param sensor_type Character string indicating which sensor collected the
#'   data. Options are "TCM-1" or "TCM-4".
#'
#' @return Returns \code{dat} with an additional grossrange_flag column for each
#'   tilt meter variable.
#'
#' @importFrom dplyr filter join_by left_join mutate select
#' @importFrom stringr str_remove_all
#' @importFrom tidyr pivot_wider
#'
#' @export


tilt_test_grossrange <- function(
    dat,
    sensor_type = c("TCM-1", "TCM-4"),
    tilt_grossrange_table = NULL
) {

  message("applying grossrange test")

  #import default thresholds from internal data file -----------------------
  if (is.null(tilt_grossrange_table)) {

    tilt_grossrange_table <- tilt_thresholds %>%
      filter(qc_test == "grossrange", sensor_type == !!sensor_type) %>%
      select(-c(qc_test, sensor_type, notes)) %>%
      pivot_wider(values_from = "threshold_value", names_from = "threshold")
  }

  dat <- dat %>%
    tilt_pivot_longer() %>%
    left_join(tilt_grossrange_table, by = join_by(variable)) %>%
    mutate(
      grossrange_flag = case_when(
        value > sensor_max | value < sensor_min ~ 4,
        (value <= sensor_max & value > user_max) |
          (value >= sensor_min & value < user_min) ~ 3,
        value <= user_max | value >= user_min ~ 1,
        TRUE ~ 2
      ),
      grossrange_flag = ordered(grossrange_flag, levels = 1:4)
    ) %>%
    select(-c(sensor_max, sensor_min, user_max, user_min)) %>%
    pivot_wider(
      names_from = variable,
      values_from = c(value, grossrange_flag),
      names_sort = TRUE
    ) %>%
    mutate(
      grossrange_flag_sea_water_to_direction_degree = if_else(
        value_sea_water_speed_cm_s <= 5,
        ordered(3, levels = 1:4),
        grossrange_flag_sea_water_to_direction_degree
      ))

  colnames(dat) <- str_remove_all(colnames(dat), "value_")

  dat
}

