path <- system.file("testdata", package = "tilt")

dat <- readRDS(paste0(path, "/tilt_test_data.RDS"))


# variable labels ---------------------------------------------------------

vars <- data.frame(
  variable = c(
    "sea_water_to_direction_degree",
    "sea_water_speed_cm_s",
    "velocity_n_cm_s",
    "velocity_e_cm_s",
    "temperature_degree_c")
)


# serial number -----------------------------------------------------------

files_pass <- c("2402067_careys_point_(0)_current.csv",
                "2402067_careys_point_(0)_temperature.csv")

files_fail <- c("2402067_careys_point_(0)_current.csv",
                "1234567_careys_point_(0)_temperature.csv")


# pivot -------------------------------------------------------------------

dat_long <- dat %>%
  select(-contains("grossrange")) %>%
  tilt_pivot_longer()

dat2 <- dat_long %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  select(
    deployment_id, county, waterbody, station,
    timestamp_utc, sea_water_speed_cm_s, sea_water_to_direction_degree,
    temperature_degree_c, velocity_e_cm_s, velocity_n_cm_s,
    bin_height_above_sea_floor_m
  )


# test --------------------------------------------------------------------

dat_qc <- dat %>%
  select(-contains("grossrange")) %>%
  tilt_test_grossrange()

dat_qc_max <- dat_qc %>%
  tilt_assign_max_flag()

tilt_plot_flags(dat_qc_max, qc_tests = "qc")

