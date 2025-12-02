library(here)
library(data.table)
library(dplyr)
library(tilt)

dat_raw <- fread(here("data-raw/2024-05-16_1442_AT006-TILT_qc.csv"), data.table = FALSE)

dat <- dat_raw %>%
  filter(row_number() %% 300 == 0) %>%
  mutate(bin_height_above_sea_floor_m = 0.5)

tilt_plot_ts(dat, scales = "free")

# Export rds file
saveRDS(dat, file = here("inst/testdata/tilt_test_data.RDS"))
