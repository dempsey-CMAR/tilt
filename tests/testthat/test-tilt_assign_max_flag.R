test_that("tilt_assign_max_flag() assigns same flag as tilt_test_grossrange()", {

  expect_equal(
    dat_qc_max$grossrange_flag_temperature_degree_c,
    dat_qc_max$qc_flag_temperature_degree_c
  )

  expect_equal(
    dat_qc_max$grossrange_flag_sea_water_speed_cm_s,
    dat_qc_max$qc_flag_sea_water_speed_cm_s
  )

  expect_equal(
    dat_qc_max$grossrange_flag_sea_water_to_direction_degree,
    dat_qc_max$qc_flag_sea_water_to_direction_degree
  )

})
