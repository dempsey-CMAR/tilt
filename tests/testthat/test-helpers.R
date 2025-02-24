test_that("tilt_create_variable_labels() assigns correct labels()", {
  expect_equal(
    (vars %>%
       tilt_create_variable_labels(
         convert_to_ordered_factor = FALSE))$variable_label,
    c("Sea Water Direction (degree)",
      "Sea Water Speed (cm/s)",
      "North Velocity (cm/s)",
      "East Velocity (cm/s)",
      "Temperature (°C)")
  )
})


test_that("tilt_extract_deployment_info2() extracts deployment date, station, and deployment id", {
  expect_equal(
    tilt_extract_deployment_info2("2025-02-24_halifax_harbour_HL100-TILT.rds"),

    data.frame(
      depl_date = "2025-02-24",
      station = "halifax harbour",
      deployment_id = "HL100-TILT"
    )
  )
})
