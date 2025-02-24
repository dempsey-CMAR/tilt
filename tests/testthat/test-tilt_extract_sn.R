test_that("tilt_extract_serial_number exports correct number", {

  expect_equal(tilt_extract_sn(files_pass), 2402067)
})

test_that("tilt_extract_serial_number exports correct number as text", {

  expect_equal(tilt_extract_sn(files_pass, return_numeric = FALSE), "2402067")
})



test_that("tilt_extract_serial_number returns error if serial numbers do no match", {

  expect_error(tilt_extract_sn(files_fail))
})
