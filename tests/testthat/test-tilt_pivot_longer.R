test_that("tilt_pivot_longer() can be converted back to dat_wide", {

  expect_equal(
    dat %>% select(-contains("grossrange")),
    data.frame(dat2)
  )

})


test_that("tilt_pivot_longer() creates columns variable and value", {

  expect_contains(colnames(dat_long), c("variable", "value"))

})
