context("test-rndr_mean.R")

test_that("mean and median works", {
  expect_equal(rndr_mean_range(1, 2, 3, 4),
               "1.00 (SD 2.00, range 3.00 to 4.00)")

  expect_equal(rndr_mean (1, 2),
               "1.00 (2.00)")

  expect_equal(rndr_median_range(1, 2, 3, 4),
               "1.00 (IRQ 2.00, range 3.00 to 4.00)")

  expect_equal(rndr_median(1, 2),
               "1.00 (IRQ 2.00)")




})


