context("test-rndr_odds_effzice.R")

test_that("rndr_r works", {
  expect_equal(rndr_r(.256), "r=.26")



})
test_that("rndr_r2 works", {
  expect_equal(rndr_r2(c(.5145, .458)) , "R<sup>2</sup>=.51, adj.R<sup>2</sup>=.46")

})


test_that("rndr_r2pseudo works", {
  expect_equal(rndr_r2pseudo(c(r2 = .256)), "r2=.26")

})






test_that("rndr_ods  works", {
  expect_equal(rndr_ods(c(.001251, 21, 13.765, .56784)),
               c("<0.01", ">20",   "13.77", "0.57"))

})



test_that("rndr_CI matrix", {
#is.character( rndr_CI(matrix(c(NA, 1:10, NA), ncol = 2), digits = 2))


  expect_equal(
    rndr_CI(matrix(c(NA, 1:10, NA), ncol = 2), digits = 2),
    c(
      "[, 6.00]",
      "[1.00, 7.00]",
      "[2.00, 8.00]" ,
      "[3.00, 9.00]",
      "[4.00, 10.00]",
      "[5.00, ]"
    )
  )

})
