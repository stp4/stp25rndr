context("test-rndr_sig-test.R")

test_that("correlation", {
 expect_equal(  rndr_corr(-.548369, 0.005896, 55)
  , "r<sub>(55)</sub>=-.55, p=.006"
 )
})

test_that("T-Test", {
  expect_equal(  rndr_T(25.45, 45, .0045)
                 ,"T<sub>(45)</sub>=25.45, p=.004"
  )
})


test_that("F-Test", {
  expect_equal(  rndr_F(25.45, 45,3, .0045)
                 , "F<sub>(45, 3)</sub>=25.45, p=.004"
  )
})

test_that("H-Test", {
  expect_equal( rndr_H(25.45, 45, .0045),
                "H<sub>(45)</sub>=25.45, p=.004"
  )
})


test_that("H-Test", {
  expect_equal( rndr_W(25.45, .0045),
                "W=25.45, p=.004"
  )
})


