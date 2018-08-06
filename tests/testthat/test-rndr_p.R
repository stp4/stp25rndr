context("test-rndr_p.R")

test_that("rndr_P default", {
  expect_equal(rndr_P(
                     c(1, .25, .08, 0.05, 0.01, 0.0001),
                      digits=3, drop0leading=TRUE),
               c("p=1.000", "p=.250",  "p=.080" , "p=.050",  "p=.010" , "p<.001"))
})


test_that("rndr_P default-with.stars", {
  expect_equal(
    rndr_P(c(1, .25, .08, 0.05, 0.01, 0.0001),
           digits=3, drop0leading=TRUE,
           with.stars = TRUE),
    c(
      "p=1.000",
      "p=.250",
      "p=.080" ,
      "p=.050*",
      "p=.010**" ,
      "p<.001***"
    )
  )

})







test_that("rndr_Stars default", {

  expect_equal(
    rndr_Stars( c(.01, .0002, .03, .04, .06)),
    c("**",  "***", "*",   "*",   ""))

})


test_that("rndr_Stars marrix-dimension", {
  x <- c(.01, .0002, .03, .04, .06)
  mx <- as.matrix(data.frame(
    x = x,
    x2 = x,
    x3 = x,
    x4 = x
  ))

  expect_equal(dim(rndr_Stars(mx)), dim(mx))


})



test_that("rndr_Stars dasta.frame", {
  x <- c(.01, .0002, .03, .04, .06)

  dx <- data.frame(x = x,
                   x2 = x,
                   x3 = x,
                   x4 = x)

  res <-   rndr_Stars(dx)
  expect_equal(res$x, c("**",  "***", "*",   "*",   ""))

  expect_equal(2 * 2, 4)
})
