context("test-format2.R")

test_that("vector", {
  x <- c(
    -0.87941590,
    -2.04291413,
     0.21485433,
    -0.75197185,
    -0.02093049,
    -0.32621055,
    -1.30286610,
     0.41051515,
     1.12308286,
     0.05353055
  )

  res <- Format2(x[1:3], digits = 2, FALSE)
  expect_equal(res, c("-.88",  "-2.04", ".21"))
})


test_that("list", {
  lx <- list(a = 1:5, b = rnorm(10))
  res <-  Format2(lx, 2, FALSE)

  expect_equal(res$a[1:2], c("1.00", "2.00"))
})


c
test_that("data.frame class", {
  df <- data.frame(
    Item = c("a", "b", "c"),
    b = c(1.2, 2.3, .3),
    beta =  c(.22, .13, NA),
    t = c(14.42, 5.03, 1.003),
    p.value = c(0.0002456, 0.0398, .256)
  )
  res <- Format2(df[, -1], digits = 2)
  expect_equal(class(res), "data.frame")

})

test_that("data.frame NA", {
  df <- data.frame(
    Item = c("a", "b", "c"),
    b = c(1.2, 2.3, .3),
    beta =  c(.22, 0.13, NA),
    t = c(14.42, 5.03, 1.003),
    p.value = c(0.0002456, 0.0398, .256)
  )
  res <- Format2(df[, -1], digits = 2)
  expect_equivalent(res$beta, c("0.22", "0.13", ""))

})

test_that("data.frame digits+format+drop", {
  df <- data.frame(
    Item = c("a", "b", "c"),
    b = c(1.2, 2.3, .3),
    beta =  c(.22, .13, NA),
    t = c(14.42, 5.03, 1.003),
    p.value = c(0.0002456, 0.0398, .256)
  )
  res <- Format2(
    df,
    digits = c(0, 2, 1, 2, 3),
    format = c("f", "f", "g", "f", "f"),
    drop0leading = c(F, F, F, F, T)
  )
  expect_equivalent(res$p.value, c(".000", ".040", ".256"))

})


test_that("list", {
  lx <- list(a = 1:5, b = rnorm(10))
  res <- Format2(lx, 2, FALSE)
  expect_equal(class(res), "list")
})



test_that("list digits", {
  lx <- list(a = 1:5, b = rnorm(10))
  res <- Format2(lx, list(c(0), c(1, 1, 4, 3)))


  expect_equal(res$a, c("1", "2", "3", "4", "5"))
})



test_that("matrix digits", {
 mdat <- matrix(
  c(3.8458, 2.01259, .04915893,
    11.458, 2.4,     0.0148),
  nrow = 2,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c("intercept", "BMI"),
                  c("b", "se", "p"))
)



expect_equal(Format2(  mdat , digits=c(2,0,3) ), matrix(
  c(  "3.85",  "2", "0.049",
     "11.46",  "2", "0.015"),
  nrow = 2,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c("intercept", "BMI"),
                  c("b", "se", "p"))
))
})






