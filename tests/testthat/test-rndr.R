context("test-rndr.R")

test_that("rndr2 works", {
  expect_equal(rndr2("2.364" , include.leading = TRUE,
                     include.trailing = TRUE,
                     include.bracket = TRUE,
                     symbol.bracket = c("(", ")"),
                     symbol.leading = "b=",
                     symbol.trailing = "***"), "(b=2.364***)" )
})



test_that("rndr2 data.frame", {

  dat <- data.frame(Source=c("intercept", "BMI"),
                    b=c(1.4,23.6),
                    df= 1:2,
                    p=c(0.04942568, 0.021457))

  res<- rndr2(dat)

  expect_equal(res$p, c("0.05", "0.02") )
})




test_that("rndr2 data.frame ein Spalte", {

  dat <- data.frame(Source=c("intercept", "BMI"),
                    b=c(1.4,23.6),
                    df= 1:2,
                    p=c(0.04942568, 0.021457))

 x1<-rndr2(dat[2])
 x2 <-rndr2(dat)[2]
  expect_equal(x1, x2)
})



test_that("rndr2 data.frame digits", {

  dat <- data.frame(Source=c("intercept", "BMI"),
                    b=c(1.4,23.6),
                    df= 1:2,
                    p=c(0.04942568, 0.021457))

 res <-rndr2(dat[2:4], digits=c(2,0,3))

  expect_equal(res$p, c(  "0.049", "0.021"))
})





test_that("rndr2 matrix dimnames", {


  mdat <- matrix(
    c(3.8458, 2.01259, .04915893,
      11.458, 2.4,     0.0148),
    nrow = 2,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(c("intercept", "BMI"),
                    c("b", "se", "p"))
  )
  res <-  rndr2(  mdat , digits=c(2,0,3))

  expect_equal(dimnames(res),  dimnames(mdat))

})


test_that("rndr2 matrix digits-drop0leading-stars", {


  mdat <- matrix(
    c(3.8458, 2.01259, .04915893,
      11.458, 2.4,     0.0148),
    nrow = 2,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(c("intercept", "BMI"),
                    c("b", "se", "p"))
  )
 res <-  rndr2(mdat ,
                 digits=c(2,0,3),
               drop0leading= c(FALSE, FALSE, TRUE),
          include.trailing = TRUE,
          symbol.trailing = c(rep("", 4),  "*", "**"))


  expect_equal(dim(res),  dim(mdat))
  expect_equal(res[1,3],  ".049*")
})




