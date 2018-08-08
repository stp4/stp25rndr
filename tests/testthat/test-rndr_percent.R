context("test-rndr_percent.R")

test_that("vector", {

  x<- c(10, 45, 14,  11 , 22 ,  1,  2, 0 , NA)

  expect_equal(rndr_percent(x/105*100, x, null_percent_sign = "0", return_as_vector=TRUE),
  c("10% (10)", "43% (45)", "13% (14)",
    "10% (11)", "21% (22)", "<1% (1)",
      "2% (2)" ,       "0",    "n.a."))



  res <- rndr_percent2(n=x, null_percent_sign = "0", return_as_vector=FALSE)

  expect_that(is.data.frame(res), is_true())


})



test_that("matrix", {

  x<- c(10, 45, 14,  11 , 22 ,  1,  2, 0 , NA)
  mx <- matrix(x,
               ncol = 3,  byrow = TRUE,
               dimnames = list(c("a", "b" ,"c"),
                               c("A", "B", "C"))
  )


  mx1 <- matrix(x,
               ncol = 3,  byrow = FALSE,
               dimnames = list(c("a", "b" ,"c"),
                               c("A", "B", "C"))
  )

  expect_equal( rndr_percent(n=mx1, null_percent_sign = "0", return_as_vector=TRUE),
                rndr_percent(n=x, null_percent_sign = "0")
  )

  resm <- rndr_percent(n=mx, return_as_vector=FALSE)
  
  
  expect_that(is.matrix(resm) , is_true())
  expect_equal(dim(resm), dim(mx))
  expect_equal( resm[1,2],
  "43% (45)")

 res<- rndr_percent2(n=mx)
  # Source        A        B        C
  # a      a 10% (10) 43% (45) 13% (14)
  # b      b 10% (11) 21% (22)  <1% (1)
  # c      c   2% (2)  <1% (0)     n.a.
  #

  expect_that(is.data.frame(res), is_true())
  expect_that(is.data.frame(res), is_true())

})


#table, xtable, ftable
test_that("table", {


x <- with(warpbreaks, table(wool))
expect_equal(
rndr_percent(n=x, digits=2, return_as_vector=TRUE),
c("50.00% (27)", "50.00% (27)"))

xt <- rndr_percent(n=x, digits=2, return_as_vector=FALSE)

expect_equal(xt[1,1], c(A="50.00% (27)"))
expect_that(is.matrix(xt),
            is_true())
#rndr_percent2(n=x, digits=2, return_as_vector=TRUE)
expect_that(is.data.frame(rndr_percent2(n=x, digits=2)), is_true())


})


test_that("xtabs", {
  x <- xtabs( ~ wool, warpbreaks)
  expect_equal(
    rndr_percent(n=x, digits=2, return_as_vector=TRUE),
    c("50.00% (27)", "50.00% (27)"))
  
  xt <-rndr_percent(n=x, digits=2, return_as_vector=FALSE)
  expect_equal(xt[1,1], c(A="50.00% (27)"))
  expect_that(is.matrix(xt),
              is_true())
  #rndr_percent2(n=x, digits=2, return_as_vector=TRUE)
  expect_that(is.data.frame(rndr_percent2(n=x, digits=2)), is_true())


  expect_equal(rndr_percent2(n=x),
               rndr_percent2(n=with(warpbreaks, table(wool))))


})


test_that("ftable", {
  x<- ftable(Titanic)
  expect_equal(
    rndr_percent(n=x, return_as_vector=TRUE, null_percent_sign = "0")[1:3],
    c("0", "5% (118)", "0")
     )

  expect_that(is.matrix(rndr_percent(n=x, return_as_vector=FALSE)),
              is_true())

  expect_that(is.data.frame(rndr_percent2(n=x )), is_true())




})


test_that("rndr_percent2 xtabs same result as table", {

x1<- with(airquality, table(OzHi = Ozone > 80, Month))
x2<- xtabs( ~I(Ozone > 80) + Month, airquality)
#rndr_percent(n=x1)
#rndr_percent2(n=x1)[1,2]

expect_equal(rndr_percent2(n=x1),rndr_percent2(n=x2))



x1<- with(airquality, table(OzHi = Ozone > 80, cut(Temp, quantile(Temp)),  Month))
x2<- xtabs( ~I(Ozone > 80) + I(cut(Temp, quantile(Temp)))+ Month, airquality)
#rndr_percent(n=x1)
expect_equal(rndr_percent2(n=x1),rndr_percent2(n=x2)
)

})

