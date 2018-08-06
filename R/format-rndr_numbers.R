#' @rdname rndr_
#' @param include.symbol,include.bracket Formatierungs Optionen
#' @export
#' @examples
#'
#'rndr_P(c(1,.25,.08,0.05,0.01,0.0001))

rndr_P <-
  function(x,
           include.symbol = TRUE,
           include.bracket = FALSE,
           digits = default_stp25("digits", "p"),
           format = "f",
           drop0leading = !default_stp25("lead.zero", "p"),
           drop0trailing = FALSE,

           with.stars = options()$stp25$apa.style$p$with.stars,

           include.leading = include.symbol,
           include.trailing = FALSE,
           symbol.bracket = c("(", ")"),
           symbol.leading = c("p=", "p<"),
           symbol.trailing = ""

           ) {


    if (missing(x))
      return(
        c(
          digits = digits,
          format = format,
          decimal.mark = getOption("OutDec"),
          drop0trailing = FALSE,
          drop0leading = drop0leading,
          include.symbol = include.symbol,
          include.bracket = include.bracket,
          with.stars = with.stars
        )
      )

    if (!is.null(with.stars)) {
      if (with.stars)
        include.trailing <- TRUE
        symbol.trailing <-rndr_Stars(x)
    }

    if (include.leading) {
      symbol.leading <-ifelse(x < 0.001, symbol.leading[2] , symbol.leading[1])
      x <- ifelse(x < 0.001, 0.001, x)
    }


    rndr2(x,

          digits = digits,
          format = format,
          decimal.mark = getOption("OutDec"),
          drop0leading = drop0leading,

          include.leading = include.symbol, symbol.leading=symbol.leading,
          include.trailing = include.trailing,symbol.trailing=symbol.trailing,
          include.bracket = include.bracket,symbol.bracket=symbol.bracket
         )
  }






#' @rdname rndr_
#' @export
rndr_Stars <- function (x,
                        stars.value =  default_stp25("stars.value", "p"),
                        stars.symbols = default_stp25("stars.symbols", "p")
                          )
{
  p_sternchen<-function(x)  {
    stern<-as.character(cut(round(x, 3),
                            c(-Inf, stars.value, Inf),
                            c(stars.symbols, "")))
    stern[is.na(stern)] <- ""
    stern
  }

  if( is.vector(x)){
    xnames<-names(x)
    x <- p_sternchen(x)
    names(x)<- xnames
  }
  else if(is.data.frame(x)) {
    xnames<-names(x)
    x <- data.frame(lapply(x, p_sternchen),
                    stringsAsFactors=FALSE)
    names(x)<- xnames
  }
  else if(is.matrix(x)) {

    xnames <- dimnames(x)
    x <- apply(x, 2, p_sternchen)
    dimnames(x) <- xnames

  }

  x
}



#' @rdname rndr_
#' @export
rndr_Effect_Size <- function(x,
                             digits = default_stp25("digits", "r")[1],
                             drop0leading = !default_stp25("lead.zero", "r"),
                             ...) {

  #stp25rndr:::default_stp25("lead.zero", "r")
  Format2(x,
          digits = digits,
          drop0leading  = drop0leading,
          ...)


}


#' @rdname rndr_
#' @export
rndr_Test_Statistic <-
  function (x,
            digits = default_stp25("digits", "Fstat")[1],
            drop0leading = !default_stp25("lead.zero", "Fstat"),
            ...)
  {
    Format2(x,
            digits = digits,
            drop0leading  = drop0leading,
            ...)
  }


