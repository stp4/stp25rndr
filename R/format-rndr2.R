#' @rdname Format2
#' @description  rndr2 ist die interne default-Methode um statistische 
#' Kennzahlen wie t, p, oder r zu Formatieren
#' @param OutDec Detzimal-Trenzeichen wird bei rndr_ verwendet und ueber Optionen gesteuert
#' @param include.leading,symbol.leading Vorangestelltes Symbol
#' @param include.trailing,symbol.trailing Nachgestelltes Symbol
#' @param include.bracket,symbol.bracket Klammern
#'
#' @export
#'
rndr2 <- function(...) {
  UseMethod("rndr2")
}



#' @rdname Format2
#' @export
rndr2.matrix <- function(x, ...) {
  matrix(
    rndr2.default(x, ...),
    ncol = ncol(x),
    byrow = FALSE,
    dimnames = dimnames(x)
  )

}

#' @rdname Format2
#' @export
#' 
rndr2.data.frame  <- function(x, ...) {

  which_is_num <-sapply(x, is.numeric)
  which_is_non <- !which_is_num
  x[which_is_num] <- as.data.frame(rndr2(as.matrix(x[which_is_num]), ...),
                                   stringsAsFactors=FALSE
                                   )
  x
}


#' @rdname Format2
#' @export
#' 
rndr2.default  <-
  function(x,
           digits =  2,
           format = "f",
           drop0leading = FALSE,
           drop0trailing = FALSE,
           decimal.mark = getOption("OutDec"),
           include.leading = FALSE,
           include.trailing = FALSE,
           include.bracket = FALSE,
           symbol.bracket = c("(", ")"),
           symbol.leading = " # ",
           symbol.trailing = " # ",
           ...) {
    if (missing(x))
      return(
        c(
          digits = digits,
          format = format,
          decimal.mark = getOption("OutDec"),
          drop0trailing = FALSE,
          drop0leading = drop0leading,
          include.leading = include.leading,
          include.trailing = include.bracket,
          include.bracket = include.bracket
        )
      )

    if (is.numeric(x))
      x <- Format2(
        x,
        digits = digits,
        format = format,
        drop0leading = drop0leading,
        drop0trailing = drop0trailing,
        decimal.mark = decimal.mark
      )

    if (include.leading) {
      x <- paste0(symbol.leading, x)
    }

    if (include.trailing) {
      x <- paste0(x, symbol.trailing)
    }

    if (include.bracket) {
      x <- paste0(symbol.bracket[1], x, symbol.bracket[2])
    }

    x
  }

