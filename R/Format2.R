#' Render numbers
#'
#' Formatieren von Zahlen nach APA-Style.
#'
#' @param x Objekt kann Vector data.frame oder list sein
#' @param digits Nachkommastellen als vektor oder liste
#' @param lead.zero,drop0leading Null am  Anfang an formatC()
#' @param type,format format an formatC()
#' @param scientific,nsmall format an formatC()
#' @param drop0trailing  Nullen am Ende an formatC()
#' @param decimal.mark Kommatrennzeichen an formatC()
#' @param ... an FormatC
#'
#' @return Gleiches objekt wie der Input aber mit Character.
#' @export
#'
#' @examples
#' #'
#' x<- rnorm(10)
#' df <- data.frame(Item=c("a", "b", "c"),
#'                  x=x[1:3],
#'                  x2=c(1.2,2.3,.3),
#'                  beta=  c(.22,.13, NA),
#'                  x3=c(.42,.03,.003),
#'                  p.value=c(0.0002456,0.0398,.256))
#'
#' mx1<- as.matrix(df[,-1])
#' mx2<-matrix(rnorm(10), ncol=2)
#' lx<- list(a=1:5, b=rnorm(10))
#'
#' # Tvl<- c(1.0145, 25.45)
#' # Df<- c(45,44.14)
#' # Pvl<-c(0.0045,0.547863)
#' # rndr_T(Tvl, Df, Pvl)
#' #
#' #
#' #
#' # digits = 2
#' # lead.zero = TRUE
#' # type = "digits"
#' # scientific = FALSE
#' # nsmall =  ifelse(is.null(digits), 0L,  digits)
#' #
#' # formatC(Tvl, digits = 2,
#' #         format = "fg",
#' #         decimal.mark = getOption("OutDec")
#' # )
#' #
#' #
#' # glue(' T<sub>({
#' #      formatC(Df, digits = 2,
#' #      format = "f",
#' #      decimal.mark = getOption("OutDec") )
#' #      })</sub>={
#' #      formatC(Tvl, digits = 2,
#' #      format = "f",
#' #      decimal.mark = getOption("OutDec"))
#' #      }, p={
#' #      formatC(Pvl, digits = 3,
#' #      format = "g", drop0trailing = TRUE,
#' #      decimal.mark = getOption("OutDec"))
#' #      }')
#' #
#' #
#' #
#'
#' sprintf("%.2f", x)
#' formatC(x, decimal.mark =",")
#' format(x, decimal.mark =",")
#' Format2(x, OutDec=",")
#' Format2(x, decimal.mark=",")
#' Format2(x )
#'
#' Format2(df, digits=c(0,2,1,2,3,3),
#'         format=c("f","f","g","f","f","f"),
#'         drop0leading=c(F,F,F,F,F,T) )
#'
#' Format2(c(0.345,2.45,5.1,2.67,39.332,.83), digits=c(0,2,1,2,3,3),
#'         format=c("f","f","g","f","f","f"),
#'         drop0leading=c(F,F,F,F,F,T) )
#'
#'
#'
#' Format2(lx, list(c(0), c(1,1,4,3)))
#'
#'
Format2 <- function(x, ...) {
  UseMethod("Format2")
}


#' @rdname Format2
#' @param n_cols anzahl an Elementen  (intern in ergaenze_vector())
#' @examples
#'
#' stp25rndr:::ergaenze_vector(c(1,2,4), 6)
#'
ergaenze_vector <- function(x,
                            n_cols=NULL) {
  n  <- length(x)

  if (n == 1)
    rep(x, n_cols)
  else if (n > n_cols)
    x[1:n_cols]
  else if (n < n_cols)
    c(x, rep(x[n], n_cols - n))
  else
    x
}


#' @rdname Format2
#' @examples
#'
#' stp25rndr:::drop_0_leading(0.26589)
#'
drop_0_leading <- function(x,
                           OutDec = getOption("OutDec")) {
    sub(glue::glue('^(-)?0[{OutDec}]'),
        glue::glue('\\1{OutDec}'),
        x)
}


#' @rdname Format2
#' @param na.strings,na.symbol in der internen Funktion make_format genutzt und kann nur über Optionen geändert werden (noch nicht implementiert)
#' @examples
#'
#' stp25rndr:::make_format(c(12.5, NA, 32.1459),
#'                           2, "f", FALSE, FALSE, ".")
#'
#'
make_format <- function(x,
                        digits,
                        format,
                        drop0trailing,
                        drop0leading,
                        decimal.mark,
                        na.strings = "NA",
                        na.symbol="") {

 # unnoetig x <- round(x, digits)

  r <- formatC(
    x,
    digits = digits,
    format = format,
    drop0trailing = drop0trailing,
    decimal.mark = decimal.mark
  )

 if(!is.null(na.strings)) r[stringr::str_detect(r, na.strings)] <- na.symbol

  if (drop0leading)
    r <- drop_0_leading(r, decimal.mark)

  r
}



#' @rdname Format2
#' @export
#' @examples
#'
#'
#' #- matrix ---------------------------
#' res <- Format2(mx1, digits=2)
#' cat("\n in: matrix out:", class(res)," \n")
#' res
#' x<-mx2[,1]
#' res <- Format2(x, digits=c(1:5), drop0trailing = TRUE)
#' cat("\n in: ", class(x)," out:", class(res)," \n")
#' res
#' Format2(x, c(1:3))
#'
Format2.matrix <- function(x, digits=2,
                           lead.zero = TRUE,
                           type = "digits",
                           drop0leading  = !lead.zero,
                           format = if(type[1]=="digits") "f" else "g",
                           ...){
  if(!is.matrix(x)) x <- matrix(x)  # Fehler abfangen wenn funktion direkt aufgerufen wird
  if(!is.numeric(x[1,1])) return(x)

  # if(length(digits)==1) apply(x, 2, Format2, digits=digits, ...)
  # else matrix(mapply(Format2, x, digits, ...), ncol=ncol(x))
  digits <- ergaenze_vector(digits, ncol(x))
  drop0leading <- ergaenze_vector(drop0leading, ncol(x))
  format <- ergaenze_vector(format, ncol(x))

  i <- 0
  apply(x,2, function(q) {
    i <<- i + 1
    Format2(q,
            digits = digits[i],
            format = format[i],
            drop0leading = drop0leading[i]
    )

  })


}

#' @rdname Format2
#' @export
#' @examples
#'
#' #- tbl_df/data.frame ----------------------
#' Format2(tribble::tbl_df(df), digits=3)
#'
Format2.tbl_df <- function(x, ...) Format2.data.frame(data.frame(x), ...)


#' @rdname Format2
#' @export
#' @examples
#'
#'
#' #- data.frame ----------------------
#' res <- Format2(df[,-1], digits=2, FALSE)
#' cat("\n in: data.drame out:", class(res)," \n")
#' res
#' Format2(df, digits=3)
#'
Format2.data.frame <- function(x,
                               digits = 2,
                               lead.zero = TRUE,
                               type = "digits",
                               drop0leading  = !lead.zero,
                               format = if(type[1]=="digits") "f" else "g",
                               ...) {
  input <- length(x)
  if (!input)
    return(x)

  digits <- ergaenze_vector(digits, ncol(x))
  drop0leading <- ergaenze_vector(drop0leading, ncol(x))
  format <- ergaenze_vector(format, ncol(x))
  i <- 0
  stp25aggregate::dapply2(x,
                          function(q) {
                            i <<- i + 1

                            if (is.numeric(q) | is.integer(q))
                              Format2(q,
                                      digits = digits[i],
                                      format = format[i],
                                      drop0leading = drop0leading[i])
                            else
                              q
                          },
                          stringsAsFactors = FALSE)

}






#' @rdname Format2
#' @export
#' @examples
#'
#'
#'
#' #- list --------------------------
#' res <- Format2(lx, 2, FALSE)
#' cat("\n in: list out:", class(res)," \n")
#' res

Format2.list <- function(x,
                         digits = NULL,
                         lead.zero = TRUE,
                         type = "digits",
                         drop0leading  = !lead.zero,
                         format = if(type[1]=="digits") "f" else "g",
                         ...) {
  n_cols <- length(x)
  if (!n_cols)   return(x)

  for (i in 1:n_cols) {

 n<- length( x[[i]])

    if(!length(digits)==1) {
      if(is.list(digits)) ndigits <-   ergaenze_vector(digits[[i]], n)
    } else ndigits<- digits

    if(!length(drop0leading)==1) {
      if(is.list(drop0leading)) ndrop0leading <-   ergaenze_vector(drop0leading[[i]], n)
    }   else ndrop0leading<- drop0leading


    if(!length(format)==1) {
      if(is.list(format)) format <-   nergaenze_vector(format[[i]], n)
    }
   else nformat <- format

    x[[i]] <- Format2(x[[i]],
                      digits = ndigits,
                      format = nformat,
                      drop0leading = ndrop0leading,
                      ...
                    )
  }

  x
}



#' @rdname Format2
#' @export
#' @examples
#'
#'
#'
#' # #- vector -------------------------
#' # res<-Format2(x, digits=2, FALSE)
#' # cat("\n in: numeric out:", class(res)," \n")
#' # res
#' # Format2(as.character(x), digits=3)
#' # Format2(factor(x), digits=3)
#'
Format2.default  <- function(x,
                             digits = 2,
                             lead.zero = TRUE,
                             type = "digits",
                             #signif
                             scientific = FALSE,
                             nsmall =  ifelse(is.null(digits), 0L,  digits),
                             #-- wenn erster wert 0 dann trotzdem digits
                             drop0leading  = !lead.zero,
                             drop0trailing = FALSE,
                             format = if (type == "digits")
                               "f"
                             else
                               "g",
                             decimal.mark = getOption("OutDec"),
                             ...)
{
  n_cols <- length(x)
  if (!n_cols)
    return(x)
  if (is.character(x))
    return(x)
  if (is.factor(x))
    return(as.character(x))

  n_digits <- length(digits)

  if (n_digits == 1) {
    make_format(x,
                digits,
                format[1],
                drop0trailing[1],
                drop0leading[1],
                decimal.mark[1])
  }
  else{
    digits <- ergaenze_vector(digits, length(x))
    drop0leading <- ergaenze_vector(drop0leading, length(x))
    format <- ergaenze_vector(format, length(x))

    mapply(make_format,
           x,
           digits,
           format,
           drop0trailing,
           drop0leading,
           decimal.mark)

  }
}












