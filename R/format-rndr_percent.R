#-- rndr_percent ------------


#require(stp25data)

#' @rdname rndr_
#' @description rndr_percent()
#' Percentages are also most clearly displayed in parentheses with no decimal places:
#' Nearly half (49%) of the sample was married.
#' @param n Anzahl
#' @param percent,style Formatierung als Prozent oder als Prozent(Anzahl)
#' @param percentage_str,null_percent_sign Formatierungs Optionen
#' @export
#' @description Prozent
#' @examples
#'
#' #-- rndr_percent ------------
#'
#' rndr_percent(c(.2568, 99, 0.02568), c(4, 569, 25), digits = 1)
rndr_percent <- function(..., return_as_vector=TRUE)  {
  return(rndr_Percent(...,return_as_vector=return_as_vector))
}

#' @rdname rndr_
#' @export
rndr_percent2 <- function(..., return_as_vector=FALSE)  {

  res <- rndr_Percent(..., return_as_vector=return_as_vector)

  if (is.vector(res)) data.frame(percent=res, stringsAsFactors = FALSE)
  else stp25output::fix_to_data_frame(res)
}




# hier ist noch zu definieren war zurückgegeben werden soll

rndr_Percent <- function(x=n/sum(n, na.rm = TRUE)*100, n=NULL,
                         digits = default_stp25("digits", "prozent")[1],
                         symbol.trailing = default_stp25("percentage_str", "prozent"),
                         symbol.na= "n.a.",
                         style = default_stp25("style", "prozent"),
                         null_percent_sign = default_stp25("null_percent_sign", "prozent"),
                         percent = TRUE,
                         drop0leading = FALSE,
                         return_as_vector = FALSE,
                         ...) {



  make_vektor <- function( ) {
    prz[which(x < 1 / (10 ^ digits))] <-
      paste0("<", 1 / (10 ^ digits), symbol.trailing)
    res <- if (style == 1)  paste(prz, cnt)  else  paste(cnt, prz)

    if (!is.null(null_percent_sign))   res[which(n == 0)] <- null_percent_sign
    res[which(is.na(n))]  <-   symbol.na
    res
  }

  make_matrix <- function() {
    prz[which(x < 1 / (10 ^ digits))] <-
      paste0("<", 1 / (10 ^ digits), symbol.trailing)
    res <- if (style == 1) paste(prz, cnt) else  paste(cnt, prz)

    if (return_as_vector) {
      n <- as.vector(n)
     res <- as.vector(res)
    } else{
      if (inherits(x, "ftable")) {
        res <-matrix(res,
               ncol = ncol(x), nrow = nrow(x),
               dimnames = dimnames(as.matrix(x)))
      }
      else {
        res <-matrix(res,
               ncol = ncol(x), nrow = nrow(x),
               dimnames = dimnames(x))
      }
    }

    if (!is.null(null_percent_sign))   res[which(n == 0)] <- null_percent_sign
    res[which(is.na(n))]  <-   symbol.na
    res
  }

  if (is.table(x)) {
    length_dim <-
      if (length(dim(x)) == 1) {
        x <- as.matrix(x)
        n <- as.matrix(n)
      } else{
        x <- ftable(x)
        n <- ftable(n)
      }
  }
  else if (is.data.frame(x)){
    # workaround
    x1 <- x
    n1 <- n

    x <- as.matrix(x)
    n <- as.matrix(n)

  }


 if(is.null(n)){

    prz <- rndr2(x,
                digits = digits,
                drop0leading  = drop0leading,
                include.trailing = TRUE,
                include.bracket = if (style == 1)  FALSE  else TRUE,
                symbol.bracket = c("(", ")"),
                symbol.trailing = symbol.trailing)


  # if (!is.null(null_percent_sign))   res[which(n == 0)] <- null_percent_sign
   prz[which(is.na(x))] <-   symbol.na
   if (!is.null(null_percent_sign))   prz[which(x == 0)] <- null_percent_sign
   if(return_as_vector) as.vector(prz)

   return(prz)
   }


  # rndr2.default gibt Vector zurück
  prz <- rndr2.default(x, digits = digits,
               drop0leading  = drop0leading,
               include.trailing = TRUE,
               include.bracket = if (style == 1)  FALSE  else TRUE,
               symbol.bracket = c("(", ")"),
               symbol.trailing = symbol.trailing)


  cnt <- rndr2.default(n,  digits = 0,
            include.bracket = if (style == 1) TRUE else FALSE,
            symbol.bracket = c("(", ")"))


  if (is.vector((x))) {
    make_vektor()
  }
  else if (is.matrix(x)) {
    make_matrix()
  }
  else if (is.data.frame(x)) {
    res <- make_matrix()
    res <- as.data.frame(res,
                         row.names = row.names(x1),
                         stringsAsFactors = FALSE)
    names(res) <- names(x1)
    res
  }
}




#


#rndr_percent(c(19, 80.5, .99), c(20, 80, 1), digits = 0)

# #
# #
# round(80.500001)
#
# rndr_percent(10, 3, F, 2)
# x <- c(.2568, 99, 0.02568)
# n = c(4, 569, 25)
# percent = TRUE
# digits = options()$stp25$apa.style$prozent$digits[1]
# percentage_str = options()$stp25$apa.style$prozent$percentage_str
# style = options()$stp25$apa.style$prozent$style
# null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign
#
# rndr_percent(x, percentage_str = "Prozent")
#
# x <- xtabs( ~ med + g, hyper)
# n <- table(x)
# x <- prop.table(n)
# rndr_percent(x, n)
#
#
# hkarz$LAI <- factor(hkarz$lai, 0:1, c("pos", "neg"))
# hkarz$Tzell <- cut(hkarz$tzell, 3, c("low", "med", "hig"))

# dimnames(x)
# #data.frame(x)
# rndr_percent(x, n)#

# x1 <- xtabs( ~ LAI + Tzell, hkarz)
# rndr_Percent(n=x1)
#
# rndr_Percent(n=ftable(x1))
# #n <- as.matrix(x)
# #x <- as.matrix(prop.table(x) * 100)
# n1 <- matrix(
#   c(10, 45, 14,
#     11 , 22 ,  1 ),
#   nrow = 2,
#   ncol = 3,
#   byrow = TRUE,
#   dimnames = list(c("a", "b"),
#                   c("A", "B", "C"))
# )
# #rndr_percent(data.frame(x), data.frame(n))
#
#
# rndr_Percent(c(19, 80.599, .99), c(20, 80, 1), digits = 0)
#
#
# rndr_Percent(data.frame(x), data.frame(n))
#
#
#
#
# rndr_Percent(n=n1)
#
#
# x1 <- xtabs( ~ LAI + Tzell, hkarz)
# rndr_Percent(n=x1)
#
#
#
# stp25rndr:::rndr_percent_ftable( ftable(prop.table(x1))*100,
#   ftable(x1),
#   digits = 1,
#   percentage_str = "%",
#   style =1,
#   null_percent_sign = "--"
#
#
# )
#
#
#
#
# r <-rndr_Percent( ftable(prop.table(x1))*100,
#                                  ftable(x1),
#                                  digits = 1,
#                                  percentage_str = "%",
#                                  style =1,
#                                  null_percent_sign = "--"
#
#
# )
#
# r

















#' rndr_percent(10, 3, F, 2)
#' x <- c(.2568, 99, 0.02568)
#' n = c(4, 569, 25)
#' percent = TRUE
#' digits = options()$stp25$apa.style$prozent$digits[1]
#' percentage_str = options()$stp25$apa.style$prozent$percentage_str
#' style = options()$stp25$apa.style$prozent$style
#' null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign
#'
#' rndr_percent(x, percentage_str = "Prozent")
#'
#' x <- xtabs(~ med + g, hyper)
#' n <- table(x)
#' x <- prop.table(n)
#' rndr_percent(x, n)
#'
#'
#' hkarz$LAI <- factor(hkarz$lai, 0:1, c("pos", "neg"))
#' hkarz$Tzell <- cut(hkarz$tzell, 3, c("low", "med", "hig"))
#' x <- xtabs(~ LAI + Tzell, hkarz)
#'
#' n <- as.matrix(x)
#' x <- as.matrix(prop.table(x) * 100)
#'
#' rndr_percent(x, n)
#'
#' rndr_percent(data.frame(x), data.frame(n))








# rndr_percent <- function(x,
#                          n = NULL, ...) {
#   if (is.vector(x)) {
#     rndr_percent_default(x, n, ...)
#   }
#   else if (length(dim(x)) == 1) {
#     rndr_percent_default(as.vector(x), as.vector(n) , ...)
#
#   } else if (is.matrix(x)) {
#     rndr_percent_matrix(x, n, ...)
#   } else if (is.data.frame(x)) {
#     rndr_percent_matrix(as.matrix(x), as.matrix(n), ...)
#   }
#   else{
#     cat("\n else not a vector !!!!!!\n")
#     print(class(x))
#     stop(" Unbekante Classe in rndr_percent() ")
#   }
# }
#
# rndr_percent_default <- function(x,
#                                  n = NULL,
#                                  percent = TRUE,
#                                  # nur die Anzahl zurueckgeben (xtabs)
#                                  digits = options()$stp25$apa.style$prozent$digits[1],
#                                  percentage_str = options()$stp25$apa.style$prozent$percentage_str,
#                                  style = options()$stp25$apa.style$prozent$style,
#                                  null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign) {
#   if (is.null(percent))
#     percent <- style != 0
#   # print(digits)
#   digits<-digits[1]
#   if (percent) {
#     prz <- ifelse(
#       x < 1 / (10 ^ digits),
#       paste0("<", 1 / (10 ^ digits), percentage_str),
#       paste0(formatC(x,
#                      format = "f",
#                      digits = digits,
#                      decimal.mark = getOption("OutDec")),
#              percentage_str)
#     )
#     if (!is.null(n)) {
#       anz <- formatC(n, format = "f", digits =  0)
#       if (style == 1)
#         res <- paste0(prz, " (", anz, ")")
#       else
#         res <- paste0(anz, " (", prz, ")")
#     } else {
#       # in Kano verwendet
#       null_percent_sign <- NULL #fehler abangen
#       res <-  prz
#     }
#   }
#   else{
#     res <- formatC(n, format = "f", digits =  0)
#   }
#
#   if (!is.null(null_percent_sign))
#     res[which(n == 0)] <- null_percent_sign
#
#   res
# }
#
#
# rndr_percent_ftable <- function(x,
#                                 count = NULL,
#                                 digits = options()$stp25$apa.style$prozent$digits[1],
#                                 percentage_str = options()$stp25$apa.style$prozent$percentage_str,
#                                 style = options()$stp25$apa.style$prozent$style,
#                                 null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign) {
#
#   # cat("\nrndr_percent_ftable")
#
#   x_char <- apply(x, 2, function(y)
#     paste0(
#       formatC(y,
#               format = "f",
#               digits = digits,
#               decimal.mark = getOption("OutDec")
#       ),
#       percentage_str
#     ))
#
#
#   if (!is.null(count)) {
#     if (style == 1)
#       res <-
#         matrix(paste0(x_char, " (", count, ")"),
#                nrow =  nrow(count),
#                ncol = ncol(count))
#     else
#       res <-
#         matrix(
#           paste0(count, " (", x_char, percentage_str, ")"),
#           nrow =  nrow(count),
#           ncol = ncol(count)
#         )
#
#     if (!is.null(null_percent_sign))
#       res[which(count == 0)] <- null_percent_sign
#
#     ans <- stp25output::fix_to_data_frame(count)
#     n <- ncol(ans)
#     ans[, (n - ncol(res) + 1):n] <- res
#   }
#
#   else  {
#     if (!is.null(null_percent_sign))
#       #   digits<-2
#       # 10^-digits
#       res[which(x <  10^-digits)] <- null_percent_sign
#     ans <- stp25output::fix_to_data_frame(x_char)
#   }
#   ans
#
# }
#
#
#
#
#
#
#
# ### Warnung irgendwas ist faul!!!!! sollte überarbeitet werden sieh oben rndr_percent_ftable
# ### Die funktion war bei APA.xtabs in gebrauch
# ### Und ist implizid in Tabelle und APA usw für Prozent in verwendung
#
# rndr_percent_matrix <- function(x,
#                                 n = NULL,
#                                 percent = TRUE,
#                                 digits = options()$stp25$apa.style$prozent$digits[1],
#                                 percentage_str = options()$stp25$apa.style$prozent$percentage_str,
#                                 style = options()$stp25$apa.style$prozent$style,
#                                 null_percent_sign = options()$stp25$apa.style$prozent$null_percent_sign) {
#
#   digits<-digits[1]
#   myattr <- attributes(n) #-- colnames and rownames
#   nrw <- nrow(n)
#   n_char <- apply(n, 2, function(x) {
#     formatC(x, format = "f", digits = 0)
#   })
#   #------------------------------------------------
#   if (percent) {
#     # cat(" percent ")
#     x_char <- apply(x, 2, function(y)
#       formatC(y,
#               format = "f",
#               digits = digits,
#               decimal.mark = getOption("OutDec")
#       ))
#
#     if (style == 1)
#       res <-
#         matrix(paste0(x_char, percentage_str, " (", n_char, ")"), nrow = nrw)
#
#     else
#       res <-
#         matrix(paste0(n_char, " (", x_char, percentage_str, ")"), nrow = nrw)
#
#   } else
#     res <-  n_char
#
#   res <- data.frame(res,
#                     row.names = myattr$row.names,
#                     stringsAsFactors = FALSE)
#   names(res) <- myattr$names
#   if (!is.null(null_percent_sign))
#     res[which(n == 0)] <- null_percent_sign
#
#   res
# }




















