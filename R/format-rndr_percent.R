#' @rdname rndr_
#' @description rndr_percent() return character rndr_percent2()return data.frame
#' Percentages are also most clearly displayed in parentheses with no decimal places:
#' Nearly half (49%) of the sample was married.
#' @param n Anzahl
#' @param percent,style Formatierung als Prozent oder als Prozent(Anzahl)
#' @param percentage_str,null_percent_sign Formatierungs Optionen
#' @export
#' @examples
#'
#' #-- rndr_percent ------------
#'
#' rndr_percent(c(.2568, 99, 0.02568), c(4, 569, 25), digits = 1)
#'
rndr_percent <- function(x = n / sum(n, na.rm = TRUE) * 100,
                         n = NULL,
                         digits = default_stp25("digits", "prozent")[1],
                         symbol.trailing = default_stp25("percentage_str", "prozent"),
                         symbol.na = "n.a.",
                         style = default_stp25("style", "prozent"),
                         null_percent_sign = default_stp25("null_percent_sign", "prozent"),
                         percent = TRUE,
                         drop0leading = FALSE,
                         return_as_vector = TRUE,
                         decimal.mark = getOption("OutDec"),
                         # include.bracket = if (style == 1)  c(FALSE, TRUE)  else c(TRUE, FALSE),
                         ...) {
  if (return_as_vector) {
    n <- as.vector(n)
    x <- as.vector(x)
  } else{
    if (is.table(x)) {
      length_dim <-
        if (length(dim(x)) == 1) {
          x <- as.matrix(x)
          n <- as.matrix(n)
        } else{
          x <- stats::ftable(x)
          n <- stats::ftable(n)
        }
    }
    else if (is.data.frame(x)) {
      row_names = row.names(x)
      names_x <- names(x)
      x <- as.matrix(x)
      n <- as.matrix(n)
      
    }
  }
  
  
  prz <- rndr2.default(x, digits = digits,
                       decimal.mark = decimal.mark)
  
  #clean_small_value
  small_values <- x < 1 / (10 ^ digits)
  if (any(small_values))
    prz[which(small_values)] <- paste0("<", 1 / (10 ^ digits))
  
  # paste_prz_cnt
  if (!is.null(n)) {
    if (style == 1)
      res <-   sprintf("%s%s (%.0f)", prz, symbol.trailing, n)
    else
      res <- sprintf("%.0f (%s%s)", n, prz, symbol.trailing)
  }
  else{
    res <- paste0(prz, symbol.trailing)
  }
  
  #clean_0_value
  if (!is.null(null_percent_sign)) {
    if (!is.null(n))
      res[which(n == 0)] <- null_percent_sign
    else
      res[which(x == 0)] <- null_percent_sign
  }

  #  clean_na_value
  if (any(is.na(x)))
    res[which(is.na(x))]  <-   symbol.na
  

  
  if (is.vector((x))) {
    return(res)
  }
  else if (is.matrix(x)) {
    if (inherits(x, "ftable")) {
     return(matrix(
        res,
        ncol = ncol(x),
        nrow = nrow(x),
        dimnames = dimnames(as.matrix(x))
      ))
    }
    else {
      return(matrix(
        res,
        ncol = ncol(x),
        nrow = nrow(x),
        dimnames = dimnames(x)
      ))
    }
  }
  else if (is.data.frame(x)) {
    res <- matrix(res,
                  ncol = ncol(x), nrow = nrow(x))
    res <- as.data.frame(res,
                         row.names = row_names ,
                         stringsAsFactors = FALSE)
    names(res) <- names_x
   return(res) 
  }
  else return(x)
}

#' @rdname rndr_
#' @export
#' 
rndr_percent2 <- function(..., return_as_vector = FALSE)  {
  res <- rndr_percent(...,
                      return_as_vector = return_as_vector)
  
  if (is.vector(res))
    data.frame(percent = res,
               stringsAsFactors = FALSE)
  else
    stp25output::fix_to_data_frame(res)
}
