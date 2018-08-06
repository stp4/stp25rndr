# Helper ------------------------------------------------------------------

default_stp25  <- function (type =NULL, x1 = NULL)
{
  if(is.null(type)) return(type)

  val <- options()$stp25$apa.style
  if (!is.null(val)) {
    if( !is.null(x1) ) val[[x1]][[type]]
    else val[[type]]
  }
  else {
    if (!is.null(x1)){
      if (x1 == "prozent")
        type <- paste0(type, ".prz")
      else if (x1 == "p")
        type <- paste0(type, ".p" )
      else if (x1 == "r")
        type <- paste0(type, ".r" )
    }
  #  print(type)
    switch(
      type,
      digits = 2,
      digits.p = 3,
      digits.r = 2,
      digits.prz = 0,

      lead.zero = TRUE,
      lead.zero.p = FALSE,
      lead.zero.r = FALSE,
      lead.zero.prz = TRUE,

      plusmin_sign = FALSE,
      plusmin_str = " ",
      median.style = "IRQ",
      sep_element = ",",
      brackets = c("[", "]"),
      style = 1,
      style.prz=1,
      percentage_sign.prz = TRUE,
      percentage_str.prz = "%",
      null_percent_sign.prz =  "0",  #NULL,
      stars.value.p = c(0.001, 0.010, 0.050),
      stars.symbols.p = c("***", "**", "*"),
      with.stars.p = FALSE,
      NULL
    )
  }
}





# Symbols -----------------------------------------------------------------



#' @rdname rndr_
#' @param output  nur intern HTML oder Konsole
symbol_chi2 <- function(output = stp25output:::which_output()) {
  if (output == "html")
    "&chi;<sup>2</sup>"
  else
    "X2"
}





symbol_kleiner_gleich <-
  function(output = stp25output:::which_output()) {
    if (output == "html")
      "&le;"
    else
      "=<"
  }



symbol_groesser_gleich <-
  function(output = stp25output:::which_output()) {
    if (output == "html")
      "&ge;"
    else
      "=>"
  }

symbol_alpha	<- function(output = stp25output:::which_output()){
  if (output == "html")
    "&alpha;"
  else
    "alpha"}
symbol_beta	<- function(output = stp25output:::which_output()) {
  if (output == "html")
    "&beta;"
  else
    "beta"
}
symbol_eta	<-
  function(output = stp25output:::which_output()) {
    if (output == "html")
      "&eta;"
    else
      "eta"
  }
symbol_kappa	<- function(output = stp25output:::which_output()) {
  if (output == "html")
    "&kappa;"
  else
    "kappa"
}

symbol_seperator <- ", "





#' @rdname rndr_
#' @description countDigits Interne Function wird in Meanci2() verwendet
#' @export
#' @examples
#' countDigits(1.2345)
countDigits <- function(x) {
  x<- signif(x, 3)
  x <- strsplit(as.character(x),"\\.")[[1]][2]
  if (is.na(x))
    0
  else
    nchar(x)
}

# --- noch nicht benutzte Funktionen ----------------------

# adapted from John Fox's numbers2words function

make.digits <- function(x) {
  # This is a function breaks an input number x into the positive (left)
  # and negative(right) elements and returns these as numbers
  x <- toString(x)
  negative <- substr(x,1,1)=="-"
  if (negative) x <- substring(x,2)

  if (length(grep('.',x, fixed=TRUE))==0) {
    left <- x %>% strsplit("") %>% unlist
    right <- NULL
  }
  else {
    y <- x %>% strsplit(".", fixed=TRUE)
    left <- y[[1]][1] %>% strsplit("") %>% unlist
    right <- y[[1]][2] %>% strsplit("") %>% unlist
  }
  list(left,right, negative)
}



# Insert commas where needed in large numbers
make.proper <- function(x, sep=",") {
  if (is.numeric(x)) x <- format(x, scientific=FALSE)
  digits <- make.digits(x)
  outlength <- ceiling(length(digits[[1]])/3)-1+length(digits[[1]])
  right <- digits[[2]]
  left <- rep("", outlength)
  left[outlength:1 %% 4==0] <- sep
  left[outlength:1 %% 4!=0] <- digits[[1]]
  if (length(right>0)) paste(c(left, ".", right), collapse="")
  else  paste(left, collapse="")
}
