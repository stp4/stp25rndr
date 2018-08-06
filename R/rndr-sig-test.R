#' Render
#'
#' Formatiere von Zahlen nach dem APA-Style ( American Psychological Association ).
#'
#' see:
#' http://winvector.github.io/APAsig/APAsig.html
#'  http://my.ilstu.edu/~jhkahn/apastats.html
#'      https://web2.uconn.edu/writingcenter/pdf/Reporting_Statistics.pdf
#' @param x Obkekt oder vektor
#' @param digits Nachkommastellen
#' @param ... alles an Format2()
#' @return Character-String
#' @export
#' @keywords internal
#' @examples
#' rndr_(1.234, 3)
#'
rndr_ <- function(...) Format2(...)



#' @rdname rndr_
#' @param m Mittelwert
#' @param s,iqr SD,IRQ  (ein Wert)
#' @export
rndr_median <- function(m, iqr,
                        digits=default_stp25("digits", "mittelwert"),
                        ...){
   paste0(Format2(m, digits[1],...), " (IRQ ", Format2(iqr, digits[1],...), ")")
}


#' @rdname rndr_
#' @export
rndr_median_quant<- function(x,
                             digits=default_stp25("digits", "mittelwert"),
                             ...){
  paste0(
    Format2(x[3], digits[1], ...),
    " (",
    Format2(x[2], digits[1], ...),
    symbol_seperator,
    Format2(x[4], digits[1], ...),
    ")"
  )
}

#' @rdname rndr_
#' @export
# noch nicht umgesetzt (Tabelle(..., APA=TRUE))
rndr_median_range <- function (m, iqr, mn, mx,
                               digits = default_stp25("digits", "mittelwert"),
                               ...) {
  paste0(
    Format2(m, digits[1],...), " (IRQ ", Format2(iqr, digits[1],...),
    ", range ",
    Format2(mn, digits[1],...),
    " to ",
    Format2(mx, digits[1],...),
    ")"
  )
}


#' @rdname rndr_
#' @export
#' @examples
#'  rndr_mean_range(1,2,3,4)
#'   rndr_mean (1,2 )
#'   rndr_median_range(1,2,3,4)
#'   rndr_median(1,2)
rndr_mean <- function(m, s,
                      digits=default_stp25("digits", "mittelwert"),
                      ...) {
  paste0(Format2(m, digits[1], ...), " (", Format2(s, digits[1], ...), ")")

}
#' @rdname rndr_
#' @export
rndr_mean_range <- function(m, s, mn, mx,
                            digits=default_stp25("digits", "mittelwert"),
                            ...) {
   paste0(
    Format2(m, digits[1], ...), " (SD ",
    Format2(s, digits[1], ...), ", range ",
    Format2(mn, digits[1], ...), " to ", Format2(mx, digits[1], ...), ")"
  )

}



#' @rdname rndr_
#' @export
rndr_ods <- function(x, digits = default_stp25("digits", "r")) {
  res <- Format2(x, digits=digits)
  res[which(x > 20)] <- ">20"
  res[which(x < .01)] <- "<0.01"
  res
}


#' @rdname rndr_
#' @param ci Vektor mit zwei Werten
#' @param sep,sep_1,sep_2 intern seperator
#' @export
#' @examples
#'   rndr_CI(matrix(c(NA, 1:10, NA), ncol=2))
#'
#'
rndr_CI <- function(ci,
                    digits = default_stp25("digits", "r"),
                    sep =    default_stp25("sep_element"),
                    sep_1 =  default_stp25("brackets")[1],
                    sep_2 =  default_stp25("brackets")[2] ){

  res<- paste0(sep_1,
               Format2.matrix(ci[,1], digits),
               sep, " ",
               Format2.matrix(ci[,2], digits),
               sep_2)

  res
}



#' @rdname rndr_
#' @export
#'
#'
rndr_ods_CI<- function(ci,
                       digits =  default_stp25("digits", "r"),
                       sep=   default_stp25("sep_element"),
                       sep_1= default_stp25("brackets")[1],
                       sep_2= default_stp25("brackets")[2]
                       ){


  res<- paste0(sep_1,
               rndr_ods(ci[,1], digits),
               sep, " ",
               rndr_ods(ci[,2], digits),
               sep_2)

 # res[which(is.na(ci[,1]))] <- NA
  res
}



#' @rdname rndr_
#' @export
rndr_mean_CI <- function(m, ci, digits) {
  #  print(c(m, s, digits))
  paste(Format2(m, digits), rndr_CI(ci, digits ))

}







#' @rdname rndr_
#' @export
rndr_r <- function(x, include.symbol=TRUE, ...) {
  r2<- rndr_Effect_Size(x, ...)
  if(include.symbol)  paste0("r=", r2)
  else r2
}


#' @rdname rndr_
#' @export
rndr_r2 <- function(x, include.symbol=TRUE, ...
                    #, output = stp25output:::which_output()
                    ) {
r2 <-  rndr_Effect_Size(x, ...)
  if(include.symbol){
   # if(output=="html")
   # HTML macht wenig Sinn da das nur bremsen würde und ich meist html verwende

    paste(paste0(c("R<sup>2</sup>", "adj.R<sup>2</sup>"), "=",
               r2),
        collapse = ", ")}
  else r2
}


#' @rdname rndr_
#' @export
rndr_r2pseudo <- function(x, include.symbol=TRUE, ...) {
  r2<- rndr_Effect_Size(x, ...)
  if(include.symbol)   paste(paste0(names(r2), "=", r2), collapse = ", ")
  else r2
}


#' @rdname rndr_
#' @description rndr_corr
#' Correlations are reported with the degrees of freedom (which is N-2)
#' in parentheses and the significance level:
#' The two variables were strongly correlated, r(55) = .49, p < .01.
#' @export
#' @examples
#' rndr_corr(-.548369,0.005896,55)
rndr_corr <- function(x, p, df){
  paste0("r", rndr_df(df), "=", rndr_Effect_Size(x),", ", rndr_P(p))
}


#' @rdname rndr_
#' @param df,df1,df2 Freiheitsgrade
#' @export
rndr_df<- function(df1, df2=NULL) {
  if(is.null(df2)) paste0("<sub>(", Format2(df1, 0), ")</sub>")
  else  paste0("<sub>(", Format2(df1, 0), ", ", Format2(df2, 0), ")</sub>")

}




#' @rdname rndr_
#' @description F-Wert \code{rndr_F()}
#' @param F_val Objekt aus einem Test
#' @export
#'
#'
rndr_F<-function(F_val, df1, df2, p=NULL){
  F_val<- paste0("F", rndr_df(df1,df2), "=", rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @rdname rndr_
#' @description T-Wert \code{rndr_T()}
#' T Tests are reported like chi-squares, but only the degrees of freedom are
#'  in parentheses. Following that, report the t statistic (rounded to two decimal places)
#'   and the significance level.
#'  There was a significant effect for gender, t(54) = 5.43, p < .001, with men receiving higher scores than women.
#' @export
#' @examples
#' rndr_T(25.45, 45, .0045)
#'
rndr_T<- function(F_val, df1, p=NULL){
  F_val <-paste0("T", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
#' @examples
#' rndr_H(25.45, 45, .0045)
rndr_H<- function(F_val, df1, p=NULL){
  F_val <-paste0("H", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}

#' @rdname rndr_
rndr_BP<- function(F_val, df1, p=NULL){
  F_val <-paste0("BP", rndr_df(df1), "=", rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @rdname rndr_
rndr_DW<- function(F_val, df1, p=NULL){
  F_val <-paste0("DW", #rndr_df(df1),
                 "=", rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
rndr_W<- function(F_val, p=NULL){
  F_val <-paste0("W=", rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
rndr_U<- function(F_val, p=NULL){
  F_val <-paste0("U=",rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}


#' @rdname rndr_
#' @export
rndr_shapiro<- function(F_val, p=NULL){
  F_val <-paste0("W=",rndr_Test_Statistic(F_val))
  if(is.null(p)) F_val
  else paste0(F_val, symbol_seperator, rndr_P(p))
}



#' @rdname rndr_
#' @export
rndr_lm<- function(F_val, df1, df2, p, r2, ar2){
  paste0("R2=",rndr_Effect_Size(r2), symbol_seperator,
         "ad.R2=",rndr_Effect_Size(ar2), symbol_seperator,
         rndr_F(F_val, df1, df2, p) )}





#' @rdname rndr_
#' @export
#' @examples
#' #capture.output(Hmisc::spearman2(pauli~g, data = rechentest))
rndr_X<-function(x, df1, df2=NULL, p=NULL){
  if(is.null(df2)) {
    if(!is.null(df1)) x <- paste0(symbol_chi2(), rndr_df(df1), "=",rndr_Test_Statistic(x))
    else x <- paste0(symbol_chi2(), "=",rndr_Test_Statistic(x))
  } else {
    x <- paste0(symbol_chi2(), rndr_df(df1),"=",rndr_Test_Statistic(x))
  }
  if(!is.null(p))  paste0(x, symbol_seperator, rndr_P(p))
  else x
}


#' @rdname rndr_
#' @export
rndr_Chisq <- function(x, df, p) rndr_X(x, df, NULL, p )


#' @rdname rndr_
#' @export
rndr_Chisq_stars <- function(x, p) {
 # in Kano Benutzt
  paste0(rndr_Test_Statistic(x) , rndr_Stars(p))
}



#' @rdname rndr_
#' @export
rndr_fischer<-function(x, p){
  paste0("OR=", rndr_Test_Statistic(x), symbol_seperator, rndr_P(p))

}

#' @rdname rndr_
#' @description CFA Confirmatorische Faktoranalyse
#'
#' Backhaus Multivariate Analysemethoden 11 AuflageSeite 383
#' GIF Goodness-of-Fit-Index >=.9
#' @examples
#' rndr_gfi_cfa(c(1,.9,.89))
rndr_gfi_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, 0.89, Inf),
      c("nicht akzeptabel", "gut")))


#' @rdname rndr_
#' @description AGIF Adjusted-Goodness-of-Fit-Index
#' @examples
#' rndr_agfi_cfa(c(1,.9,.89))
rndr_agfi_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, 0.89, Inf),
      c("nicht akzeptabel", "gut")))

#' @rdname rndr_
#' @description SRMR
#' @examples
#' rndr_rmsea_cfa(c(1,.9,.89))
rndr_rmsea_cfa <- function(x) as.character(
  cut(x,
      c(-Inf,  0.079, Inf),
      c("gut", "nicht akzeptabel")))

#' @rdname rndr_
#' @description  Chisq_cfa:  Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339
#' CHISQ Chi-Quadrat/df 0,2, 3
#' @examples
#' rndr_Chisq_cfa(c(0,2,3,2.01,3.4))
rndr_Chisq_cfa <- function(x, df=1) as.character(
  cut(x/df,
      c(-Inf, 2, 3, Inf),
      c("gut", "akzeptabel", "nicht akzeptabel"))
)

#' @rdname rndr_
#' @description RMSEA Root-Mean-Square-Error of Approximation 0, 0.050, 0.080
#' @examples
#' rndr_rmsea_cfa(c(0, 0.050, 0.080, .051, .081) )
rndr_rmsea_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, 0.050, 0.08, Inf),
      c("gut", "akzeptabel", "nicht akzeptabel")))

#' @rdname rndr_
#' @description CFI Comparative-Fit-Index .970-1.00, .950-.969
#' @examples
#' rndr_cfi_cfa(c(.970,1.00, .950-.969,.8))
rndr_cfi_cfa <- function(x) as.character(
  cut(x,
      c(-Inf, .950, .970,  Inf),
      c("nicht akzeptabel","akzeptabel", "gut"),
      right=FALSE))

#' @rdname rndr_
#' @description NFI Normed-Fit-Index .950-1.00, .900-.949
#' @examples
#' rndr_nfi_cfa(c(.950, 1.00, .900,  .949))

rndr_nfi_cfa <- function(x) as.character(
  cut(x,
      c( -Inf, .900,  0.950, Inf),
      c("nicht akzeptabel","akzeptabel", "gut"),
      right=FALSE))











# default_stp25()
# default_stp25("lead.zero")
# default_stp25("digits", "p")





















