# isDate.r
# written by JuG
# December 21 2020


#' Test if a vector can be a class Date
#' @author JuG
#' @description
#' @param x a vector
#' @param date.format 	a character string as in as.Date
#' @examples
#' dates <- c("02/27/92", NA, "01/14/92", "02/28/92", "02/01/92")
#' isDate(dates, date.format= "%m/%d/%y")
#'
#' data <- data.frame( Date=c("10/11/2012","10/12/2012", "25/12/2012"),
#'                     AE=c(1211,100,23),
#'                     Percent=c(0.03,0.43,"a"),
#'                     Date2=c("10/27/2012","10/12/12","12/25/12" ))
#' sapply(data,isDate)
#' sapply(data,isDate, date.format= "%m/%d/%y")

#' @return vector
#' @export


isDate <- function(x,date.format){
  if(missing(date.format)){
    yn <-   tryCatch(!all(is.na(as.Date(x))),
                     error = function(err) {FALSE})
  }else{
    yn <-  tryCatch(!all(is.na(as.Date(x,date.format ))),
                    error = function(err) {FALSE})
  }
  return(yn)
}
