# is.01.r
# written by JuG
# November 21 2020


#' Check if a variable is 0/1 encoded
#' @author JuG
#' @description
#' @param x a vector
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @details
#' @examples
#'testDf <- data.frame(a = runif(10),
#'                     b = rbinom(10, size = 1, prob = .2),
#'                     c = as.Date(Sys.time())- 10:1,
#'                     d = factor(x = rep(3,10)),
#'                     e = NA,
#'                     f = rep(0,10),
#'                     g = rep(1,10),
#'                     h = sample(x = c(0,1,NA), size = 10, replace = T),
#'                     i = sample(x = c(0,NA), size = 10, replace = T),
#'                     j = sample(x = c(1,NA), size = 10, replace = T))
#'sapply(testDf, FUN=is.01)
#' @return boolean
#' @export


is.01 <- function(x, na.rm=FALSE){
  returnedVal <- FALSE
  if(na.rm){
    x <- na.omit(x)
  }
  xf <- as.factor(x)
  nlev <- length(levels(xf))
  #nlev == 0 if all NA
  if(nlev<3 & nlev > 0){
    lab <- names(table(x))
    if(all(lab %in% c("0", "1")))
      returnedVal <- TRUE
  }
  return(returnedVal)
}
