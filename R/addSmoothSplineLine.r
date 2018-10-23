# addSmoothSplineLine.r
# written by JuG
# December 20 2017


#' Add a fitted cubic smoothing spline to the supplied graph
#' @author JuG
#' @description Add a fitted cubic smoothing spline to the supplied graph
#' @param y yvalues
#' @param x (optional) x values
#' @details ...
#' @examples
#' n <- 1e3
#' dat <- data.frame(
#'   x = 1:n,
#'   y = sin(seq(0, 5*pi, length.out = n)) + rnorm(n=n, mean = 0, sd=0.1)
#' )
#' plot(dat)
#' addSmoothSplineLine(dat$y,col='red')
#' @export


addSmoothSplineLine<- function(y, x = NULL,col="blue",lwd=2,...){
  yss <- smooth.spline(y)
  if(!is.null(x)){
   lines(x,yss, col=col,lwd=lwd)
  }else{
    lines(yss, col=col,lwd=lwd)
  }
  return()
}
