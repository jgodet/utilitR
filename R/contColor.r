# contColor.r
# written by JuG
# April 15 2019


#' Encode colour for plotting a continuous variabe
#' @author JuG
#' @description Encode colour for plotting a continuous variable
#' @param num a continuous variable
#' @param palette a palette like terrain.color(n), ...
#' @details
#' @examples
#'x <- runif(n = 100, min = 1, max = 100)
#'plot(x,x,col= contColor(num = x))
#'plot(x,x,col= contColor(num = x, palette = terrain.colors(10)))
#' @return
#' @export


contColor<- function(num, palette = matlab.like(100)){
  if(!require('colorRamps')){install.packages('colorRamps')}
  require(colorRamps)
  n <- length(palette)
  cutNum <- cut (num, n)
  return(palette[cutNum])
}
