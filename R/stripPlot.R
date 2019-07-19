# stripPlot.r
# written by JuG
# Fri Dec  1 21:58:59 2017"



#' Plot a customized stipchart
#' @author JuG
#' @description Plot a customized stipchart
#' @param datafr data.frame
#' @param formula  a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor)
#' @param col   if col is non-null it is assumed to contain colors to be used
#' @param addBoxplot if TRUE, a bxplot (package beeswarm) is drawn for each factor modalities
#' @param grid if TRUE, draw a grid
#' @return customized stripchart graphics
#' @examples
#' dtf <- data.frame(val1 = rnorm(1000,1,.2), fact2 = LETTERS[gl(n = 5,k = 200,length = 1000)])
#' if(!require(colorRamps)){install.packages('colorRamps')}
#' require(colorRamps)
#' stripPlot(data = dtf, val1 ~ fact2, addBoxplot = TRUE,col=matlab.like(12))
#'
#' if(!require(RColorBrewer)){install.packages('RColorBrewer')}
#' library("RColorBrewer")
#' stripPlot(data = dtf, val1 ~ fact2,col=brewer.pal(n = 5, name = "Dark2"), jitter = .2,grid = T)
#'
#' @export


stripPlot <- function(data, formula, col = NULL, addBoxplot = FALSE, jitter = .3, grid = FALSE, xlab=NULL, ylab=NULL,xlim=NULL, ylim=NULL,...){
  if(!require(beeswarm)){install.packages('beeswarm')}
  require(beeswarm)
  if (is.null(col)){
    col = 1:10
  }
  if(grid){
    stripchart(  formula,data=data,method='jitter',jitter= jitter,vertical=TRUE,
             pch=21, bg=rgb(.7,.7,.7,.8),col=col,panel.first= grid(),...)
  }else{
    stripchart(  formula,data=data,method='jitter',jitter= jitter,vertical=TRUE,
                 pch=21, bg=rgb(.7,.7,.7,.8),col=col,...)
  }
  if(addBoxplot){
    bxplot( formula,data=data,add=TRUE)
  }

}
