# plotlm.r
# written by JuG
# December 20 2017


#' Plot 2D regression line with confidence intervals
#' @author JuG
#' @description Plot 2D regression line with confidence intervals
#' @param x
#' @param y
#' @param data
#' @param prediction if TRUE, prediction intervals are drawn
#' @details
#' @examples
#' nbPoints = 20
#' x <-seq(1,10, length = nbPoints)
#' y <- 3 * x + rnorm(nbPoints, 0,6) + 10
#' plotlm(x,y,ylim=c(0,50),main="plotlm example",prediction=T)
#' @return
#' @export


plotlm<- function(x,y,data=NULL,prediction = FALSE,...){

  if(is.null(data) & !missing(x) & !missing(y)){
    df <- data.frame(x,y)
  }
  # model
  mod <- lm(y ~ x, data = df)

  # predicts + interval
  if(!is.null(data)){
    newx <- seq(min(df$x), max(df$x), length.out=100)
  }else{
    newx <- seq(min(x), max(x), length.out=100)
  }

  preds <- predict(mod, newdata = data.frame(x=newx),
                   interval = 'confidence')
  predsP <- predict(mod, newdata = data.frame(x=newx),
                   interval ="prediction")
  # plot
  plot(y ~ x, data = df, type = 'n',...)
  # add fill
  polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
  # model
  abline(mod)
  # intervals
  lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
  lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
  points(y ~ x, data = df,pch=19,col='blue')
  if(prediction){
    lines(newx,predsP[,3],lty=2)
    lines(newx,predsP[,2],lty=2)
  }
}
