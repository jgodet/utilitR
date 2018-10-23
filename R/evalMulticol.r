# evalMulticol.r
# written by JuG
# December 18 2017


#' Evaluating multicollinearity using variance inflation factor
#' @author JuG
#' @description Evaluating multicollinearity using variance inflation factor
#' @param mod a lm or a glm object
#' @details calculation based on \code{vif} (package car) - threshold of 2 proposed in \emph{R in action} R.I. Kabacoff (p.200)
#' @examples
#' data(mtcars)
#' mod <- lm(mpg~ .,data=mtcars)
#' evalMulticol(mod)
#'
#' nIndep <- 6
#' mu<-runif(nIndep+1,2,7)
#' sigma<-rep(2, nIndep+1)
#' sample.size<-100
#' dataInd<-as.data.frame(mapply(function(x,y){rnorm(x,y,n=sample.size)},x=mu,y=sigma))
#' colnames(dataInd) <- c('y', paste("X",1:nIndep,sep=''))
#' mod2 <- lm(y~ ., data= dataInd)
#' evalMulticol(mod2)
#'
#' @return dataframe
#' @references \enumerate{
#' \item Fox, J. and Monette, G. (1992) Generalized collinearity diagnostics. JASA, 87, 178–183.
#' \item Fox, J. and Weisberg, S. (2011) An R Companion to Applied Regression, Second Edition, Sage.
#' }
#' @export


evalMulticol<- function(mod, threshold = 2){
  if(!require(car)){install.packages('car')}
  library(car)
  return(as.data.frame(rbind(round(vif(mod),4),as.character(sqrt(vif(mod))>threshold))))
}
