#summaryOR.r
# written by JuG
#Thu Dec 14 16:54:12 2017


#'Odds ratio summary statistics
#'#' @author JuG
#' @description Odds ratio summary statistics
#' @param glm.fit   an object of class "glm.fit" calculated with a "binomial "link function.
#' @param lateX if TRUE, return lateX xtable of the result table
#' @param graph if TRUE, plot the coefficients of a model with \emph{broom} and \emph{ggplot2}
#' @details  Provide Odds ratio  (exponential of the glm.fit coefficients) and their confidence intervals (given by \code{\link{confint}}). Stars are "***" if p-value < 0.001, "**" if 0.001 < p-value <= 0.01, "*" if 0.01 < p-value <= 0.05, "." if 0.05 < p-value <=0.1
#' @examples
#' y <- rbinom(n = 50, size = 1, prob = .3)
#' x1 <- rnorm(50, 5,1)
#' x2 <- runif(50, 0,5) * (1 + 2*y)
#' x3 <- rnorm(50, 12,5)
#'  Data <- data.frame(y,x1,x2,x3)
#'
#'  logist <- glm(y~., data=Data, family="binomial")
#'  summary(logist)
#'  summaryOR(glm.fit=logist,lateX=FALSE,graph=FALSE)
#' @export


summaryOR <- function(glm.fit, lateX = TRUE, graph = TRUE,...){
  if(!require(xtable)){install.packages('xtable')}
  require(xtable)
  if(!require('broom')){install.packages('broom')}
  require(broom)
  if(!require('GGally')){install.packages('GGally')}
  require(GGally)

  etoiles <- ifelse(summary(glm.fit)$coefficients[,4] < 0.001,"***",
                    ifelse(summary(glm.fit)$coefficients[,4] < 0.01,"**",
                           ifelse(summary(glm.fit)$coefficients[,4] < 0.05,"*",
                                  ifelse(summary(glm.fit)$coefficients[,4] < 0.1,".","")
                           )
                    )
  )
  results <- as.data.frame(cbind(round(exp(cbind(coef(glm.fit), suppressMessages(confint.default(glm.fit)))),3),round(summary(glm.fit)$coefficients[,4],4),noquote(etoiles)))
  colnames(results) <- c("Odds Ratio", "2.5 %", "97.5 %","p.value","" )
  results$p.value <- ifelse(as.numeric(as.character(results$p.value))<0.001,"<0.001",round(as.numeric(as.character(results$p.value)),3))

  if(graph){
    print(ggcoef(glm.fit,exponentiate = T,vline_color = "red",
           vline_linetype =  "solid",
           errorbar_color = "blue",
           errorbar_height = .25,color = "purple", size = 5, shape = 18))
  }

  if(lateX){
    return(xtable::print.xtable(xtable::xtable(results),...))
  }else{
    return(results)
  }


}




