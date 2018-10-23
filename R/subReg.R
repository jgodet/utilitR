# allsubreg.r
# written by JuG
# Fri Dec  1 21:58:59 2017"



#' Functions for model selection
#' @author JuG
#' @description Model selection by exhaustive search. All subset regression are evaluated to help choosing the best minimalist model.    R in action p 211.
#' @param formula The models fit by, e.g., the lm and glm functions  specified in a compact symbolic form.
#' @param data data.frame
#' @param nbest  number of subsets of each size to record
#' @return graphic for best model selection based on Adjusted R-squre
#' @examples
#'
#' form = as.formula("Fertility ~ .")
#'
#' if(!require('MASS')){install.packages('MASS')}
#' library('MASS')
#' fit1 <- lm(form, data = swiss)
#' stepAIC(fit1,direction = "both")
#'
#' subReg(form, data=swiss, nbest = 3)
#'
#' relweights(fit1, col="lightgrey",las=1)
#'
#' @export



subReg<-function(formula,data,nbest){

  if(!require('leaps')){install.packages('leaps')}
  require(leaps)

  l<-regsubsets(formula,data=data,nbest=nbest)
  plot(l, scale = "adjr2")
}
