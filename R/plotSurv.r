# plotSurv.r
# written by JuG
# December 17 2017


#' Plot survival curves
#' @author JuG
#' @description Plot survival curves
#' @param fit a survfit object
#' @details code from \url{http://www.sthda.com/english/wiki/survival-analysis-basics}
#' @examples
#' require(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data = lung)
#' plotSurv(fit)
#' fit <- survfit(Surv(time, status) ~ 1, data = lung)
#' @return graph
#' @export


plotSurv<- function(fit,pval = TRUE,... ){
  if(!require(ggplot2)){install.packages('ggplot2')}
  require(ggplot2)
  if(!require(survminer)){install.packages('survminer')}
  require(survminer)
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}
  library(RColorBrewer)
  coul <- brewer.pal(n = 8, name = "Set1")
  ggsurvplot(fit,pval=pval,
             conf.int = TRUE,
             risk.table = TRUE, # Add risk table
             risk.table.col = "strata", # Change risk table color by groups
             linetype = "strata", # Change line type by groups
             surv.median.line = "hv", # Specify median survival
             ggtheme = theme_bw(), # Change ggplot2 theme
             palette = coul,...)
}
