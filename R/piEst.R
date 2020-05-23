# piEst.r
# written by JuG
# "Mon Dec  4 17:57:41 2017"



#' Estimate population proportion and interval limits
#' @author JuG
#'
#' @description  Estimate population proportion and interval limits from observede counts of succes and trials.
#' @param y counts of successes
#' @param n counts of trials
#' @param beta.priors vector of 2 non-negative parameters of the Beta prior distribution (shapes - by default Jeffrey's c(0.5,0.5))
#' @param credMass mass of the HDI region
#' @param showPlot if TRUE, show a graphical representation of Pi distribution
#' @param lateX if TRUE, print lateX output
#' @return Proportion estimates with HDI, CI (quantile c(.025,0.975)), and binomial CI (from binom.test)
#'
#' @examples
#' piEst(y = 3, n = 12, showPlot = TRUE)
#'
#' @export



piEst <- function(y=12, n=20, beta.priors =c(.5,.5),credMass = 0.95, showPlot = FALSE, lateX = FALSE, ...){

  if(!require('stargazer')){install.packages('stargazer')}
  library('stargazer')
  if(!require('kableExtra')){install.packages('kableExtra')}
  library('kableExtra')
  kableExtra
  # likelihood p
  p <- y/n
  alpha.lkhood <- y
  beta.lkhood <- n-y

  #priors
  alpha.prior <- beta.priors[1]
  beta.prior <- beta.priors[2]

  #posterior
  alpha.post <-  alpha.lkhood +  alpha.prior
  beta.post <- beta.lkhood + beta.prior
  E <- (alpha.lkhood  +  alpha.prior)/( beta.lkhood + alpha.lkhood + alpha.prior + beta.prior)

  hdi <- HDIofICDF( qbeta , shape1 = alpha.post , shape2 = beta.post,credMass = credMass )
  xx <- seq(0,1,by=.001)

  ci <- quantile(rbeta(3.10^6,alpha.post, beta.post),c((1-credMass)/2,1-(1-credMass)/2))
  bt <- binom.test(x = y,n = n,conf.level =credMass )


  res <- data.frame(p_obs = round(p,4), Pi.est = round(E,4), HDI = round(hdi,4),CI =round(unname(ci),4), Binom = round(bt$conf.int[1:2],4))
  res[2,1:2] <- ""
  if(lateX){
    kable(t(res), format = "latex")
  }else{
    stargazer(t(res), type="text",single.row=FALSE,summary=FALSE)
  }

  if(showPlot){
    curve(dbeta(x,alpha.post, beta.post),from=0, to =1,xlab="",ylab="Density",
          main=expression(paste("Estimation of ", pi)),
          sub = paste("HDI (red): ",round(hdi[1],3), " - ", round(hdi[2],3),"\n CI.95% (blue): ",round(ci[1],3), " - ", round(ci[2],3))
          )
    abline(v=E)
    segments(x0 = hdi[1] , y0 = 0.5, x1 = hdi[2], lwd=5,col='red')
    abline(v = c(hdi[1],hdi[2]), lty= 2, col ="red")
    segments(x0 = ci[1] , y0 = -.05, x1 = ci[2], lwd=5,col='blue')
    abline(v = c(ci[1],ci[2]), lty= 2, col ="blue")
  }

}











