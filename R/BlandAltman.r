# BlandAltman.r
# written by JuG
# February 16 2019


#' Perform and plot Band and Altman analysis
#' @author JuG
#' @description
#' @param data1 vector of data1
#' @param data2 vector of data2
#' @param alpha type I error
#' @param rep.meas boolean true if data are repeated measurements
#' @param subject
#' @param silent boolean if FALSE, parameters list is not returned
#' @details
#' @examples
#' data1 <- rnorm(20)
#' data2 <- rnorm(20,mean = 1)
#' BlandAltman(data1,data2, ylim=c(-5,5))
#' @return
#' @export


BlandAltman<- function (data1, data2, alpha = 0.05, rep.meas = FALSE, subject, silent=FALSE,...)
{ x <- data1
  y <- data2
  z <- qnorm(1 - alpha/2)
  d <- x - y
  m <- (x + y)/2
  diff.moy <- mean(d, na.rm = TRUE)
  if (!rep.meas) {
    et.d = sqrt(var(d, na.rm = TRUE))
  }
  else {
    if (!is.factor(sujet))
      sujet <- as.factor(sujet)
    n <- length(levels(sujet))
    modele <- aov(d ~ sujet)
    MSB <- anova(modele)[[3]][1]
    MSW <- anova(modele)[[3]][2]
    paires <- NULL
    for (i in 1:length(levels(as.factor(sujet)))) {
      paires[i] <- sum(is.na(d[sujet == levels(sujet)[i]]) ==
                         FALSE)
    }
    Sig.dl <- (MSB - MSW)/((sum(paires)^2 - sum(paires^2))/((n -
                                                               1) * sum(paires)))
    et.d <- sqrt(Sig.dl + MSW)
  }
  bsup <- diff.moy + z * et.d
  binf <- diff.moy - z * et.d
  plot(m, d, abline(h = c(diff.moy, bsup, binf), col = "red"),
       lty = c(1, 2, 2), pch = 16, main = "Bland Altman plot", ...)
  valeurs <- round(cbind(binf, diff.moy, bsup), 4)
  colnames(valeurs) <- c("Borne inf", "Moyenne", "Borne sup")
  if (!rep.meas)
    sortie <- list(limites = valeurs, variance = et.d^2)
  else sortie <- list(limites = valeurs, variance = Sig.dl)
  return(sortie)
}
