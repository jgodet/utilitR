# stdDiffDotPlot.r
# written by JuG
# May 18 2020


#' Dot plot the absolute standardised differences of variables used to generate the propensity score
#' @author JuG
#' @description Plot (dotplot) of the absolute standardised differences of variables used to generate the propensity score. Standardised differences are calculated using stddiff package. Standardized differences are used to compare the balance in measured baseline covariates. Standardized difference in excess of 10% may be indicative of meaningful imbalance in a covariates between treated and control subjects
#' @param data dataframe
#' @param treat name of the treatment variable
#' @details
#' @examples
#' #synthetic data
#' set.seed(2020)
#' treat<-round(abs(rnorm(100)+1)*10,0) %% 2
#' numeric1<-round(abs(rnorm(100)+1)*10,0)
#' numeric2 <- round(abs(rnorm(100)+1)*10,0)
#' binary<-factor(round(abs(rnorm(100)+1)*10,0) %% 2)
#' category<-factor(round(abs(rnorm(100)+1)*10,0) %% 3)
#' data<-data.frame(treat,numeric1, numeric2,binary,category)
#'
#' @import stddiff
#' @return data.frame
#' @export


stdDiffDotPlot<- function(data, treat = "treat", showRes = TRUE, plotGraph = TRUE, ... ){

  if(!require('stddiff')){install.packages('stddiff')}
  if(!require('dplyr')){install.packages('dplyr')}
  resData.frame <- data.frame(mean.c=NA,sd.c=NA, mean.t=NA,  sd.t=NA,p.c=NA, p.t=NA, missing.c=NA, missing.t=NA,
                              stddiff = NA, stddiff.l=NA, stddiff.u=NA)

  indTreat <- which(names(data)==treat)

  colclasse <- sapply(data,class)
  #recodage class en "factor", "numeric","integer" uniquement
  if(any(!colclasse %in% c("factor", "numeric","integer" ))){
    return("Class must be factor, numeric or integer")
  }
  colclasse[!colclasse%in%"factor"] <- "numeric"
  colclasse[colclasse%in%"factor"] <- "category"
  indBin <- which(sapply(data[colclasse%in%"category"], nlevels)==2)
  colclasse[colclasse%in%"category"][indBin] <- "binary"
  colclasse[indTreat] <- "treat"

  stdnum <- as.data.frame(stddiff.numeric(data=data,gcol=indTreat,vcol=c(which(colclasse%in%"numeric"))))
  j = dim(stdnum)[1]
  resData.frame[1:j, -6:-5] <- stdnum
  rnames <- rownames(stdnum)

  stdbin <- as.data.frame(stddiff.binary(data=data,gcol=indTreat,vcol=c(which(colclasse%in%"binary"))))
  k = dim(stdbin)[1]
  resData.frame[(j+1):(j+k), -4:-1] <- stdbin
  rnames <- c(rnames,rownames(stdbin))

  stdcat <- as.data.frame(stddiff.category(data=data,gcol=indTreat,vcol=c(which(colclasse%in%"category"))))
  l = dim(na.omit(stdcat))[1]
  resData.frame[(j+k+1):(j+k+l), -4:-1] <- na.omit(stdcat)
  rnames <- c(rnames,names(data)[which(colclasse%in%"category")])

  rownames(resData.frame) <- rnames

  }




  return()
}
