# stdDiffDotPlot.r
# written by JuG
# May 18 2020


#' Plot the absolute standardised differences of variables used to generate the propensity score
#' @author JuG
#' @description Plot (dotchart) the absolute standardised differences of variables used to generate the propensity score. Standardised differences are calculated using stddiff package. Standardized differences are used to compare the balance in measured baseline covariates. Standardized difference in excess of 10% may be indicative of meaningful imbalance in a covariates between treated and control subjects
#' @param dataSMD dataframe of SMD results
#' @param data dataframe
#' @param treat name of the treatment variable
#' @param showRes boolean if true print result table
#' @param plotGraph boolean if true show graph in the output
#' @param lateX boolean if true print result table in lateX format
#' @param inset inset for legend position relative to the bottom right
#' @param xlim xlim see ?par
#' @param col colors
#' @details
#' @examples
#' #synthetic data
#' set.seed(2020)
#' treat<-round(abs(rnorm(100)+1)*10,0) %% 2
#' numeric1<-round((rnorm(100)+1)*10,0)
#' numeric2 <- round(abs(rnorm(100)+1)*10,0)
#' binary<-factor(round(abs(rnorm(100)+1)*10,0) %% 2)
#' binary2<-factor(round(abs(rnorm(100)+1)*10,0) %% 2)
#' category<-factor(round(abs(rnorm(100)+1)*10,0) %% 3)
#' category2<-factor(round(abs(rnorm(100)+1)*10,0) %% 5)
#' data<-data.frame(treat,numeric1, numeric2,binary, binary2,category, category2)
#'
#'
#' stdDiffDotPlot(data = data, treat = "treat", showRes = TRUE, plotGraph = FALSE)
#' stdDiffDotPlot(data = data, treat = "treat", showRes = FALSE, plotGraph = TRUE, xlim=c(-.5,1), pch=21, bg="lightgrey", col=c("blue","black"))
#' #res <- stdDiffDotPlot(data = data, treat = "treat", showRes = TRUE, plotGraph = FALSE)
#' #segments(x0=res$stddiff.l, x1=res$stddiff.u, y0=1:6)
#'
#'
#' data2 <- data.frame(age=c(rnorm(2929, 69.6, 13.5), rnorm(6178, 65, 13.3)), ttt=c(rep("NobetaBlock",2929), rep("betaBlock", 6178)))
#' stdDiffDotPlot(data = data2, treat = "ttt", showRes = TRUE, plotGraph = FALSE)
#'
#' example 2 : with dataSMD
#' dataSMD = data.frame(unmatched = c(.4,.2,.25,.3),
#'    matched = c(.1,.1,.05,.02),
#'    weighted = c(.02,.05,.08,.03))
#' rownames(dataSMD) <- c("Age", "Height", "Weight", "ParamX")
#'
#' stdDiffDotPlot(dataSMD=dataSMD,col=c("black", "blue", "purple") )
#' @import stddiff
#' @return data.frame
#' @export


stdDiffDotPlot <- function(dataSMD, data, treat = "treat", showRes = TRUE, plotGraph = TRUE,lateX=FALSE,inset=.02, xlim=c(0,.5), col,... ){

  if(!require('stddiff')){install.packages('stddiff')}
  if(!require('dplyr')){install.packages('dplyr')}
  if(!require('xtable')){install.packages('xtable')}
  if(!missing(data) & missing(dataSMD)){
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

  if(sum(colclasse%in%"numeric")>0){
    stdnum <- as.data.frame(stddiff.numeric(data=data,gcol=indTreat,vcol=c(which(colclasse%in%"numeric"))))
    j = dim(stdnum)[1]
    resData.frame[1:j, -6:-5] <- stdnum
    rnames <- rownames(stdnum)
  }

  if(sum(colclasse%in%"binary")>0){
    stdbin <- as.data.frame(stddiff.binary(data=data,gcol=indTreat,vcol=c(which(colclasse%in%"binary"))))
    k = dim(stdbin)[1]
    resData.frame[(j+1):(j+k), -4:-1] <- stdbin
    rnames <- c(rnames,rownames(stdbin))
  }
  if(sum(colclasse%in%"category")>0){
    stdcat <- as.data.frame(stddiff.category(data=data,gcol=indTreat,vcol=c(which(colclasse%in%"category"))))
    l = dim(na.omit(stdcat))[1]
    resData.frame[(j+k+1):(j+k+l), -4:-1] <- na.omit(stdcat)
    rnames <- c(rnames,names(data)[which(colclasse%in%"category")])
  }
  rownames(resData.frame) <- rnames

  resData.frame <- resData.frame[ order(resData.frame$stddiff),]

  if(plotGraph){
    dotchart(resData.frame$stddiff,labels = rownames(resData.frame),xlim=xlim,...)
  }
  if(showRes){
    if(lateX){
      xtable(resData.frame)
    }else{
      return(resData.frame)
    }
  }
  }
  if(missing(data) & !missing(dataSMD)){
    dataSMD <- dataSMD[order(dataSMD[,1]),]
    if(missing(col)){
      col <- 1:ncol +1
    }
    if(plotGraph){
      dotchart(dataSMD[,1],labels = rownames(dataSMD),
              xlab="Absolute standardized difference",xlim=xlim,col=col[1],...)
      ncol <- dim(dataSMD)[2]
      nvbl <- dim(dataSMD)[1]

      for (i in 2:ncol){
        points(dataSMD[,i],1:nvbl, col=col[i], pch=13+i)
      }

      abline(v=.1, lty=2)
      legend("bottomright", pch = c(1,(13+2):(13+ncol)),
             col=c("black", col[2:ncol]),
             legend = colnames(dataSMD),inset=inset,...)
    }

    if(showRes){
      if(lateX){
        xtable(dataSMD)
      }else{
        return(dataSMD)
      }
    }
  }
}


