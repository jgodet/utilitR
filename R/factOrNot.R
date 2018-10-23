# factOrNot.r
# written by JuG
# Fri Dec  1 21:58:59 2017"



#' Define class of data.frame variables
#' @author JuG
#' @description Merge several data.frames
#' @param datafr data.frame
#' @param arg  class to be evaluated c("factor","Date","integer","numeric","character")
#' @param ind   if TRUE return index, else return names
#' @return list or names of columns
#' @examples
#' library(survival)
#' data(cgd)
#' factOrNot(cgd)
#' factOrNot(cgd,arg = "Date",ind=T)

#' @export

factOrNot<-function(datafr,arg="factor",ind=FALSE){         #arg="c(factor","Date","integer","numeric")
  if(ind){
    res <- as.vector(which(lapply(datafr,class)==arg))
  }else{
    res <- names(datafr)[lapply(datafr,class)==arg]
  }
  return( res)
}
