# closeRefine.r
# written by JuG
# December 17 2017


#' Close connection to openRefine API
#' @author JuG
#' @description Get openRefine cleaned data.frame and close
#' @param dataFrameOutput if TRUE, return a data.frame (a tibble otherwise)
#' @param kill if TRUE delete openRefine project
#' @details ...
#' @examples
#' cleanedData <- closeRefine(dataFrameOutput = TRUE)
#'
#' @return data.frame or tibble
#' @export


closeRefine<- function(dataFrameOutput = TRUE,kill=FALSE){
  lfm_clean <- refine_export(project.name = "data_cleanup",)

  if(dataFrameOutput){
    out <- as.data.frame(lfm_clean)
    for (i in factOrNot(out,arg = "character",ind = T)){
      out[,i] <- as.factor(out[,i])
    }
    return(out)

  }else{
    return(lfm_clean)
  }
  if(kill){refine_delete(project.name = "data_cleanup")}

}
