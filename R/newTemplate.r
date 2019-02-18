# newTemplate.r
# written by JuG
# February 18 2019


#' Create a R script template
#' @author JuG
#' @description Create a R script template
#' @param path R script path
#' @param name R script name
#' @param createFile boolean
#' @details
#' @examples
#' newTemplate(name="test")
#'
#' @return
#' @export


newTemplate<- function(path, name, createFile = F){
  Sys.setenv(TZ="Europe/Berlin")
  Sys.setlocale("LC_TIME", "C");
  dte<-format(Sys.time(),"%B %d %Y")

  if(createFile){
    if(is.null(path)){
      path <- paste(getwd(),"/R/",as.character(name),".r",sep="")
    }
    cat(file = path, "# ",as.character(name),".r\\n# written by JuG\\n# ",dte,"\\n")
    }else{
      cat("# ",as.character(name),".r\n# written by JuG\n# ",dte,"\n",sep='')
      }
}
