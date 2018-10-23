# newFun.r
# written by JuG
# December 16 2017


#' Print a skeleton for a new function in the console
#' @author JuG
#' @description Print a skeletton for a new function in the console
#' @param name A name for the function
#' @details >Sys.setenv(TZ="Europe/Berlin") >Sys.setlocale("LC_TIME", "C"); to get UK date format
#' @examples
#' newFun("newFunction")
#' newFun("newFunction",createFile = TRUE)
#'
#' @return void
#' @export



newFun <- function(name, createFile = FALSE,path=NULL){
  Sys.setenv(TZ="Europe/Berlin")
  Sys.setlocale("LC_TIME", "C");
  dte<-format(Sys.time(),"%B %d %Y")

  if(createFile){
    if(is.null(path)){
      path <- paste(getwd(),"/R/",as.character(name),".r",sep="")
    }
    cat(file = path, "# ",as.character(name),".r\n# written by JuG\n# ",dte,"\n\n\n#' Do something\n#' @author JuG\n#' @description \n#' @param \n#' @details \n#' @examples \n#'\n#'\n#' @return \n#' @export\n\n\n",name, "<- function(){\n\n  return()\n}",sep="")
  }else{
    cat("# ",as.character(name),".r\n# written by JuG\n# ",dte,"\n\n\n", sep="")
    cat("#' Do something\n#' @author JuG\n#' @description \n#' @param \n#' @details \n#' @examples \n#'\n#'\n#' @return \n#' @export\n\n\n")
    cat(name, "<- function(){\n\n  return()\n}")
  }
}

