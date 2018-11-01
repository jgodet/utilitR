# mkdirPaper.r
# written by JuG
# November 01 2018


#' Create an organised folder for publications data
#' @author JuG
#' @description
#' @param path path to folder where to create mkdirPaper
#' @param name name of the folder
#' @details
#' @examples
#'path = getwd()
#'name = "testFolder"
#'mkdirPaper(path,name = name)
#' @return
#' @export


mkdirPaper<- function(path, name){
  if(file.exists(path)){
    setwd(path)
    dir.create(name, showWarnings = TRUE)
    setwd(paste(path,"/",name,sep=''))
    dir.create("code", showWarnings = TRUE)
    dir.create("biblio", showWarnings = TRUE)
    dir.create("figures", showWarnings = TRUE)
    dir.create("manuscript", showWarnings = TRUE)
    dir.create("data", showWarnings = TRUE)
    dir.create("submitted", showWarnings = TRUE)
    dir.create("proof", showWarnings = TRUE)
    dir.create("bordel")
  }else{
    return()
  }
  return(cat("dir ", name, " created as ", paste(path,"/",name,sep='') ))
}
