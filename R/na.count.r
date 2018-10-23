# na.count.r
# written by JuG
# December 16 2017


#' Count missing data
#' @author JuG
#' @description Count missing data
#' @param data a vector (numeric, factor or ordered), a matrix, a data.frame or a list
#' @details key function  \code{nafun <- function(x){sum(is.na(x))}}
#' @examples
#' #with data.frame
#' data3 <- data.frame(a = c(1,2,3), b= c("e",NA,3), c = 1:3)
#' na.count(data3)
#' #with list
#' data4 <- list(a = c(1,2,3), b= c("e",NA))
#' na.count(data4)
#'
#' @return vector of missing value per variable
#' @export

na.count<- function(data){
  nafun <- function(x){sum(is.na(x))}
  if(!class(data)[1] %in% c("numeric","factor", "ordered","matrix","data.frame","list")){
    cat("Class of data is not correct\ncheck !class(data) %in% c(\"numeric\",\"factor\", \"ordered\",\"matrix\", \"data.frame\", \"list\")\n")
    return()
  }
  if(class(data)[1] %in% c("numeric","factor","ordered")){
    naSum <- nafun(data)
  }else if(class(data)=="matrix" | class(data)=="data.frame"){
    naSum <- apply(data,2,FUN=nafun)
  }else{
    naSum <- sapply(data,FUN=nafun)
  }
  return(naSum)
}

