# tonum.r
# written by JuG
# December 16 2017


#' Convert ill-encoded factor to numeric
#' @author JuG
#' @description Convert ill-encoded factor to numeric
#' @param data a vector (character, factor, oredered) or a data.frame
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector.
#' @param replacement a replacement for matched pattern
#' @param comma2period change "," to "."
#' @details Apply \code{as.numeric(as.character())}
#' @examples
#' devtools::install_github("vpnagraj/rrefine")
#' library(rrefine)
#' lateformeeting$sleephours
#' tonum(lateformeeting$sleephours)
#' tonum(lateformeeting$sleephours,pattern = c("six", "7 and a half"), replacement = c("6","7.5"))
#'
#' dosage <- c( 1.2, 1,5, 5.6, "<1", 12, "13,2", 1.1,12, "<1",3, ">50") #in pg
#' tonum(dosage, pattern = c("<1",">"), replacement = c("0",""))
#' semiquant <- ordered(cut(tonum(dosage, pattern = c("<1",">"), replacement = c("0","")),c(-0.1,1,5,100),labels = c("<1 pg","1-5 pg",">5 pg")))
#' data.frame(dosage,semiquant)
#'
#' dosageData <- data.frame(dosage1 = dosage, dosage2= sample(dosage))
#' tonum(dosageData) #no replacement
#' tonum(dosageData, pattern = c("<1",">"), replacement = c("0","")) #with replacemen
#'
#'    @return numeric() vector or matrixaze
#' @export


tonum<- function(data, pattern = NULL, replacement = NULL, comma2period = TRUE){
  transftonum <- function(x){
    if(class(data)[1] == "ordered"){
      return(as.numeric(data))
    }else{
      char <- as.character(x)
      if(comma2period){
        char<- gsub(pattern = ",",replacement = ".",char)
      }
      return(as.numeric(char))
    }
  }

  if(!class(data)[1] %in% c("character","factor","ordered","data.frame")){
    cat("Class of data is not correct\ncheck !class(data) %in% c(\"character\",\"factor\", \"ordered\",\"data.frame\")")
    return()
  }

  if(!is.null(pattern) & !is.null(replacement)){
    if(length(pattern) != length(replacement)){
      cat("Replacement not done : length of pattern differs from length of replacement")
    }else{
      if(class(data)=="data.frame"){
        for (j in 1:dim(data)[2]){
          for (i in 1:length(pattern)){
            data[,j] <- gsub(pattern = pattern[i], replacement = replacement[i], data[,j])
          }
        }

      }else{
        for (i in 1:length(pattern)){
          data <- gsub(pattern = pattern[i], replacement = replacement[i], data)
        }
      }
    }
  }

  if(class(data)[1] %in% c("character","factor", "ordered")){
    num <- transftonum(data)
  }else{
    num <- apply(data,2,FUN=transftonum)
  }
  return(num)
}
