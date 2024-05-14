# rmBtwBrackets.r
# written by JuG
# May 14 2024


#' Remove text between brackets
#' @author JuG
#' @description Remove text between brackets
#' @param x a string with text to remove inside brackets
#' @details
#' @examples
#' stringX <- c("a", "b (in mL)", "cd")
#' rmBtwBrackets(stringX)
#'
#' @return vector of characters
#' @export


rmBtwBrackets<- function(x){
  return(gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(x)))
}
