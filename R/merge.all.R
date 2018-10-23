# merge.all.r
# written by JuG
# Fri Dec  1 21:58:59 2017"



#' Merge several data.frames
#' @author JuG
#' @description Merge several data.frames
#' @param by  variable to merge on
#' @examples
#'  a <- data.frame("USUBJID" = 1:10, val1 = rnorm(10,1,.2))
#'  b <- data.frame("USUBJID" = 1:10, val2 = LETTERS[1:10])
#'  c <- data.frame("USUBJID" = 1:10, val3 = gl(n = 5,k = 2,length = 10))
#'  merged<-merge.all(by="USUBJID",a,b,c)
#' @return merged data.frame
#' @export

#fonction pour merger plusieurs bases
merge.all <- function(by, ...) {
  frames <- list(...)
  return (Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames))
}  # end merge.all
