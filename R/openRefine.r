# openRefine.r
# written by JuG
# December 17 2017


#'Connect to openRefine API
#'@author JuG
#'@description Connect to openRefine API to import, export or delete a project in OpenRefine directly from an R script
#'@param data a data.frame
#'@details Connect to openRefine API \url{https://cran.r-project.org/web/packages/rrefine/vignettes/rrefine-vignette.html}. openRefine must be installed on the local machine \url{http://openrefine.org/download.html}.
#'@examples
#' #devtools::install_github("vpnagraj/rrefine")
#' library(rrefine)
#' lateformeeting[1:10,]
#' openRefine(lateformeeting)
#'
#'@return void
#'@export


openRefine<- function(data){

  #https://cran.r-project.org/web/packages/rrefine/vignettes/rrefine-vignette.html

  write.csv(data, file = "dataTemp.csv", row.names = FALSE)
  refine_upload(file = "dataTemp.csv", project.name = "data_cleanup", open.browser = TRUE)
  cat("Run closeRefine() when ready...\n")
}
