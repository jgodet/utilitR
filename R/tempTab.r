# tempTab.r
# written by JuG
# February 27 2021


#' Draft code script for table one
#' @author JuG
#' @description Print draft code script for table one
#' @param gv data.frame name
#' @param strata strata (optional)
#' @details
#' @examples
#' gv <- "data.frame.Name"
#'tempTab(gv=gv)
#' @return
#' @export


tempTab<- function(gv=gv, strata= NULL){
  cat("tabDesc <- tableone::CreateTableOne(vars = names(",gv,"), data = ",gv, ",strata =",strata,",  addOverall = T)")
  cat("\n")
  cat('pp <- print(tabDesc, nonnormal =  names(',gv,'),exact = names(',gv,'), smd = FALSE, printToggle = FALSE, noSpaces = TRUE, test = T)
  kable(pp, booktabs = T,format = "latex") %>%
    row_spec(0,bold=TRUE) %>%
    row_spec(1, hline_after = T) %>%
    row_spec(dim(pp)[1], hline_after = T) %>%
    kable_styling(latex_options = c("striped","HOLD_position","scale_down"), stripe_color = "oldlace",
                  full_width = F) %>%
    add_footnote("Median[Q1, Q3], n(%), p: wilcoxon or kruskal tests, fisher exact test", notation="alphabet")')
  cat("\n")
  return(" ")
}
