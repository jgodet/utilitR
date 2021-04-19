# describe.r
# written by JuG
# March 31 2021


#' Do something
#' @author JuG
#' @description
#' @param df data.frame
#' @details
#' @examples
#' df <- data.frame(a = rnorm(50),
#'                  b = factor(rbinom(n = 50, size=1, prob=.2)),
#'                  c = runif(50),
#'                  d = factor(sample(LETTERS[1:5], size=50, replace=T)))
#'
#' @return
#' @export


# describe<- function(df,group='b'  ){
#   df %>%
#     group_by(b) %>%
#     summarise(across(
#       .cols = where(is.numeric),
#       .fns = list(Mean = mean, SD = sd), na.rm = TRUE,
#       .names = "{col}_{fn}"
#     ))
#
#   df %>%
#     group_by(b) %>%
#     summarise(across(
#       .cols = where(is.factor & names(.)!= "b"),
#       .fns = list(N = n()), na.rm = TRUE,
#       .names = "{col}_{fn}"
#     ))
#   return()
# }
