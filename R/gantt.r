# gantt.r
# written by JuG
# February 28 2025


#' Plot a Gantt diagram.
#' @author JuG
#' @description  Plot a Gantt diagram. from https://stackoverflow.com/questions/3550341/gantt-charts-with-r
#'
#' @param
#' @details
#' @examples
#' df <- data.frame(task = c("task1", "task2", "task3"),
#'   status = c("done", "active", "crit"),
#'   pos = c("first_1", "first_2", "first_3"),
#'   start = c("2014-01-06", "2014-01-09", "after first_2"),
#'   end = c("2014-01-08", "3d", "5d"))
#' gantt(df)
#'
#'
#' @return
#' @export


gantt<- function(gantt_df, title = "Titre"){
  library(tidyr)
  library(dplyr)
  library(DiagrammeR)

  m <- mermaid(
    paste0(
      # mermaid "header", each component separated with "\n" (line break)
      "gantt", "\n",
      "dateFormat  YYYY-MM-DD", "\n",
      "title ",title,  "\n",
      # unite the first two columns (task & status) and separate them with ":"
      # then, unite the other columns and separate them with ","
      # this will create the required mermaid "body"
      paste(gantt_df %>%
              unite(i, task, status, sep = ":") %>%
              unite(j, i, pos, start, end, sep = ",") %>%
              .$j,
            collapse = "\n"
      ), "\n"
    )
  )
  m$x$config = list(ganttConfig = list(
    axisFormatter = list(list(
      "%d %b %Y"
      ,htmlwidgets::JS(
        'function(d){ return d.getDay() == 1 }'
      )
    ))
  ))
  return(m)
}
