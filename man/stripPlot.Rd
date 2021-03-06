% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stripPlot.R
\name{stripPlot}
\alias{stripPlot}
\title{Plot a customized stipchart}
\usage{
stripPlot(
  data,
  formula,
  col = NULL,
  addBoxplot = FALSE,
  jitter = 0.3,
  grid = FALSE,
  ...
)
}
\arguments{
\item{formula}{a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor)}

\item{col}{if col is non-null it is assumed to contain colors to be used}

\item{addBoxplot}{if TRUE, a bxplot (package beeswarm) is drawn for each factor modalities}

\item{grid}{if TRUE, draw a grid}

\item{datafr}{data.frame}
}
\value{
customized stripchart graphics
}
\description{
Plot a customized stipchart
}
\examples{
dtf <- data.frame(val1 = rnorm(1000,1,.2), fact2 = LETTERS[gl(n = 5,k = 200,length = 1000)])
if(!require(colorRamps)){install.packages('colorRamps')}
require(colorRamps)
stripPlot(data = dtf, val1 ~ fact2, addBoxplot = TRUE,col=matlab.like(12))

if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library("RColorBrewer")
stripPlot(data = dtf, val1 ~ fact2,col=brewer.pal(n = 5, name = "Dark2"), jitter = .2,grid = T)

}
\author{
JuG
}
