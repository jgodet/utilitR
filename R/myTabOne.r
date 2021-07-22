# myTabOne.r
# written by JuG
# May 11 2021


#' Create TableOne with NA counts
#' @author JuG
#' @description  basis from package tableone
#' @param
#' @details
#' @examples
#'dat <- data.frame(Var.Gr = gl(n = 2, k = 10),
#'    Var.A = rnorm(20),
#'    Var.B = factor(x = rbinom(n = 20, size = 1, .2)),
#'    Var.C = runif(n = 20,1,20))
#' dat$Var.A[sample(size = 4, 1:20)]<- NA
#' dat$Var.B[sample(size = 3, 1:20)]<- NA
#'pp2 <- myTabOne(dat = dat, strata = "Var.Gr")
#'
#'myTabOne(dat=lung[,-2:-1], strata = "status")
#' @return
#' @export


myTabOne <- function(dat=dat, strata = "Var.Grp" ){

  require(tableone)
  require(tidyverse)
  require(kableExtra)
  dat[,strata] <- as.factor(dat[,strata])

  tabDesc <- tableone::CreateTableOne(vars = names( dat )[-1], data =  dat ,strata = strata ,
                                     includeNA = F, addOverall = TRUE)
  pp <- print(tabDesc, nonnormal =  names( dat ),exact = names( dat ), smd = FALSE,
              printToggle = FALSE, noSpaces = TRUE, test = T )

  #cette section hacke la fonction create table to add NA
  tabDescNA <- tableone::CreateTableOne(vars = names( dat )[-1], data =  dat)
  ppNA <-  print(tabDescNA, nonnormal =  names( dat ),exact = names( dat ),
                 smd = FALSE, printToggle = FALSE, noSpaces = TRUE )
  NAcount <- function(x){sum(is.na(x))}
  attributes(ppNA)$dimnames[[2]] <- "NA.counts"

  res <- dat %>% group_by( dat[which(names(dat)==strata)]) %>%summarise_all(NAcount) %>% .[,-1] %>% t(.)
  resTot <- apply(res,1,sum)

  parVal <- apply(res, 1, function(x){paste(x, collapse=',')})
  resDetail <- paste("(", parVal, ")", sep="")
  ppNA[1,]<- ""
  ppNA[2:dim(ppNA)[1],] <- paste(resTot,resDetail )

  ##############################################
  #ajuster ici le nombre de colonne
  # if(addOverall){
    pp2 <- cbind(pp[,1:(length(levels(dat[,strata]))+2)], ppNA)
  # }else{
  #   pp2 <- cbind(pp[,1:(length(levels(dat[,strata]))+1)], ppNA)
  # }
  return(pp2)
}

