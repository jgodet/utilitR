# cleanUpAttempt.r
# written by JuG
# December 16 2017


#' Attempt to clean-up messy vectors
#' @author JuG
#' @description An attempt to clean-up messy vectors
#' @param messy a factor vector
#' @param cluster logical
#' @param ngroup integer giving the number of cluster to be formed
#' @param graph logical plot or not hclust
#' @details use stringdist package. examples from \url{https://cran.r-project.org/web/packages/rrefine/vignettes/rrefine-vignette.html}
#' @examples
#' x <- c("Y", "Y,", "Yes", "N", "No",NA,"No","No","No","Nope","Yes","Yes","Yes")
#' cleanUpAttempt(x)
#' cleanUpAttempt(x,ngroup =  2)
#' xc <- cleanUpAttempt(messy = x,ngroup =  2, cluster = TRUE)
#' summary(xc)
#'
#'
#' # install.packages("devtools")
#' devtools::install_github("vpnagraj/rrefine")
#' library(rrefine)
#' summary(lateformeeting$what.day.whas.it) #What a mess!
#' cleanUpAttempt(messy = lateformeeting$what.day.whas.it)
#' cleaned <-cleanUpAttempt(messy = lateformeeting$what.day.whas.it,ngroup = 5,cluster=TRUE)
#' summary(cleaned)
#'
#' cleanUpAttempt(messy = lateformeeting$was.i.on.time.for.work,ngroup=2)
#'
#' raw <- c("persistante modérée à sévère", "Persistante modérée a sévère","légère", "persistante modérée à sévère",
#' "persistante légère", "persistante modérée à sévère","persistante  modérée à sévère",
#' "persistante modérée à sévère","persistante modérée  à sévère", "persistante modérée à sévère","rien à voir",
#' "persistante modérée à sévère",NA, "persistante modérée à sévère",
#' "persistante légère","persistante modérée à sévère", "autre solution","persistante modérée,à sévère",
#' "persistante modérée à sévère","persistante modérée à sévère", "persistante légère","persistante modérée à sévèr",
#' "persistante légère", "persistant modérée à sévère")
#' cleanUpAttempt(messy = raw)
#' cleanUpAttempt(messy = raw,ngroup=4)
#' @return void or cleaned-up factor
#' @export


cleanUpAttempt<- function(messy, cluster = FALSE, ngroup = NULL, graph = TRUE ){
  if(!require(stringdist)){install.packages('stringdist')}
  if(length(messy)<3){
    return(cat("Messy must be of length > 2"))
    }
  require(stringdist)
  clean <- messy
  indnoNA <- which(!is.na(messy))
  messy <- na.omit(messy)
  cleanTemp <- messy
  d1 <- stringdistmatrix(unaccent(messy))
  d2 <- stringdistmatrix(gsub(pattern = " ",replacement = "",tolower(unaccent(messy))))
  d3 <- stringdistmatrix(substring(tolower(unaccent(messy)), first = 1,last = 1))
  d <- d1+d2+d3
  hc <- hclust(d)
  if(graph){plot(hc,labels = messy,hang=-1)}


  # sc<-numeric()
  # cible <- names(table(messy))
  # for (i in cible){
  #   cibletrans <- tolower(unaccent(i))
  #   aeval <- tolower(unaccent(messy))
  #   aeval2 <- gsub(pattern = " ",replacement = "",tolower(unaccent(messy)))
  #   aeval3 <- substring(tolower(unaccent(messy)), first = 1,last = 1)
  #
  #
  #   for (j in c("osa", "lv", "dl", "lcs", "qgram",
  #               "cosine", "jaccard", "jw", "soundex")){
  #     sc <- rbind(sc,stringdist(a = aeval, b = cibletrans,method = j))
  #     sc <- rbind(sc,stringdist(a = aeval2, b = cibletrans,method = j))
  #     sc <- rbind(sc,stringdist(a = aeval3, b = cibletrans,method = j))
  #   }
  # }
  # colnames(sc)<-messy
  # hc <- hclust(dist(t(sc)))
  # plot(hc, hang = -1)

  if(!is.null(ngroup)){
    rect.hclust(hc, k = ngroup,border=rainbow(ngroup))
  }

  if(cluster & !is.null(ngroup)){

    clust <- cutree(hc,ngroup)
    for (j in 1:ngroup){
      cleanTemp[clust==j] <- names(sort(table(messy[clust==j]),decreasing = TRUE)[1])
    }

    clean[indnoNA]<-cleanTemp
    clean <- clean[,drop=TRUE]
    return(as.factor(clean))
  }
}


