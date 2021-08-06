# cleanUpAttempt.r
# written by JuG
# December 16 2017


#' Attempt to clean-up messy vectors
#' @author JuG
#' @description An attempt to clean-up messy vectors
#' @param messy a factor vector
#' @param cluster logical
#' @param ngroup integer giving the number of cluster to be formed
#' @param h	 numeric scalar or vector with heights where the tree should be cut
#' @param graph logical plot or not hclust
#' @param showUnique logical make hclust on labels with duplicate elements/rows removed
#' @param meth Method for distance calculation. Can be "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw","soundex". See stringdist-metrics for more information
#' @details use stringdist package. examples from \url{https://cran.r-project.org/web/packages/rrefine/vignettes/rrefine-vignette.html}
#' @examples
#' x <- c("Y", "Y,", "Yes", "N", "No",NA,"No","No","No","Nope","Yes","Yes","Yes")
#' cleanUpAttempt(x)
#' cleanUpAttempt(x, meth="osa")
#' cleanUpAttempt(x,ngroup =  2)
#' cleanUpAttempt(x, h=2)
#' xc <- cleanUpAttempt(messy = x,ngroup =  2, cluster = TRUE)
#' summary(xc)
#' xd <- cleanUpAttempt(messy = x,h =  2, cluster = TRUE)
#' summary(xd)
#'
#' # install.packages("devtools")
#' devtools::install_github("vpnagraj/rrefine")
#' library(rrefine)
#' summary(lateformeeting$what.day.whas.it) #What a mess!
#' cleanUpAttempt(messy = lateformeeting$what.day.whas.it)
#' cleanUpAttempt(messy = lateformeeting$what.day.whas.it, showUnique=T)
#' cleaned <-cleanUpAttempt(messy = lateformeeting$what.day.whas.it,ngroup = 5,cluster=TRUE, showUnique=T)
#' summary(cleaned)
#'
#' cleanUpAttempt(messy = lateformeeting$was.i.on.time.for.work,ngroup=2)
#'
#' raw <- c("persistante modérée à sévère", "Persistante modérée a sévère","légère", "persistante modérée à sévère",
#' "persistante légère", "persistante modérée à sévère","persistante  modérée à sévère",
#' "persistante modérée à sévère","persistante modéré  à sévère", "persistante modérée à sévère","rien à voir",
#' "persistante modérée à sévère",NA, "persistant modéré a severe",
#' "persistante légère","persistante modérée à sévère", "autre solution","persistante modérée,à sévère",
#' "persistante modérée à sévère","persistante modérée à sévère", "persistante légère","persistante modérée à sévèr",
#' "persistant legere", "persistant modérée à sévère", "")
#' cleanUpAttempt(messy = raw)
#' cleanUpAttempt(messy = raw,ngroup=6, showUnique=T)
#' table(cleanUpAttempt(messy=raw, ngroup=6,  graph = F, cluster = T))
#' @return void or cleaned-up factor
#' @export


cleanUpAttempt<- function(messy, cluster = FALSE, ngroup = NULL, graph = TRUE, showUnique=FALSE,  h=NULL, meth="jw" ){
  if(!require(stringdist)){install.packages('stringdist')}
  if(length(messy)<3){
    return(cat("Messy must be of length > 2"))
    }
  require(stringdist)
  clean <- messy
  indnoNA <- which(!is.na(messy))
  messy <- na.omit(messy)
  cleanTemp <- messy
  d1 <- stringdistmatrix(utilitR::unaccent(messy), method=meth)
  d2 <- stringdistmatrix(gsub(pattern = " ",replacement = "",tolower(utilitR::unaccent(messy))), method=meth)
  d3 <- stringdistmatrix(substring(tolower(utilitR::unaccent(messy)), first = 1,last = 1), method=meth)
  d <- d1+d2+d3
  hc <- hclust(d)
  if(graph & ! showUnique){plot(hc,labels = messy,hang=-1, xlab = "",sub = "")}

  if(graph & showUnique){
    messyU <- unique(messy)
    d1U <- stringdistmatrix(utilitR::unaccent(messyU), method=meth)
    d2U <- stringdistmatrix(gsub(pattern = " ",replacement = "",tolower(utilitR::unaccent(messyU))), method=meth)
    d3U <- stringdistmatrix(substring(tolower(utilitR::unaccent(messyU)), first = 1,last = 1), method=meth)
    dU <- d1U + d2U + d3U
    hcU <- hclust(dU)

    plot(hcU,labels = messyU, hang=-1, xlab = "",sub = "")
    }


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
  if(!is.null(h) & !is.null(ngroup)){
    cat("Choose either ngroup or h!")
    break
  }
  if(!is.null(ngroup) & graph & !showUnique){
    rect.hclust(hc, k = ngroup,border=rainbow(ngroup))
  }
  if(!is.null(ngroup) & graph & showUnique){
    rect.hclust(hcU, k = ngroup,border=rainbow(ngroup))
  }

  if(!is.null(h) & graph & !showUnique){
    rect.hclust(hc, h = h,border=rainbow(10))
  }
  if(!is.null(h) & graph & showUnique){
    rect.hclust(hcU, h = h,border=rainbow(10))
  }
  if(cluster & !is.null(ngroup)){
    clust <- cutree(hc,k = ngroup)
    for (j in 1:ngroup){
      cleanTemp[clust==j] <- names(sort(table(messy[clust==j]),decreasing = TRUE)[1])
    }
    clean[indnoNA]<-cleanTemp
    clean <- clean[,drop=TRUE]
    return(as.factor(clean))
  }

  if(cluster & !is.null(h)){
    clust <- cutree(hc,h = h)
    ngroup = length(table(clust))
    for (j in 1:ngroup){
      cleanTemp[clust==j] <- names(sort(table(messy[clust==j]),decreasing = TRUE)[1])
    }
    clean[indnoNA]<-cleanTemp
    clean <- clean[,drop=TRUE]
    return(as.factor(clean))
  }
}


