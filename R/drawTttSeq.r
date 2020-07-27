# drawTttSeq.r
# written by JuG
# July 27 2020


#' Draw treatment sequence
#' @author JuG
#' @description Draw treatment sequence for multiple and intermittent exposures (treatment switches) in the estimation of IPW using a survival model. Based on two vectors defining if treatment starts, is continued, is stopped or if no treatment on the time interval.
#' @param x a treatment sequence (made of 0 and 1)
#' @param returnType define which vector to return (init of term)
#' @details init takes values of 1 (treatment started), 0 (no treatment) or NA (if term is non missing) ; term takes values of 1 (treatment stopped), 0 (treatment continuation) or NA (if init is non missing)
#' @examples
#'seqx <- c(1,1,1,0,0,1,1,0,1,1)
#'seqx <- c(0, 1, 0, 0, 1, 1, 1, 1)
#'seqx <- c(0,0,0,0,0)
#'seqx <- c(1,1,1,1,1)
#'init <- drawTttSequence(x = seqx, returnType = "init")
#'term <- drawTttSequence(x = seqx, returnType = "term")
#'rbind(seqx, init, term)
#' @return vector
#' @export


drawTttSequence <- function(x = seqx, returnType="init"){
  getInitTtt <- function(seqx) {
    init <- rep(NA, length(seqx))
    if (seqx[1] == 1) {
      init[1] <- 1
    }
    init0 <- cumsum(rle(seqx)$lengths)[(which(rle(seqx)$values == 1)) - 1] + 1 #init
    init[init0] <- 1
    return(init)
  }

  getTermTtt <- function(seqx) {
    init <- rep(0, length(seqx))
    init0 <-cumsum(rle(x)$lengths)[(which(rle(x)$values==0))-1]+1  #term
    init[init0] <- 1
    return(init)
  }

  getTreat <- function(seqx) {
    init <- rep(0, length(seqx))
    if (seqx[1] == 1) {
      init[1] <- 1
    }
    init0 <- cumsum(rle(seqx)$lengths)[(which(rle(seqx)$values == 1)) - 1] + 1 #init
    init[init0] <- 1

    term <- rep(0, length(seqx))
    term0 <-cumsum(rle(x)$lengths)[(which(rle(x)$values==0))-1]+1  #term
    term[term0] <- 1
    cumsumdiff <- cumsum(init)-cumsum(term)

    if(max(cumsum(init))==0){
      correction <- rep(NA, length(cumsumdiff))
      return(correction)
    }else{
      return(cumsumdiff)
    }
  }



  init0 <- getInitTtt(seqx = x)
  termm0 <- getTermTtt(seqx = x)
  tttseq <- getTreat(seqx =x)
  init <- init0
  termm <- termm0
  termm[init==1] <- NA
  termm[termm==0 & tttseq==0] <- NA
  init[is.na(init0) & tttseq==0 & termm0 == 0] <- 0

  if(!any(!is.na(tttseq))){
    init <- rep(0, length(init))
    termm <- rep(NA, length(init))
  }

  if(returnType=="init"){
    return(init)
  }else{
    if(returnType=="term"){
      return(termm)
    } else{
      return("returntype must be init or term")
    }
  }
}



