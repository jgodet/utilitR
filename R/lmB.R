# lmB.r
# written by JuG
# Tue Dec 12 14:47:40 2017"



#' Fitting Linear Models using bayesian inference
#' @author JuG
#' @description Fitting Linear Models using bayesian inference
#' @param formula  an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which blm is called.
#' @param graphOutput regression parameters graphical output (MCMC Trace and posterior density)
#' @param nIter number of iterations
#' @param thin thinning interval for monitors
#' @param returnCodaSamples if TRUE, return the cosa samples output as a mcmc.list
#' @details Models for lm are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response.
#' @examples
#' data(mtcars)
#' summary(lm(mpg~ cyl + vs+gear+carb,data=mtcars))
#' lmB(mpg~ cyl + vs+gear+carb,data=mtcars,nIter=50000)
#' lmB(mpg~ .,data=mtcars,nIter=50000,graphOutput=FALSE)
#'
#'
#' @return regression parameters
#' @export


lmB <- function(formula, data = NULL, graphOutput = TRUE, nIter=10000, thin=1, returnCodaSamples = FALSE){
  if(!require(rjags)){install.packages('rjags')}
  require('rjags')
  if(missing(formula)){cat("Formula is missing")}

  if(!missing(formula) & !missing(data)){
    data <- data.frame(get_all_vars(formula,data=data))
  }
  if(!missing(formula) & missing(data)){
    data <- data.frame(get_all_vars(formula))
  }

    n <- dim(data)[1]
    p <- dim(data)[2]-1
    y <- data[,1]
    X <- data[,-1]

  dtfResp=list(y=y,X=X,n=n,p=p)
  init=list(tau=1,alpha=0,beta=rep(0,p))
  if(p>1){
  modelstring="
  model {
  for (i in 1:n) {
  mean[i]<-alpha+inprod(X[i,],beta)
  y[i]~dnorm(mean[i],tau)
  }
  for (j in 1:p) {
  beta[j]~dnorm(0,0.001)
  P_beta[j]	<- step(beta[j])
  }
  alpha~dnorm(0,0.0001)
  P_alpha <- step(alpha)
  tau~dgamma(1,0.001)
  }
  "
  }
  if(p==1){
    modelstring="
  model {
    for (i in 1:n) {
    mean[i]<-alpha + beta* X[i]
    y[i]~dnorm(mean[i],tau)
    }
    for (j in 1:p){
    beta[j]~dnorm(0,0.001)
    P_beta <- step(beta)
    }
    alpha~dnorm(0,0.0001)
    P_alpha <- step(alpha)
    tau~dgamma(1,0.001)
  }
    "
  }
  model=jags.model(textConnection(modelstring),
                   data=dtfResp,inits=init)
  update(model,n.iter=1000)
  output=coda.samples(model=model,variable.names=c("alpha","beta","tau"),
                      n.iter=nIter,thin=thin)
  outputP=coda.samples(model=model,variable.names=c("P_alpha","P_beta"),
                       n.iter=nIter,thin=thin)
  Step <- c(summary(outputP)[[1]][,1],NA)
  tabOut <- summary(output)
  if(!is.null(colnames(X))){
    rownames(tabOut[[1]])[2:(p+1)] <- substr(colnames(X),1,20)
    rownames(tabOut[[2]])[2:(p+1)] <- substr(colnames(X),1,20)
  }
  tabOut[[2]] <- cbind(tabOut[[2]],Step)
  print(tabOut)
  if(graphOutput){plot(output)}
  if(returnCodaSamples){
    return(output)
  }
}
