# glmB.r
# written by JuG
# Tue Dec 12 14:47:40 2017"



#' Fitting Generalized Linear Models using bayesian inference (family = binomial)
#' @author JuG
#' @description Generalized Linear Models using bayesian inference (logistic regression)
#' @param formula  an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which blm is called.
#' @param graphOutput regression parameters graphical output (MCMC Trace and posterior density)
#' @param nIter number of iterations
#' @param thin thinning interval for monitors
#' @param returnCodaSamples if TRUE, return the cosa samples output as a mcmc.list
#' @param priorPrec precisions of alpha and beta dnorm distributions
#' @details Models for glm are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response.
#' @examples
#' dtf1 <- data.frame(Y = rbinom(n = 60 ,size=1,prob = .3), X = rnorm(60, 10,2))
#' mod1 <- glmB(Y ~ X , data= dtf1)
#' dtf2 <- data.frame(Y = rbinom(n = 60 ,size=1,prob = .3), X = rnorm(60, 10,2), X2 = rnorm(60, 10,2))
#' mod2 <- glmB(Y ~ X + X2, data= dtf2 )
#' mod3 <- glmB(Y ~ X + X2, data= dtf2 ,priorPrec = c(.001, .1) )
#' @return regression parameters
#' @export


glmB <- function(formula, data = NULL, graphOutput = FALSE, nIter=10000, thin=1, returnCodaSamples = FALSE, priorPrec =c(.001,.001)){
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
    np <- dim(data)[2]-1
    y <- data[,1]
    X <- data[,-1]


  init=list(alpha=0,beta=rep(0,np))

  ######@
  # modelH<-function(){
  #
  #   for( i in 1 : N ) {Y[i]~dbern(p[i])
  #
  #     for( j in 1 : p ){monome[i,j]     <- designmat[i,j]*beta[j]}
  #     logit(p[i])     <- sum(monome[i,])   }
  #
  #   for( j in 1 : p ){beta[j]         ~  dnorm(Matrice_prior[1,j],Matrice_prior[2,j])
  #     Pr_beta_pos[j]  <- step(beta[j]-Seuilmonitor[j])
  #     RC[j]           <- exp (beta[j])}
  #
  # }

#
#   modelString="
# model{
#   for (i in 1:N) {
#     y[i] ~ dbern(p[i])
#     logit(p[i]) <- max(-20,min(20,beta0+beta1*x[i]))
#   }
#   beta0 ~ dnorm(0,1.0E-06)
#   beta1 ~ dnorm(0,1.0E-06)
# }
# "
  ##########


  if(np>1){
    dtfResp=list(y=y,X=X,n=n,np=np)
  modelstring=paste("
  model {
  for (i in 1:n) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- max(-20,min(20,alpha+inprod(X[i,],beta)))
  }
  for (j in 1:np) {
    beta[j]~dnorm(0,",priorPrec[2],")
    P_beta[j]	<- step(beta[j])
  }
  alpha~dnorm(0,",priorPrec[1],")
  P_alpha <- step(alpha)
  }
  ",sep="")
  }

if(np==1){
  dtfResp=list(y=y,X=X,n=n)
    modelstring=paste("
model{
  for (i in 1:n) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- max(-20,min(20,alpha+beta*X[i]))
  }
  alpha ~ dnorm(0,",priorPrec[1],")
  beta ~ dnorm(0,",priorPrec[2],")
  P_alpha <- step(alpha)
  P_beta <- step(beta)
}
",sep="")
  }
  model=jags.model(textConnection(modelstring),
                   data=dtfResp,inits=init)
  update(model,n.iter=1000)
  output=coda.samples(model=model,variable.names=c("alpha","beta"),
                      n.iter=nIter,thin=thin)
  outputP=coda.samples(model=model,variable.names=c("P_alpha","P_beta"),
                       n.iter=nIter,thin=thin)
  Step <- c(summary(outputP)[[1]][,1])
  tabOut <- summary(output)
  if(!is.null(colnames(X))){
    rownames(tabOut[[1]])[2:(np+1)] <- substr(colnames(X),1,20)
    rownames(tabOut[[2]])[2:(np+1)] <- substr(colnames(X),1,20)
  }
  tabOut[[2]] <- cbind(tabOut[[2]],Step)
  colnames(tabOut[[2]])[6] <- "Pr(X>0)"
  print(tabOut)

  if(graphOutput){plot(output)}
  if(returnCodaSamples){
    return(output)
  }else{
    return(tabOut)
  }
}
