# glmBselect.r
# written by JuG
# Tue Dec 12 14:47:40 2017"



#' Variable selection and fitting Linear Models using bayesian inference.
#' @author JuG
#' @description  Variable selection and fitting Linear Models using bayesian inference.
#' @param formula  an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which blm is called.
#' @param graphOutput regression parameters graphical output (MCMC Trace and posterior density)
#' @param nIter number of iterations
#' @param thin thinning interval for monitors
#' @param effect "fixed", "random" or "randomPrior" effect for variable selection
#' @details Models for lm are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response.    The effect "randomPrior" adds a beta prior for the model inclusion probability. This induces a distribution for the number of included variables which has longer tails than the binomial distribution, allowing the model to learn about the degree of sparsity.
#' @examples
#' #generate model data.
#' n <- 500
#' p <- 20
#' X <- matrix(rnorm(n*p),ncol=p)
#' beta <- 2^(0:(1-p))
#' print(beta)
#' alpha <- 3
#' tau <- 2
#' eps <- rnorm(n,0,1/sqrt(tau))
#' y <- as.numeric(cut(alpha+as.vector(X%*%beta + eps),c(-10,3,10)))-1
#' daten <- cbind(y,as.data.frame(X))
#'
#' mod <- glm(y~.,data=daten, family='binomial')

#' require(MASS)
#' stepAIC(mod,direction = "both")
#' glmBselect(y~.,data=daten,nIter=10000,graphOutput=FALSE,effect="fixed")
#'
#'@references \enumerate{
#'\item O’Hara, R. and Sillanpaa, M. (2009) A review of Bayesian variable selection methods: what, how and which. Bayesian Analysis, 4(1):85-118.
#'\item Kuo, L. and Mallick, B. (1998) Variable selection for regression models. Sankhya B, 60(1):65-81.
#'}
#'
#' @return regression parameters
#' @export


glmBselect <- function(formula, data, graphOutput = TRUE, nIter=10000, thin=1,effect = "fixed"){

  #effect = c("fixed","random"," : Beta prior for the model inclusion probability. This induces a distribution for the number of included variables which has longer tails than the binomial distribution, allowing the model to learn about the degree of sparsity.
  #O’Hara, R. and Sillanpaa, M. (2009) A review of Bayesian variable selection methods: what, how and which. Bayesian Analysis, 4(1):85-118.


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

  if(effect == "fixed"){
    #with fixed effect
    init=list(beta0=1,betaT=rep(0,p),ind=rep(0,p))

    modelstringFE="
    model {
    for (i in 1:n) {
    y[i]~dbern(pp[i])
    logit(pp[i])<-max(-20,min(20, beta0+inprod(X[i,],beta)))
    }
    for (j in 1:p) {
    ind[j]~dbern(0.4)
    betaT[j]~dnorm(0,0.001)
    beta[j]<-ind[j]*betaT[j]
    }
    beta0~dnorm(0,0.0001)
    }
    "
    modelFE=jags.model(textConnection(modelstringFE),
                     data=dtfResp,inits=init)
    update(modelFE,n.iter=1000)
    output=coda.samples(model=modelFE,variable.names=c("beta0","beta","ind"),
                        n.iter=nIter,thin=thin)
  }

  if(effect == "random"){
  #TODO corriger cette partie
      break()
    #with fixed effect
    init=list(tau=1,taub=1,alpha=0,betaT=rep(0,p),ind=rep(0,p))
    modelstringRE="
    model{
    for (i in 1:n) {
    y[i]~dbern(p[i])
    logit(p[i])<-max(-20,min(20, beta0+inprod(X[i,],beta)
    }
    for (j in 1:p) {
      ind[j]~dbern(0.4)
      betaT[j]~dnorm(0,taub)
      beta[j]<-ind[j]*betaT[j]
    }
    beta0~dnorm(0,0.0001)
    tau~dgamma(1,0.001)
    taub~dgamma(1,0.001)
    }
    "
    modelRE=jags.model(textConnection(modelstringRE),
                       data=dtfResp,inits=init)
    update(modelRE,n.iter=1000)
    output=coda.samples(model=modelRE,
                        variable.names=c("alpha","beta","ind","tau","taub"),
                        n.iter=nIter,thin=thin)
  }

   if(effect == "randomPrior"){
     init=list(tau=1,taub=1,pind=0.5,alpha=0,betaT=rep(0,p),ind=rep(0,p))
     modelstringREP="
     model {
     for (i in 1:n) {
     y[i]~dbern(p[i])
     logit(p[i])<-max(-20,min(20, beta0+inprod(X[i,],beta)
     }
     for (j in 1:p) {
     ind[j]~dbern(pind)
     betaT[j]~dnorm(0,taub)
     beta[j]<-ind[j]*betaT[j]
     }
     beta0~dnorm(0,0.0001)
     tau~dgamma(1,0.001)
     taub~dgamma(1,0.001)
     pind~dbeta(2,8)
     }
     "
     modelREP=jags.model(textConnection(modelstringREP),
                      data=dtfResp,inits=init)
     update(modelREP,n.iter=1000)
     output=coda.samples(model=modelREP,
                         variable.names=c("alpha","beta","ind","tau","taub","pind"),
                         n.iter=nIter,thin=thin)
   }



  print(summary(output))
  if(graphOutput){plot(output)}
}
