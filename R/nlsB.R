# nlsB.r
# written by JuG
# Tue Dec 12 14:47:40 2017"



#' Determine the nonlinear (weighted) MCMC estimates of the parameters of a nonlinear model.
#' @author JuG
#' @description Determine the nonlinear (weighted) MCMC estimates of the parameters of a nonlinear model.
#' @param formula  an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which blm is called.
#' @param graphOutput regression parameters graphical output (MCMC Trace and posterior density)
#' @param nIter number of iterations
#' @param thin thinning interval for monitors
#' @details Models for lm are specified symbolically. A typical model has the form response ~ terms where response is the (numeric) response vector and terms is a series of terms which specifies a linear predictor for response.
#' @examples
#' data(mtcars)
#' summary(lm(mpg~ cyl + vs+gear+carb,data=mtcars))
#' lmB(mpg~ cyl + vs+gear+carb,data=mtcars,nIter=50000)
#' lmB(mpg~ .,data=mtcars,nIter=50000,graphOutput=FALSE)
#' @return non-linear regression parameters
#' @export

nlsB <- function(formula, data, graphOutput = TRUE, nIter=10000, thin=1){
#   if(!require(rjags)){install.packages('rjags')}
#   require('rjags')
#   if(missing(formula)){cat("Formula is missing")}
#
#   if(!missing(formula) & !missing(data)){
#     data <- data.frame(get_all_vars(formula,data=data))
#   }
#   if(!missing(formula) & missing(data)){
#     data <- data.frame(get_all_vars(formula))
#   }
#
#   n <- dim(data)[1]
#   p <- dim(data)[2]-1
#   y <- data[,1]
#   X <- data[,-1]
#
#
#   dtfResp=list(y=y,X=X,n=n,p=p)
#   init=list(tau=1,alpha=0,beta=rep(0,p))
#   modelstring="
#  model{
#
#   # likelihood
#   for( i in 1:length(n)) {
#   y[i] ~ dnorm(mu[i], tau)
#   mu[i] <-
#   }
#
#   tau~dgamma(1,0.001)
#   }
#   "
#   model=jags.model(textConnection(modelstring),
#                    data=dtfResp,inits=init)
#   update(model,n.iter=1000)
#   output=coda.samples(model=model,variable.names=c("alpha","beta","tau"),
#                       n.iter=nIter,thin=thin)
#   print(summary(output))
#   if(graphOutput){plot(output)}
# }
#
#
#
# ############@
#
# If = 1000
# I0 = 50
# k1 = .02
# t = 0:200
# y <- If - (If-I0) *exp(-k1 * t) + rnorm(length(t),0,30)
# plot(t,y)
#
# Data <- list(t = t, y = y)
# nlsfit2 <- nls(y ~ If - (If-I0) * exp(-k1 * t),data=Data,
#                , start = list(If = 230000,  k1 = 0.02))
# summary(nlsfit2)
#
# nlsB <- function(formula,data,dependant,priors,fixedVal=NULL,graphOutput = TRUE, nIter=10000, thin=1,monitor){
#
#   start <- regexpr(pattern = "~",as.character(formula))[1] +1
#   stop <- nchar(as.character(formula))
#   form <- substring(as.character(formula), start, stop)
#
#   if(regexpr(pattern = "exp\\(-",text = as.character(form)) == -1 ){
#     form <- gsub(pattern = "exp\\(-",replacement = "exp(-1*",x = as.character(form))
#   }
#
#   form <- gsub(pattern = dependant,replacement = paste(dependant,"[i]",sep=''),x = as.character(form))
#
#   N <- dim(data)[1]
#
#   if(!is.null(fixedVal)){
#     for (i in 1:length(fixedVal)){
#       form <- gsub(pattern = names(fixedVal)[i],replacement = fixedVal[[i]],x = as.character(form))
#     }
#   }
#
#   aa <- paste("\nmodel{ \n",sep='')
#   aa <- paste(aa,"  # likelihood\n",sep='')
#   aa <- paste(aa,"  for( i in 1:N) {\n",sep='')
#   aa <- paste(aa,"    y[i] ~ dnorm(mu[i], tau)\n",sep='')
#   aa <- paste(aa,"    mu[i] <-",form,"\n",sep='')
#   aa <- paste(aa,"}\n",sep='')
#   aa <- paste(aa,"#priors \n",sep='')
#   for (i in 1:length(priors)){
#     aa <- paste(aa,priors[[i]],"\n",sep="")
#   }
#   aa <- paste(aa,"tau  ~ dgamma(0.01, 0.01)\n",sep="")
#   aa <- paste(aa,"sigma <- 1 / sqrt(tau)\n",sep="")
#   aa <- paste(aa, "}\n",sep='')
#
#
#
#   mod1 <- jags.model(textConnection(aa), data = data, n.chains = 4,
#                    n.adapt = 100)
#   update(mod1, 5000)
#
#   mod1.samples <- coda.samples(model = mod1,
#                              variable.names = dput(monitor),n.iter=nIter,thin=thin)
#
#   print(summary(mod1.samples))
#   if(graphOutput){plot(mod1.samples,ask=T)}
# }
#
#
# Ddata <- data.frame(x=-10:10, y = 3.14 * (-10:10)  + 3 + rnorm(length(-10:10),0,10))
# nls(formula = "y ~ k * x + 3",data = Ddata, start = list(k = 0.02))
#
# nlsB(formula = "y ~ k * x + 3",data = list(x = Ddata$x, y = Ddata$y, N = length(data$y)), dependant = "x", priors = list("k ~ dunif(-5,5)"), monitor=c("k"))
#
# ?nls
#
# DNase1 <- subset(DNase, Run == 1)
# fm3DNase1 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
#                  data = DNase1,
#                  start = list(Asym = 3, xmid = 0, scal = 1))
# summary(fm3DNase1)
#
# data <- data.frame(get_all_vars(formula,data=DNase1)
# n <- dim(data)[1]
# p <- dim(data)[2]-1
# y <- data[,1]
# X <- data[,-1]
#
#
# nlsB(formula = "density ~ Asym/(1 + exp((xmid - log(conc))/scal))",
#      fixedVal = NULL,
#      data= list(y = DNase1$density, conc = DNase1$conc, N = length( DNase1$density)),
#      dependant = "conc",
#      priors = list("Asym ~ dnorm(0, 1)", "xmid ~ dnorm(0.1,1)", "scal = dnorm(0, 1)"),
#      monitor = c("Asym","xmid", "scal")
#      )
#
#
#
# formula = "y ~ If - (If-I0) * exp(-k1 * t)",
# data = list(x = Ddata$x, y = Ddata$y, N = length(data$y)),
# dependant = "t",
# priors = list("If ~ dunif(100, 10000)", "k1 ~ dnorm(0.1,1000)I(0,)"),
# fixedVal = list(I0 = 50),graphOutput = TRUE, nIter=10000, thin=1,monitor = c("If","k1","I0", "sigma"))
}
