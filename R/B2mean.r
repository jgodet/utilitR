# B2mean.r
# written by JuG based on M Schaeffer code
# April 22 2021


#' Bayesian comparison of two means
#' @author JuG
#' @description
#' @param var1 variable 1
#' @param var2 = variable 2
#' @param n.iter number of iteration
#' @param n.burnin number of iteration used in the update of the model
#' @param n.thin n thin
#' @param prior.norm normal parameters for priors of means 1 and 2 - two parameters each (mean, precision)
#' @param prior.gamma gamma parameters for priors of tau 1 and tau 2 - two parameters each (shape, rate)
#' @details precision is the inverse of variance
#' @examples
#' x <- rnorm(200, 2.1, .4)
#' y <- rnorm(200, 2, .4)
#' B2mean(var1=x, var2=y)
#'
#' @return list with Stats (descriptive stats) and MCMC (inference)
#' @export


B2mean<-function(var1,var2,n.iter=11000,n.burnin=1000,n.thin=1,prior.norm=c(0,0.001,0,0.001),prior.gamma=c(0.5,0.5,0.5,0.5) ){

  library(rjags)
  #--------------------------------------------------------------------------------------------------
  mod1_string = " model {
    for (i in 1:N1) {		X[i] ~ dnorm(mu.x,tau.x)		}
    for (i in 1:N2) {     	Y[i] ~ dnorm(mu.y,tau.y)  		}

			  mu.x	~dnorm (am  ,bm)
			  mu.y	~dnorm (cm  ,dm)
			  tau.x	~dgamma(a,b)
			  tau.y	~dgamma(c,d)
			  di	<-     mu.x-mu.y
			  prob <- step(mu.x-mu.y)
  }
  "

  donnees<-list(	N1  =length(var1),
                  N2	=length(var2),
                  X		=var1,
                  Y		=var2,
                  a		=prior.gamma[1],
                  b		=prior.gamma[2],
                  c		=prior.gamma[3],
                  d		=prior.gamma[4],
                  am  =prior.norm[1],
                  bm  =prior.norm[2],
                  cm  =prior.norm[3],
                  dm  =prior.norm[4] )


  parametre	 <-c("mu.x","mu.y","tau.x","tau.y","di","prob")
  inits1 = function() {
    inits = list("mu.x"= .5,
                 "mu.y" = .5)
  }


  mod1 = jags.model(textConnection(mod1_string), data=donnees, inits = inits1, n.chains=3, quiet = T)
  update(mod1, n.burnin) # burn-in

  mod1_sim = coda.samples(model=mod1,
                          variable.names=parametre,
                          n.iter=n.iter, thin = n.thin)
  mod1_csim = do.call(rbind, mod1_sim)

  mod1_csim2 <- mod1_csim

  nom1		     <-deparse(substitute(var1))                                                           ### Autres STATS a faire apparaitre ?galement
  nom2		     <-deparse(substitute(var2))
  indexDiff	<-grep  ("di",colnames(mod1_csim2))
  indexMux	<-grep  ("mu.x",colnames(mod1_csim2))
  indexMuy	<-grep  ("mu.y",colnames(mod1_csim2))
  indexP	<-grep  ("prob",colnames(mod1_csim2))



  RES<-mod1_csim2[,c(indexMux,indexMuy,indexDiff,indexP )]
  moyennes	     <-round(apply(RES,2,mean,na.rm=T)  ,3)
  variances	     <-round(apply(RES,2,var,na.rm=T),3)
  sd		         <-round(apply(RES,2,sd,na.rm=T),2)
  q0.025	         <-round(apply(RES,2,quantile,na.rm=T, prob=.025),2)
  q0.250	         <-round(apply(RES,2,quantile,na.rm=T, prob=.25),2)
  q0.500	         <-round(apply(RES,2,quantile,na.rm=T, prob=.5),2)
  q0.750	         <-round(apply(RES,2,quantile,na.rm=T, prob=.75),2)
  q0.975	         <-round(apply(RES,2,quantile,na.rm=T, prob=.975),2)

  RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)

  rownames(RES)    <- c(	paste("Moy",nom1),paste("Moy",nom2),"difference",paste("P(",nom1,">",nom2,")")[1])
#######TODO
  ########
  diff.signif<-c("","","    ***","")
  if(RES[3,4]<0 & RES[3,8]>0){diff.signif<-c("","","","")}
  RES<-noquote(cbind(RES,diff.signif))
  colnames(RES)    <-c("Moy","Var","Sd","2.5%","25%","50%","75%","97.5%","")
  RES<-RES[,-2]


  min1             <-round(min(var1,na.rm=TRUE),2)
  max1             <-round(max(var1,na.rm=TRUE),2)
  min2             <-round(min(var2,na.rm=TRUE),2)
  max2             <-round(max(var2,na.rm=TRUE),2)
  mo1              <-round(mean(var1,na.rm=TRUE),2)
  mo2              <-round(mean(var2,na.rm=TRUE),2)
  sd1				 <-round(  sqrt(var(var1,na.rm=TRUE))/sqrt(length(var1[!is.na(var1)]))							,2		)
  sd2				 <-round(  sqrt(var(var2,na.rm=TRUE))/sqrt(length(var2[!is.na(var2)]))							,2		)
  ete1             <-round(max1-min1,2)
  ete2             <-round(max2-min2,2)
  NA1              <-round(sum(is.na(var1)),2)
  NA2              <-round(sum(is.na(var2)),2)
  pNA1             <-round(NA1/length(var1),2)*100
  pNA2             <-round(NA2/length(var2),2)*100
  N1               <-length(var1)
  N2               <-length(var2)
  if(var(var1,na.rm=TRUE)!=0){
    p.Norm1 <-round(shapiro.test(var1)$p.value,2)
    if(p.Norm1<0.05){pp.Norm1<-"***"}else{pp.Norm1<-""}
  }else{p.Norm1<-NA
    pp.Norm1<-""}
    if(var(var2,na.rm=TRUE)!=0){p.Norm2 <-round(shapiro.test(var2)$p.value,2)
    if(p.Norm2<0.05){pp.Norm2<-"***"}else{pp.Norm2<-""}
  }	else{p.Norm2<-NA
  pp.Norm2<-""}

  RES1<-noquote(cbind(c(N1,N2),c(NA1,NA2),c(pNA1,pNA2),c(min1,min2),c(max1,max2),c(ete1,ete2),c(mo1,mo2),c(sd1,sd2),c(p.Norm1,p.Norm2),c(pp.Norm1,pp.Norm2)))
  colnames(RES1)   <-c("N","NA","%NA","min","max","etendue","Moy","Sd","Shapiro","")
  rownames(RES1)   <-c(nom1,nom2)


  RESULTAT         <-list(Stats=RES1,MCMC=RES)
  return(RESULTAT)
}
