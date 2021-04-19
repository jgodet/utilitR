# Bprop2.r
# written by JuG
# April 19 2021


#' Bayesian comparison for two proportions
#' @author JuG
#' @description Cod to compare two proportions using bayesian model with beta priors
#' @param var1 variable 1
#' @param var2 = variable 2
#' @param n.iter number of iteration
#' @param n.burnin number of iteration used in the update of the model
#' @param n.thin n thin
#' @param prior.beta beta priors (vector of 4 parameters, two first for var1)
#' @details
#' @examples
#' dtf<-data.frame(grp=c(rep("A",150),rep("B", 150)),
#'                 res=c(rbinom(n = 150, size = 1, prob = .5),
#'                       rbinom(n = 150, size = 1, prob = .49)) )
#'
#' res <- B.prop2(var1 = dtf$res[dtf$grp=="A"], var2 = dtf$res[dtf$grp=="B"])
#' res

#' @return
#' @export


B.prop2<-function(var1,var2,n.iter=11000,n.burnin=1000,n.thin=1,prior.beta=c(1,1,1,1)){

  library(rjags)
  #--------------------------------------------------------------------------------------------------
  mod1_string = " model {
    for (i in 1:N1) {  X[i] ~ dbin(mu.x,1)	}
    for (j in 1:N2) {	 Y[j] ~ dbin(mu.y,1)    }

    mu.x~dbeta(a,b)
    mu.y~dbeta(c,d)

    di<-mu.x-mu.y
    p<-step(mu.x-mu.y)
    or<-mu.x*(1-mu.y)/(mu.y*(1-mu.x))
  }
  "

  donnees<-list(	    N1			=length(var1),
                     N2			=length(var2),
                     X				=dput(as.numeric(var1),		control=NULL),
                     Y				=dput(as.numeric(var2),		control=NULL),
                     a				=prior.beta[1],
                     b				=prior.beta[2],
                     c				=prior.beta[3],
                     d				=prior.beta[4]                            )

  parametre	 <-c("mu.x","mu.y","di","p","or")
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
  indexProb	<-grep  ("p",colnames(mod1_csim2))
  indexOR		<-grep  ("or",colnames(mod1_csim2))


  RES<-mod1_csim2[,c(indexMux,indexMuy,indexDiff,indexProb,indexOR)]
  moyennes	     <-round(apply(RES,2,mean,na.rm=T)  ,3)
  variances	     <-round(apply(RES,2,var,na.rm=T),3)
  sd		         <-round(apply(RES,2,sd,na.rm=T),2)
  q0.025	         <-round(apply(RES,2,quantile,na.rm=T, prob=.025),2)
  q0.250	         <-round(apply(RES,2,quantile,na.rm=T, prob=.25),2)
  q0.500	         <-round(apply(RES,2,quantile,na.rm=T, prob=.5),2)
  q0.750	         <-round(apply(RES,2,quantile,na.rm=T, prob=.75),2)
  q0.975	         <-round(apply(RES,2,quantile,na.rm=T, prob=.975),2)

  RES              <-cbind(moyennes,variances,sd,q0.025,q0.250,q0.500,q0.750,q0.975)

  rownames(RES)    <-c(	paste("Prop",nom1),paste("Prop",nom2),"difference",paste("P(",nom1,">",nom2,")")[1],"Rapport_cotes")

  diff.signif<-c("","","    ***","","")
  if(RES[3,4]<0 & RES[3,8]>0){diff.signif<-c("","","","","")}
  RES<-noquote(cbind(RES,diff.signif))
  colnames(RES)    <-c("Moy","Var","Sd","2.5%","25%","50%","75%","97.5%","")
  RES<-RES[,-2]
  var11<-var1
  var22<-var2
  var1<-var1[!is.na(var1)]
  var2<-var2[!is.na(var2)]
  succes1          <-sum(var1,na.rm=TRUE)
  echec1           <-length(var1)-succes1
  succes2          <-sum(var2,na.rm=TRUE)
  echec2           <-length(var2)-succes2
  p.succes1	     <-round(succes1/length(var1),2)
  p.succes2	     <-round(succes2/length(var2),2)
  p.echec1	     <-round(echec1/length(var1),2)
  p.echec2	     <-round(echec2/length(var2),2)
  NA1              <-round(sum(is.na(var11)),2)
  NA2              <-round(sum(is.na(var22)),2)
  pNA1             <-round(NA1/length(var11),2)*100
  pNA2             <-round(NA2/length(var22),2)*100
  N1               <-length(var11)
  N2               <-length(var22)


  RES1<-noquote(cbind(c(N1,N2),c(NA1,NA2),c(pNA1,pNA2),c(succes1,succes2),c(echec1,echec2),c(p.succes1,p.succes2),c(p.echec1,p.echec2)))
  colnames(RES1)   <-c("N","NA","%NA","#1","#0","%1","%0")
  rownames(RES1)   <-c(nom1,nom2)

  RESULTAT         <-list(Stats=RES1,MCMC=RES)
  return(RESULTAT)
}


