#library(ggplot2);library(anytime);library(forecast);library(cowplot);library(tscount)
#source("revision_fn.R")

###Loading the data sets and covariates.
#ht.count = read.csv("ht_count_cleaned.csv")
#datt = ht.count$count
#ht.covar = read.csv("ht_covariate_cleaned.csv")[1:153,] # 15th~153rd


####ACF Plot
ggacf.null = ggAcf(datt,lag.max=25)+labs(title="ACF")
ggpacf.null = ggPacf(datt,lag.max=25)+labs(title="Partial ACF")
x11()
plot_grid(ggacf.null,ggpacf.null,nrow=1,labels = c("(a)","(b)"))
####ACF Plot - default R
x11()
par(mfrow=c(1,2))
acf1(datt,max.lag=20, main = "ACF")
acf1(datt,max.lag=20, main = "PACF",pacf=T)
par(mfrow=c(1,1))


####NULL model comparison -NB ####
####NULL model comparison -NB ####
####NULL model comparison -NB ####
####NULL model comparison -NB ####

model01 = list(past_mean = NULL, past_obs = 1:1)
model02 = list(past_mean = NULL, past_obs = 1:2)
model03 = list(past_mean = NULL, past_obs = 1:3)
model11 = list(past_mean = 1, past_obs = 1:1)
model11 = list(past_mean = 1, past_obs = 1:2)
model11 = list(past_mean = 1, past_obs = 1:3)


ts.log.null.11 = tsglm(datt, model=model11,xreg=NULL,link="log",distr="nbinom")
ts.log.null.12 = tsglm(datt, model=model12, xreg=NULL, link="log", distr = "nbinom")
ts.log.null.13 = tsglm(datt, model=model13, xreg=NULL, link="log", distr = "nbinom")
ts.log.null.01 = tsglm(datt, model=model01,xreg=NULL,link="log",distr="nbinom")
ts.log.null.02 = tsglm(datt, model=model02,xreg=NULL,link="log",distr="nbinom")
ts.log.null.03 = tsglm(datt, model=model03,xreg=NULL,link="log",distr="nbinom")

ml.sp.null.11 =  NB_ml(datt, model=model11      , init = c(1/ts.log.null.11$sigmasq,ts.log.null.11$coef), xreg=NULL, link="sp")
ml.sp.null.12 =  NB_ml(datt, model=model12      , init = c(1/ts.log.null.12$sigmasq,ts.log.null.12$coef), xreg=NULL, link="sp")
ml.sp.null.13 =  NB_ml(datt, model=model13      , init = c(1/ts.log.null.13$sigmasq,ts.log.null.13$coef), xreg=NULL, link="sp")
ml.sp.null.01 =  NB_ml_ARCH(datt, model=model01, init = c(1/ts.log.null.01$sigmasq,ts.log.null.01$coef), xreg=NULL, link="sp")
ml.sp.null.02 =  NB_ml_ARCH(datt, model=model02, init = c(1/ts.log.null.02$sigmasq,ts.log.null.02$coef), xreg=NULL, link="sp")
ml.sp.null.03 =  NB_ml_ARCH(datt, model=model03, init = c(1/ts.log.null.01$sigmasq,ts.log.null.03$coef), xreg=NULL, link="sp")

ml.log.null.11 =  NB_ml(datt, model=model11      , init = c(1/ts.log.null.11$sigmasq,ts.log.null.11$coef), xreg=NULL, link="log")
ml.log.null.12 =  NB_ml(datt, model=model12      , init = c(1/ts.log.null.12$sigmasq,ts.log.null.12$coef), xreg=NULL, link="log")
ml.log.null.13 =  NB_ml(datt, model=model13      , init = c(1/ts.log.null.13$sigmasq,ts.log.null.13$coef), xreg=NULL, link="log")
ml.log.null.01 =  NB_ml_ARCH(datt, model=model01, init = c(1/ts.log.null.01$sigmasq,ts.log.null.01$coef), xreg=NULL, link="log")
ml.log.null.02 =  NB_ml_ARCH(datt, model=model02, init = c(1/ts.log.null.02$sigmasq,ts.log.null.02$coef), xreg=NULL, link="log")
ml.log.null.03 =  NB_ml_ARCH(datt, model=model03, init = c(1/ts.log.null.01$sigmasq,ts.log.null.03$coef), xreg=NULL, link="log")



###For fair comparison that the first three obs be removed in lik calculation.
aic.null = 
  data.frame(
    SP = 
      c(
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.sp.null.01$fitted_value[-(1:2)],size = ml.sp.null.02$disp,log = T)) + 2*length(ml.sp.null.01$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.sp.null.11$fitted_value[-(1:2)],size = ml.sp.null.11$disp,log = T)) + 2*length(ml.sp.null.11$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.sp.null.02$fitted_value[-1]    ,size = ml.sp.null.02$disp,log = T)) + 2*length(ml.sp.null.02$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.sp.null.12$fitted_value[-1]    ,size = ml.sp.null.12$disp,log = T)) + 2*length(ml.sp.null.12$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.sp.null.03$fitted_value        ,size = ml.sp.null.03$disp,log = T)) + 2*length(ml.sp.null.03$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.sp.null.13$fitted_value        ,size = ml.sp.null.13$disp,log = T)) + 2*length(ml.sp.null.13$estimates)
      ),
    log=
      c(
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.log.null.01$fitted_value[-(1:2)],size = ml.log.null.02$disp,log = T)) + 2*length(ml.log.null.01$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.log.null.11$fitted_value[-(1:2)],size = ml.log.null.11$disp,log = T)) + 2*length(ml.log.null.11$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.log.null.02$fitted_value[-1]    ,size = ml.log.null.02$disp,log = T)) + 2*length(ml.log.null.02$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.log.null.12$fitted_value[-1]    ,size = ml.log.null.12$disp,log = T)) + 2*length(ml.log.null.12$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.log.null.03$fitted_value        ,size = ml.log.null.03$disp,log = T)) + 2*length(ml.log.null.03$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = ml.log.null.13$fitted_value        ,size = ml.log.null.13$disp,log = T)) + 2*length(ml.log.null.13$estimates)
      )
  )

####NULL model comparison - Poisson  ####
####NULL model comparison - Poisson  ####
####NULL model comparison - Poisson  ####
####NULL model comparison - Poisson  ####

ts.poi.log.null.01 = tsglm(datt, model=model01,xreg=NULL,link="log")
ts.poi.log.null.11 = tsglm(datt, model=model11,xreg=NULL,link="log")
ts.poi.log.null.02 = tsglm(datt, model=model02,xreg=NULL,link="log")
ts.poi.log.null.12 = tsglm(datt, model=model12,xreg=NULL,link="log")
ts.poi.log.null.03 = tsglm(datt, model=model03,xreg=NULL,link="log")
ts.poi.log.null.13 = tsglm(datt, model=model13,xreg=NULL,link="log")

poi.sp.null.11 =  Poi_ml(datt, model=model11      , init = c(ts.poi.log.null.11$coef), xreg=NULL, link="sp")
poi.sp.null.12 =  Poi_ml(datt, model=model12      , init = c(ts.poi.log.null.12$coef), xreg=NULL, link="sp")
poi.sp.null.13 =  Poi_ml(datt, model=model13      , init = c(ts.poi.log.null.13$coef), xreg=NULL, link="sp")
poi.sp.null.01 =  Poi_ml_ARCH(datt, model=model01, init = c(ts.poi.log.null.01$coef), xreg=NULL, link="sp")
poi.sp.null.02 =  Poi_ml_ARCH(datt, model=model02, init = c(ts.poi.log.null.02$coef), xreg=NULL, link="sp")
poi.sp.null.03 =  Poi_ml_ARCH(datt, model=model03, init = c(ts.poi.log.null.03$coef), xreg=NULL, link="sp")

poi.log.null.11 =  Poi_ml(datt, model=model11      , init = c(ts.poi.log.null.11$coef), xreg=NULL, link="log")
poi.log.null.12 =  Poi_ml(datt, model=model12      , init = c(ts.poi.log.null.12$coef), xreg=NULL, link="log")
poi.log.null.13 =  Poi_ml(datt, model=model13      , init = c(ts.poi.log.null.13$coef), xreg=NULL, link="log")
poi.log.null.01 =  Poi_ml_ARCH(datt, model=model01, init = c(ts.poi.log.null.01$coef), xreg=NULL, link="log")
poi.log.null.02 =  Poi_ml_ARCH(datt, model=model02, init = c(ts.poi.log.null.02$coef), xreg=NULL, link="log")
poi.log.null.03 =  Poi_ml_ARCH(datt, model=model03, init = c(ts.poi.log.null.03$coef), xreg=NULL, link="log")



###For fair comparison that the first three obs be removed in lik calculation.
aic.poi.null = 
  data.frame(
    SP = 
      c(
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.sp.null.01$fitted_value[-(1:2)],log = T)) + 2*length(poi.sp.null.01$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.sp.null.11$fitted_value[-(1:2)],log = T)) + 2*length(poi.sp.null.11$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.sp.null.02$fitted_value[-1]    ,log = T)) + 2*length(poi.sp.null.02$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.sp.null.12$fitted_value[-1]    ,log = T)) + 2*length(poi.sp.null.12$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.sp.null.03$fitted_value        ,log = T)) + 2*length(poi.sp.null.03$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.sp.null.13$fitted_value        ,log = T)) + 2*length(poi.sp.null.13$estimates)
      ),
    log=
      c(
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.log.null.01$fitted_value[-(1:2)],log = T)) + 2*length(poi.log.null.01$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.log.null.11$fitted_value[-(1:2)],log = T)) + 2*length(poi.log.null.11$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.log.null.02$fitted_value[-1]    ,log = T)) + 2*length(poi.log.null.02$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.log.null.12$fitted_value[-1]    ,log = T)) + 2*length(poi.log.null.12$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.log.null.03$fitted_value        ,log = T)) + 2*length(poi.log.null.03$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.log.null.13$fitted_value        ,log = T)) + 2*length(poi.log.null.13$estimates)
      )
  )

xtable::xtable(t(aic.null),digits = 4)
xtable::xtable(t(aic.poi.null),digits = 4)
####PIT plot#####

#########PIT plots for selected null models
#########PIT plots for selected null models
#########PIT plots for selected null models
#########PIT plots for selected null models
pit.null.log.03   = pit_mar((1:10)/10,NB_u(datt[-(1:3)],ml.log.null.03))
pit.null.log.03   = c(pit.null.log.03[1],diff(pit.null.log.03))
pit.null.sp.03   = pit_mar((1:10)/10,NB_u(datt[-(1:3)],ml.sp.null.03))
pit.null.sp.03   = c(pit.null.sp.03[1],diff(pit.null.sp.03))
x11()
par(mfrow=c(1,2))
hist(rep(seq(0.05,0.95,by=0.1),round(pit.null.log.03*1000)),breaks = 10,
     freq=F,ylim=c(0,2),main="NB log-INGARCH(0,3)",xlab="Probability integral Transform", ylab="Density")
abline(h=1,lwd=2,lty=2,col="blue")
hist(rep(seq(0.05,0.95,by=0.1),round(pit.null.sp.03*1000)),breaks = 10,
     freq=F,ylim=c(0,2),main="NB SP-INGARCH(0,3)",xlab="Probability integral Transform", ylab="Density")
abline(h=1,lwd=2,lty=2,col="blue")
par(mfrow=c(1,1))










###Significance detection based on NB-sp-INGARCH(1,1)
###Significance detection based on NB-sp-INGARCH(1,1)
###Significance detection based on NB-sp-INGARCH(1,1)
###Significance detection based on NB-sp-INGARCH(1,1)
feature = ht.covar[1:153,1:18]
feature[,c(2:4,13:17)] = NA
feature[-1,2]  = diff(ht.covar[1:153,2])
feature[-1,3]  = diff(ht.covar[1:153,3])
feature[-1,4]  = diff(log(ht.covar[1:153,4]))
feature[-1,13] = diff(log(ht.covar[1:153,13]))
feature[-1,14] = diff(log(ht.covar[1:153,14]))
feature[-1,15] = diff(log(ht.covar[1:153,15]))
feature[-1,16] = diff(log(ht.covar[1:153,16]))
feature[-1,17] = diff(log(ht.covar[1:153,17]))


##SP-INGARCH(0,3)
sig.mat.03.sp = matrix(-100,ncol=6 , nrow=17) #a total of 17 covariates.
sig.aic.03.sp = matrix(NA,ncol=6 , nrow=17)
##log-INGARCH(0,3)
sig.mat.03.log = matrix(-100,ncol=6 , nrow=17) #a total of 17 covariates.
sig.aic.03.log = matrix(NA,ncol=6 , nrow=17)

for(j in 1:17){
  init.val = tsglm(datt,model = model03, link="log", distr="nbinom", xreg = feature[15:153,j+1])
  init.val = c(1/init.val$sigmasq,init.val$coef)
  init.val[6] = 0
  for(lag in 0:5){
    sig.fit = NB_ml(dat = datt, init = NULL, model=model03, link = "sp", xreg = as.matrix(feature[(15:153)-lag,j+1]))
    hess.qr = qr(sig.fit$hessian)
    sig.se = sqrt( diag(solve(qr.R(hess.qr))%*%solve(qr.Q(hess.qr)) )[ length(init.val) ])
    sig.mat.03.sp[j,lag+1] = sign( prod( sig.fit$estimates[ length(init.val) ] + c(-1,1)*2*sig.se )  )
    if(sig.mat.03.sp[j,lag+1]==1){ sig.aic.03.sp[j,lag+1] = sig.fit$aic }
    
    sig.fit = NB_ml(dat = datt, init = NULL, model=model03, link = "log", xreg = as.matrix(feature[(15:153)-lag,j+1]))
    hess.qr = qr(sig.fit$hessian)
    sig.se = sqrt( diag(solve(qr.R(hess.qr))%*%solve(qr.Q(hess.qr)) )[ length(init.val) ])
    sig.mat.03.log[j,lag+1] = sign( prod( sig.fit$estimates[ length(init.val) ] + c(-1,1)*2*sig.se )  )
    if(sig.mat.03.log[j,lag+1]==1){ sig.aic.03.log[j,lag+1] = sig.fit$aic }
  }
  cat("j=",j,"\n")
}
rownames(sig.aic.03.sp) = colnames(feature)[-1]
sig.aic.03.sp = sig.aic.03.sp[apply(sig.aic.03.sp,1,mean)!=10000,]
apply(sig.aic.03.sp,1,which.min)
apply(sig.aic.03.sp,1,min)  

rownames(sig.aic.03.log) = colnames(feature)[-1]
sig.aic.03.log = sig.aic.03.log[apply(sig.aic.03.log,1,mean)!=10000,]
apply(sig.aic.03.log,1,which.min)
apply(sig.aic.03.log,1,min) 

print(xtable::xtable(data.frame(sig.aic[c(1,2,4,5,3,6),]),digits=4)) 
###The end of significance detection based on NB-sp-INGARCH(1,1)
###The end of significance detection based on NB-sp-INGARCH(1,1)
###The end of significance detection based on NB-sp-INGARCH(1,1)
###The end of significance detection based on NB-sp-INGARCH(1,1)
###The end of significance detection based on NB-sp-INGARCH(1,1)

