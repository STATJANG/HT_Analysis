##NB INGARCH(0,3) regressions with jointly significant covariates.

feature.log.03 = cbind(feature[(15:153)-5,2 ], feature[(15:153)-1,3 ], feature[(15:153)-0,4 ],
                       feature[(15:153)-2,10], feature[(15:153)-4,11], feature[(15:153)-0,12],
                       feature[(15:153)-1,14], feature[(15:153)-5,15], feature[(15:153)-0,16], feature[(15:153)-0,17])
feature.sp.03  = cbind(feature[(15:153)-5,2 ], feature[(15:153)-1,3 ], feature[(15:153)-0,4 ],
                                               feature[(15:153)-4,11], feature[(15:153)-0,12],
                                               feature[(15:153)-5,15], feature[(15:153)-0,16], feature[(15:153)-0,17])

colnames(feature.log.03) = colnames(feature)[c(2:4,10:12,14:17)]
colnames(feature.sp.03)  = colnames(feature)[c(2:4,11,12,15:17)]

####Without further variable selection 
####Without further variable selection
####Without further variable selection


##REGRESSION WITH Covariates selected from log-INGARCH
multi.log.01.from.log = NB_ml_ARCH(datt, model = model01, link="log", xreg = as.matrix(feature.log.03))
multi.log.02.from.log = NB_ml_ARCH(datt, model = model02, link="log", xreg = as.matrix(feature.log.03))
multi.log.03.from.log = NB_ml_ARCH(datt, model = model03, link="log", xreg = as.matrix(feature.log.03))
multi.log.11.from.log = NB_ml(     datt, model = model11, link="log", xreg = as.matrix(feature.log.03))
multi.log.12.from.log = NB_ml(     datt, model = model12, link="log", xreg = as.matrix(feature.log.03))
multi.log.13.from.log = NB_ml(     datt, model = model13, link="log", xreg = as.matrix(feature.log.03))

poi.multi.log.01.from.log = Poi_ml_ARCH(datt, model = model01, link="log", xreg = as.matrix(feature.log.03))
poi.multi.log.02.from.log = Poi_ml_ARCH(datt, model = model02, link="log", xreg = as.matrix(feature.log.03))
poi.multi.log.03.from.log = Poi_ml_ARCH(datt, model = model03, link="log", xreg = as.matrix(feature.log.03))
poi.multi.log.11.from.log = Poi_ml(     datt, model = model11, link="log", xreg = as.matrix(feature.log.03))
poi.multi.log.12.from.log = Poi_ml(     datt, model = model12, link="log", xreg = as.matrix(feature.log.03))
poi.multi.log.13.from.log = Poi_ml(     datt, model = model13, link="log", xreg = as.matrix(feature.log.03))


##REGRESSION WITH Covariates selected from SP-INGARCH
multi.sp.01.from.sp = NB_ml_ARCH(datt, model = model01, link="sp", xreg = as.matrix(feature.sp.03))
multi.sp.02.from.sp = NB_ml_ARCH(datt, model = model02, link="sp", xreg = as.matrix(feature.sp.03))
multi.sp.03.from.sp = NB_ml_ARCH(datt, model = model03, link="sp", xreg = as.matrix(feature.sp.03))
multi.sp.11.from.sp = NB_ml(     datt, model = model11, link="sp", xreg = as.matrix(feature.sp.03))
multi.sp.12.from.sp = NB_ml(     datt, model = model12, link="sp", xreg = as.matrix(feature.sp.03))
multi.sp.13.from.sp = NB_ml(     datt, model = model13, link="sp", xreg = as.matrix(feature.sp.03))

poi.multi.sp.01.from.sp = Poi_ml_ARCH(datt, model = model01, link="sp", xreg = as.matrix(feature.sp.03))
poi.multi.sp.02.from.sp = Poi_ml_ARCH(datt, model = model02, link="sp", xreg = as.matrix(feature.sp.03))
poi.multi.sp.03.from.sp = Poi_ml_ARCH(datt, model = model03, link="sp", xreg = as.matrix(feature.sp.03))
poi.multi.sp.11.from.sp = Poi_ml(     datt, model = model11, link="sp", xreg = as.matrix(feature.sp.03))
poi.multi.sp.12.from.sp = Poi_ml(     datt, model = model12, link="sp", xreg = as.matrix(feature.sp.03))
poi.multi.sp.13.from.sp = Poi_ml(     datt, model = model13, link="sp", xreg = as.matrix(feature.sp.03))



##AIC tables.
aic.03 = 
  data.frame(
    sp = 
      c(
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.sp.01.from.sp$fitted_value[-(1:2)],size = multi.sp.01.from.sp$disp,log = T)) + 2*length(multi.sp.01.from.sp$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.sp.11.from.sp$fitted_value[-(1:2)],size = multi.sp.11.from.sp$disp,log = T)) + 2*length(multi.sp.11.from.sp$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.sp.02.from.sp$fitted_value[-1]    ,size = multi.sp.02.from.sp$disp,log = T)) + 2*length(multi.sp.02.from.sp$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.sp.12.from.sp$fitted_value[-1]    ,size = multi.sp.12.from.sp$disp,log = T)) + 2*length(multi.sp.12.from.sp$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.sp.03.from.sp$fitted_value        ,size = multi.sp.03.from.sp$disp,log = T)) + 2*length(multi.sp.03.from.sp$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.sp.13.from.sp$fitted_value        ,size = multi.sp.13.from.sp$disp,log = T)) + 2*length(multi.sp.13.from.sp$estimates)
      ),
    log=
      c(
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.log.01.from.log$fitted_value[-(1:2)],size = multi.log.01.from.log$disp,log = T)) + 2*length(multi.log.01.from.log$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.log.11.from.log$fitted_value[-(1:2)],size = multi.log.11.from.log$disp,log = T)) + 2*length(multi.log.11.from.log$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.log.02.from.log$fitted_value[-1]    ,size = multi.log.02.from.log$disp,log = T)) + 2*length(multi.log.02.from.log$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.log.12.from.log$fitted_value[-1]    ,size = multi.log.12.from.log$disp,log = T)) + 2*length(multi.log.12.from.log$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.log.03.from.log$fitted_value        ,size = multi.log.03.from.log$disp,log = T)) + 2*length(multi.log.03.from.log$estimates),
        -2*sum(dnbinom(datt[-(1:3)], mu = multi.log.13.from.log$fitted_value        ,size = multi.log.13.from.log$disp,log = T)) + 2*length(multi.log.13.from.log$estimates)
      )
  )

aic.03.poi = 
  data.frame(
    sp = 
      c(
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.sp.01.from.sp$fitted_value[-(1:2)],log = T)) + 2*length(poi.multi.sp.01.from.sp$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.sp.11.from.sp$fitted_value[-(1:2)],log = T)) + 2*length(poi.multi.sp.11.from.sp$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.sp.02.from.sp$fitted_value[-1]    ,log = T)) + 2*length(poi.multi.sp.02.from.sp$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.sp.12.from.sp$fitted_value[-1]    ,log = T)) + 2*length(poi.multi.sp.12.from.sp$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.sp.03.from.sp$fitted_value        ,log = T)) + 2*length(poi.multi.sp.03.from.sp$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.sp.13.from.sp$fitted_value        ,log = T)) + 2*length(poi.multi.sp.13.from.sp$estimates)
      ),
    log=
      c(
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.log.01.from.log$fitted_value[-(1:2)],log = T)) + 2*length(poi.multi.log.01.from.log$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.log.11.from.log$fitted_value[-(1:2)],log = T)) + 2*length(poi.multi.log.11.from.log$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.log.02.from.log$fitted_value[-1]    ,log = T)) + 2*length(poi.multi.log.02.from.log$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.log.12.from.log$fitted_value[-1]    ,log = T)) + 2*length(poi.multi.log.12.from.log$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.log.03.from.log$fitted_value        ,log = T)) + 2*length(poi.multi.log.03.from.log$estimates),
        -2*sum(dpois(datt[-(1:3)], lambda  =  poi.multi.log.13.from.log$fitted_value        ,log = T)) + 2*length(poi.multi.log.13.from.log$estimates)
      )
  )


aic.03
aic.03.poi
rbind(
  round(t(aic.03.poi[c(1,3,5),]),2)[1,,drop=F],
  round(t(aic.03[c(1,3,5),]),2)[1,,drop=F],
  round(t(aic.03.poi[c(1,3,5),]),2)[2,,drop=F],
  round(t(aic.03[c(1,3,5),]),2)[2,,drop=F]
)


#####Whether estimates satisfy stationarity condition?
#From sp
multi.log.01.from.log$estimates
multi.log.02.from.log$estimates
multi.log.03.from.log$estimates
multi.log.11.from.log$estimates  #Nearly  #non-stationary
multi.log.12.from.log$estimates  #non-stationary
multi.log.13.from.log$estimates  #non-stationary

poi.multi.log.01.from.log$estimates
poi.multi.log.02.from.log$estimates
poi.multi.log.03.from.log$estimates
poi.multi.log.11.from.log$estimates  #Nearly non-stationary
poi.multi.log.12.from.log$estimates  #non-stationary
poi.multi.log.13.from.log$estimates  #non-stationary



## From sp
multi.sp.01.from.sp$estimates
multi.sp.02.from.sp$estimates
multi.sp.03.from.sp$estimates
multi.sp.11.from.sp$estimates #Nearly non-stationary
multi.sp.12.from.sp$estimates #non-stationary
multi.sp.13.from.sp$estimates  #non-stationary

poi.multi.sp.01.from.sp$estimates
poi.multi.sp.02.from.sp$estimates
poi.multi.sp.03.from.sp$estimates
poi.multi.sp.11.from.sp$estimates #Nearly non-stationary
poi.multi.sp.12.from.sp$estimates #non-stationary
poi.multi.sp.13.from.sp$estimates  #non-stationary





#####PIT and acf plot for sp link
pit.multi.sp.03.from.log  = pit_mar((1:10)/10,NB_u(datt[-(1:3)],multi.sp.03.from.log))
pit.multi.sp.03.from.log  = c(pit.multi.sp.03.from.log[1],diff(pit.multi.sp.03.from.log))
pit.multi.sp.03.from.sp   = pit_mar((1:10)/10,NB_u(datt[-(1:3)],multi.sp.03.from.sp))
pit.multi.sp.03.from.sp   = c(pit.multi.sp.03.from.sp[1],diff(pit.multi.sp.03.from.sp))

x11()
par(mfrow=c(2,2))
hist(rep(seq(0.05,0.95,by=0.1),round(pit.multi.sp.03.from.log*1000)),breaks = 10,
     freq=F,ylim=c(0,2),main="Model1",xlab="Probability integral Transform", ylab="Density")
abline(h=1,lwd=2,lty=2,col="blue")
acf(multi.sp.03.from.log$pearson,main = "Model 1" )
hist(rep(seq(0.05,0.95,by=0.1),round(pit.multi.sp.03.from.sp*1000)),breaks = 10,
     freq=F,ylim=c(0,2),main="Model2",xlab="Probability integral Transform", ylab="Density")
abline(h=1,lwd=2,lty=2,col="blue")
acf(multi.sp.03.from.sp$pearson,main = "Model2" )
par(mfrow=c(1,1))

#pb.multi.sp.03.from.sp = NB_ml_pb(multi.sp.03.from.sp,pb_size=2000)
#pb.multi.log.03.from.log = NB_ml_pb(multi.log.03.from.log,pb_size=2000)
print(data.frame(sd = apply(pb.multi.sp.03.from.sp,2,sd)),digits=2,row.names=F)

print(data.frame(t(apply(pb.multi.sp.03.from.sp,2,quantile,c(0.025,0.975)))),digits=2,row.names=F)
print(data.frame(t(apply(pb.multi.log.03.from.log,2,quantile,c(0.025,0.975)))),digits=2,row.names=F)

print(round(data.frame(x=multi.log.03.from.log$estimates),4),row.names=F)
print(round(data.frame(sd = apply(pb.multi.log.03.from.log,2,sd)),4),row.names=F)
print(round(data.frame(t(apply(pb.multi.log.03.from.log,2,quantile,c(0.025,0.975)))),4),row.names=F)

print(round(data.frame(x=multi.sp.03.from.sp$estimates),4),row.names=F)
print(round(data.frame(sd = apply(pb.multi.sp.03.from.sp,2,sd)),4),row.names=F)
print(round(data.frame(t(apply(pb.multi.sp.03.from.sp,2,quantile,c(0.025,0.975)))),4),row.names=F)




#####Reduced model
#####Reduced model
#####Reduced model
#####Reduced model

reduced.03.log = NB_ml_ARCH(datt, model = model03, link="log", xreg = as.matrix(feature.log.03[,c(1,5,6)]))
reduced.03.sp  = NB_ml_ARCH(datt, model = model03, link="sp", xreg = as.matrix(feature.sp.03[,c(1,4,5)]))
#pb.reduced.03.log = NB_ml_pb(reduced.03.log, pb_size=2000)
#pb.reduced.03.sp = NB_ml_pb(reduced.03.sp, pb_size=2000)

print(round(data.frame(coef=reduced.03.log$estimates,sd=apply(pb.reduced.03.log,2,sd), t(apply(pb.reduced.03.log,2,quantile,c(0.025,0.975)))),4),row.names=F)
print(round(data.frame(coef=reduced.03.sp$estimates ,sd=apply(pb.reduced.03.sp,2,sd) , t(apply(pb.reduced.03.sp ,2,quantile,c(0.025,0.975)))),4),row.names=F)

#####PIT plot for reduced models
pit.reduced.03.log  = pit_mar((1:10)/10,NB_u(datt[-(1:3)],reduced.03.log))
pit.reduced.03.log  = c(pit.reduced.03.log[1],diff(pit.reduced.03.log))
pit.reduced.03.sp  = pit_mar((1:10)/10,NB_u(datt[-(1:3)],reduced.03.sp))
pit.reduced.03.sp  = c(pit.reduced.03.sp[1],diff(pit.reduced.03.sp))

x11()
par(mfrow=c(2,2))
hist(rep(seq(0.05,0.95,by=0.1),round(pit.reduced.03.log*1000)),breaks = 10,
     freq=F,ylim=c(0,2),main="Reduced NB log-INGARCH(0,3) Regression",xlab="Probability integral Transform", ylab="Density")
abline(h=1,lwd=2,lty=2,col="blue")
hist(rep(seq(0.05,0.95,by=0.1),round(pit.reduced.03.sp*1000)),breaks = 10,
     freq=F,ylim=c(0,2),main="Reduced NB SP-INGARCH(0,3) Regression",xlab="Probability integral Transform", ylab="Density")
abline(h=1,lwd=2,lty=2,col="blue")

acf(reduced.03.log$pearson,main = "Reduced NB log-INGARCH(0,3) Regression" )
acf(reduced.03.sp$pearson,main = "Reduced NB SP-INGARCH(0,3) Regression" )
par(mfrow=c(1,1))

