##One-step ahead prediction
##One-step ahead prediction
##One-step ahead prediction

sp.03.pred = log.03.pred = sp.03.pred.reduced = log.03.pred.reduced =0 
for (pr in 101:139){
  
  sp.03  =  NB_ml_ARCH(datt[1:(pr-1)], model=model03, link="sp",  init = multi.sp.03.from.sp$estimates   , xreg = as.matrix(feature.sp.03[1:(pr-1),]))
  log.03 =  NB_ml_ARCH(datt[1:(pr-1)], model=model03, link="log", init = multi.log.03.from.log$estimates , xreg = as.matrix(feature.log.03[1:(pr-1),]))
  sp.03.reduced  =  NB_ml_ARCH(datt[1:(pr-1)], model=model03, link="sp",  init = reduced.03.sp$estimates , xreg = as.matrix(feature.sp.03[1:(pr-1),c(1,4,5)]))
  log.03.reduced =  NB_ml_ARCH(datt[1:(pr-1)], model=model03, link="log", init = reduced.03.log$estimates, xreg = as.matrix(feature.log.03[1:(pr-1),c(1,5,6)]))
  
  sp.03.pred[pr-100]          = ingarch.pred(sp.03 ,link="sp" , xreg = as.matrix(feature.sp.03[pr,]))
  log.03.pred[pr-100]         = ingarch.pred(log.03        ,link="log", xreg = as.matrix(feature.log.03[pr,]))
  sp.03.pred.reduced[pr-100]  = ingarch.pred(sp.03.reduced ,link="sp" , xreg = as.matrix(feature.sp.03[pr,c(1,4,5)]))
  log.03.pred.reduced[pr-100] = ingarch.pred(log.03.reduced,link="log", xreg = as.matrix(feature.log.03[pr,c(1,5,6)]))
  
  print(pr)
}

msfe = apply(
  cbind(sp.03.pred, log.03.pred),
  2,function(x){cumsum( (x-datt[101:139])^2)/(1:39) } )

msfe.reduced = 
  apply(
    cbind(sp.03.pred.reduced, log.03.pred.reduced),
    2,function(x){cumsum( (x-datt[101:139])^2)/(1:39) } )


x11()
par(mfrow=c(1,2))
plot(91:139, datt[91:139], type="l",xlab="Time",ylab="Counts",xaxt = "n",main = "One-step-ahead prediction",ylim=c(0,30))
axis(1, at = c(7,19,31,43)+90, labels = as.numeric(format(datt.month[91:139][c(7,19,31,43)],'%Y')))
points(101:139 ,sp.03.pred.reduced , type="l", col="blue", lty=3,lwd=2) #sp.reduced
points(101:139 ,log.03.pred.reduced , type="l", col="red", lty=2,lwd=2) #log.reduced
legend("topleft",c("Observed","Reduced-log","Reduced-softplus"),lwd=c(1,2,2),col=c("black","red","blue"),lty=1:3)

plot(101:139 ,msfe.reduced[,1] , type="l", col="blue", lty=3,lwd=2,ylim=c(0,15),main="MSFE", xlab="Time",ylab = "MSFE",xaxt="n")
axis(1, at = c(7,19,31)+100, labels = as.numeric(format(datt.month[101:139][c(7,19,31)],'%Y')))
points(101:139 ,msfe.reduced[,2] , type="l", col="red", lty=2,lwd=2) #log.reduced
legend("bottomright",c("Reduced-log","Reduced-softplus"),col=c("red","blue"),lty=2:3,lwd=2)
par(mfrow=c(1,1))