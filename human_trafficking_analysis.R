library(forecast)
library(tscount)
library(ggplot2)
library(cowplot)
library(anytime)

###Loading the data.
#ht.count = read.csv("ht_count_cleaned.csv")
#ht.covar = read.csv("ht_covariate_cleaned.csv")

plot(ht.count[,2],type="l",ylab="",xlab="",main="Counts of Human Trafficking in US")
##Correlation 
par(mfrow=c(1,2))
acf(ht.count[,2]);pacf(ht.count[,2])
par(mfrow=c(1,1))
mean(ht.count$count) ; var(ht.count$count)

#ggplot
ggplot.ht <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")
ggplot.ht
ggAcf(ht.count$count,lag.max=25)+labs(title="")
#ggPacf(ht.count$count,lag.max=25)+labs(title='PACF of monthly human trafficking cases')


##Without covariates
ht.log = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),link="log",distr = "nbinom")
ht.log.nb = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),link="log",distr = "nbinom")
par(mfrow=c(1,2))
acf(ht.log$residuals);pacf(ht.log$residuals)
par(mfrow=c(1,1))
pit(ht.log)
pit(ht.log.nb)


##Estimated Mean process
plot(ht.count[,2],type="o",ylab="",xlab="",main="Counts of Human Trafficking in US")#,xaxt="n")
#axis(1,at=c(1:length(ht.count[-1,2]))[seq(1,146,by=6)],labels=format(anytime::anydate(ht.count$month),"%m %y")[seq(1,146,by=6)],las=2)
points(ht.log$fitted.values,pch=20,col="red",type="l",lwd=2)

pdf("htcount_pit.pdf",width = 10,height = 5)
par(mfrow=c(1,2))
pit(ht.log,xlab="",ylab="",main="Poisson-INGARCH")
pit(ht.log.nb,xlab="",ylab="",main="NB-INGARCH");par(mfrow=c(1,1))
dev.off()

###REGRESSION
#cpi
ht.cpi = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)]),link="log",distr = "nbinom")
ht.cpi1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-1]),link="log",distr = "nbinom")
ht.cpi2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-2]),link="log",distr = "nbinom")
ht.cpi3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-3]),link="log",distr = "nbinom")
ht.cpi4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-4]),link="log",distr = "nbinom")
ht.cpi5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-5]),link="log",distr = "nbinom")
summary(ht.cpi)
summary(ht.cpi1)
summary(ht.cpi2)
summary(ht.cpi3)
summary(ht.cpi4)
summary(ht.cpi5)


ht.gdp = tsglm(ht.count$count[-139],model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:152)]),link="log",distr = "nbinom")
ht.gdp1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-1]),link="log",distr = "nbinom")
ht.gdp2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-2]),link="log",distr = "nbinom")
ht.gdp3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-3]),link="log",distr = "nbinom")
ht.gdp4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-4]),link="log",distr = "nbinom")
ht.gdp5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-5]),link="log",distr = "nbinom")
summary(ht.gdp)
summary(ht.gdp1)
summary(ht.gdp2)
summary(ht.gdp3)
summary(ht.gdp4)
summary(ht.gdp5)


ht.unemp = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)])),link="log",distr = "nbinom")
ht.unemp1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-1])),link="log",distr = "nbinom")
ht.unemp2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-2])),link="log",distr = "nbinom")
ht.unemp3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-3])),link="log",distr = "nbinom")
ht.unemp4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-4])),link="log",distr = "nbinom")
ht.unemp5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-5])),link="log",distr = "nbinom")
summary(ht.unemp)
summary(ht.unemp1)
summary(ht.unemp2)
summary(ht.unemp3)
summary(ht.unemp4)
summary(ht.unemp5)


ht.COMPMAT = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)],link="log",distr = "nbinom")
ht.COMPMAT1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-1],link="log",distr = "nbinom")
ht.COMPMAT2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-2],link="log",distr = "nbinom")
ht.COMPMAT3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-3],link="log",distr = "nbinom")
ht.COMPMAT4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-4],link="log",distr = "nbinom")
ht.COMPMAT5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.COMPMAT)
summary(ht.COMPMAT1)
summary(ht.COMPMAT2)
summary(ht.COMPMAT3)
summary(ht.COMPMAT4)
summary(ht.COMPMAT5)



ht.WELFARE = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)],link="log",distr = "nbinom")
ht.WELFARE1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-1],link="log",distr = "nbinom")
ht.WELFARE2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-2],link="log",distr = "nbinom")
ht.WELFARE3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-3],link="log",distr = "nbinom")
ht.WELFARE4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-4],link="log",distr = "nbinom")
ht.WELFARE5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.WELFARE)
summary(ht.WELFARE1)
summary(ht.WELFARE2)
summary(ht.WELFARE3)
summary(ht.WELFARE4)
summary(ht.WELFARE5)



ht.FISCALPOL = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)],link="log",distr = "nbinom")
ht.FISCALPOL1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-1],link="log",distr = "nbinom")
ht.FISCALPOL2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-2],link="log",distr = "nbinom")
ht.FISCALPOL3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-3],link="log",distr = "nbinom")
ht.FISCALPOL4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-4],link="log",distr = "nbinom")
ht.FISCALPOL5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-5],link="log",distr = "nbinom")

summary(ht.FISCALPOL)
summary(ht.FISCALPOL1)
summary(ht.FISCALPOL2)
summary(ht.FISCALPOL3)
summary(ht.FISCALPOL4)
summary(ht.FISCALPOL5)





ht.NATSEC = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)],link="log",distr = "nbinom")
ht.NATSEC1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-1],link="log",distr = "nbinom")
ht.NATSEC2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-2],link="log",distr = "nbinom")
ht.NATSEC3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-3],link="log",distr = "nbinom")
ht.NATSEC4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-4],link="log",distr = "nbinom")
ht.NATSEC5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.NATSEC)
summary(ht.NATSEC1)
summary(ht.NATSEC2)
summary(ht.NATSEC3)
summary(ht.NATSEC4)
summary(ht.NATSEC5)



ht.AGRPOLICY = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)],link="log",distr = "nbinom")
ht.AGRPOLICY1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-1],link="log",distr = "nbinom")
ht.AGRPOLICY2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-2],link="log",distr = "nbinom")
ht.AGRPOLICY3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-3],link="log",distr = "nbinom")
ht.AGRPOLICY4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-4],link="log",distr = "nbinom")
ht.AGRPOLICY5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.AGRPOLICY)
summary(ht.AGRPOLICY1)
summary(ht.AGRPOLICY2)
summary(ht.AGRPOLICY3)
summary(ht.AGRPOLICY4)
summary(ht.AGRPOLICY5)





ht.EMNLABORREG = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)],link="log",distr = "nbinom")
ht.EMNLABORREG1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-1],link="log",distr = "nbinom")
ht.EMNLABORREG2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-2],link="log",distr = "nbinom")
ht.EMNLABORREG3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-3],link="log",distr = "nbinom")
ht.EMNLABORREG4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-4],link="log",distr = "nbinom")
ht.EMNLABORREG5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.EMNLABORREG)
summary(ht.EMNLABORREG1)
summary(ht.EMNLABORREG2)
summary(ht.EMNLABORREG3)
summary(ht.EMNLABORREG4)
summary(ht.EMNLABORREG5)

summary(ht.EMNLABORREG)$AIC
summary(ht.EMNLABORREG1)$AIC
summary(ht.EMNLABORREG2)$AIC
summary(ht.EMNLABORREG3)$AIC ##Selected.R
summary(ht.EMNLABORREG4)$AIC
summary(ht.EMNLABORREG5)$AIC
#ts.plot(ht.count$count,type="o")
#points(ht.EMNLABORREG3$fitted.values,col="red",pch=20,type="o")





ht.IMMIGRATION = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)],link="log",distr = "nbinom")
ht.IMMIGRATION1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-1],link="log",distr = "nbinom")
ht.IMMIGRATION2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-2],link="log",distr = "nbinom")
ht.IMMIGRATION3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-3],link="log",distr = "nbinom")
ht.IMMIGRATION4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-4],link="log",distr = "nbinom")
ht.IMMIGRATION5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.IMMIGRATION)
summary(ht.IMMIGRATION1)
summary(ht.IMMIGRATION2)
summary(ht.IMMIGRATION3)
summary(ht.IMMIGRATION4)
summary(ht.IMMIGRATION5)

summary(ht.IMMIGRATION)$AIC
summary(ht.IMMIGRATION1)$AIC
summary(ht.IMMIGRATION2)$AIC
summary(ht.IMMIGRATION3)$AIC
summary(ht.IMMIGRATION4)$AIC
summary(ht.IMMIGRATION5)$AIC
#ts.plot(ht.count$count,type="o")
#points(ht.IMMIGRATION$fitted.values,col="red",pch=20,type="o")




ht.LaborForceWomen = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)],link="log",distr = "nbinom")
ht.LaborForceWomen1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-1],link="log",distr = "nbinom")
ht.LaborForceWomen2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-2],link="log",distr = "nbinom")
ht.LaborForceWomen3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-3],link="log",distr = "nbinom")
ht.LaborForceWomen4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-4],link="log",distr = "nbinom")
ht.LaborForceWomen5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.LaborForceWomen);summary(ht.LaborForceWomen)$AIC
summary(ht.LaborForceWomen1);summary(ht.LaborForceWomen1)$AIC
summary(ht.LaborForceWomen2);summary(ht.LaborForceWomen2)$AIC
summary(ht.LaborForceWomen3);summary(ht.LaborForceWomen3)$AIC
summary(ht.LaborForceWomen4);summary(ht.LaborForceWomen4)$AIC
summary(ht.LaborForceWomen5);summary(ht.LaborForceWomen5)$AIC








ht.LaborForceBlack = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)],link="log",distr = "nbinom")
ht.LaborForceBlack1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-1],link="log",distr = "nbinom")
ht.LaborForceBlack2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-2],link="log",distr = "nbinom")
ht.LaborForceBlack3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-3],link="log",distr = "nbinom")
ht.LaborForceBlack4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-4],link="log",distr = "nbinom")
ht.LaborForceBlack5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.LaborForceBlack);summary(ht.LaborForceBlack)$AIC
summary(ht.LaborForceBlack1);summary(ht.LaborForceBlack1)$AIC
summary(ht.LaborForceBlack2);summary(ht.LaborForceBlack2)$AIC
summary(ht.LaborForceBlack3);summary(ht.LaborForceBlack3)$AIC
summary(ht.LaborForceBlack4);summary(ht.LaborForceBlack4)$AIC
summary(ht.LaborForceBlack5);summary(ht.LaborForceBlack5)$AIC




ht.LaborForceLatino = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)],link="log",distr = "nbinom")
ht.LaborForceLatino1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-1],link="log",distr = "nbinom")
ht.LaborForceLatino2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-2],link="log",distr = "nbinom")
ht.LaborForceLatino3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-3],link="log",distr = "nbinom")
ht.LaborForceLatino4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-4],link="log",distr = "nbinom")
ht.LaborForceLatino5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.LaborForceLatino);summary(ht.LaborForceLatino)$AIC
summary(ht.LaborForceLatino1);summary(ht.LaborForceLatino1)$AIC
summary(ht.LaborForceLatino2);summary(ht.LaborForceLatino2)$AIC
summary(ht.LaborForceLatino3);summary(ht.LaborForceLatino3)$AIC
summary(ht.LaborForceLatino4);summary(ht.LaborForceLatino4)$AIC
summary(ht.LaborForceLatino5);summary(ht.LaborForceLatino5)$AIC






ht.EmpPopRatioMen = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)],link="log",distr = "nbinom")
ht.EmpPopRatioMen1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-1],link="log",distr = "nbinom")
ht.EmpPopRatioMen2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-2],link="log",distr = "nbinom")
ht.EmpPopRatioMen3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-3],link="log",distr = "nbinom")
ht.EmpPopRatioMen4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-4],link="log",distr = "nbinom")
ht.EmpPopRatioMen5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.EmpPopRatioMen);summary(ht.EmpPopRatioMen)$AIC
summary(ht.EmpPopRatioMen1);summary(ht.EmpPopRatioMen1)$AIC
summary(ht.EmpPopRatioMen2);summary(ht.EmpPopRatioMen2)$AIC
summary(ht.EmpPopRatioMen3);summary(ht.EmpPopRatioMen3)$AIC
summary(ht.EmpPopRatioMen4);summary(ht.EmpPopRatioMen4)$AIC
summary(ht.EmpPopRatioMen5);summary(ht.EmpPopRatioMen5)$AIC








ht.EmpPopRatioWomen = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)],link="log",distr = "nbinom")
ht.EmpPopRatioWomen1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-1],link="log",distr = "nbinom")
ht.EmpPopRatioWomen2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-2],link="log",distr = "nbinom")
ht.EmpPopRatioWomen3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-3],link="log",distr = "nbinom")
ht.EmpPopRatioWomen4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-4],link="log",distr = "nbinom")
ht.EmpPopRatioWomen5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-5],link="log",distr = "nbinom")
summary(ht.EmpPopRatioWomen);summary(ht.EmpPopRatioWomen)$AIC
summary(ht.EmpPopRatioWomen1);summary(ht.EmpPopRatioWomen1)$AIC
summary(ht.EmpPopRatioWomen2);summary(ht.EmpPopRatioWomen2)$AIC
summary(ht.EmpPopRatioWomen3);summary(ht.EmpPopRatioWomen3)$AIC
summary(ht.EmpPopRatioWomen4);summary(ht.EmpPopRatioWomen4)$AIC
summary(ht.EmpPopRatioWomen5);summary(ht.EmpPopRatioWomen5)$AIC


####Significant variables
####Significant variables
####Significant variables
####Significant variables
####Significant variables
summary(ht.IMMIGRATION)
summary(ht.EMNLABORREG2)
summary(ht.AGRPOLICY)
summary(ht.IMMIGRATION)$QIC; summary(ht.EMNLABORREG2)$QIC ; summary(ht.AGRPOLICY)$QIC



###Fitted lines
###Fitted lines
###Fitted lines
ggplot.ht.null <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.log.nb$fitted.values),size=1,colour="blue")+geom_point()
ggplot.ht.im <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.IMMIGRATION$fitted.values),size=1,colour="red")+geom_point()
ggplot.ht.lab <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.EMNLABORREG2$fitted.values),size=1,colour="red")+geom_point()
ggplot.ht.agr <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.AGRPOLICY$fitted.values),size=1,colour="red")+geom_point()
plot_grid(ggplot.ht.null,ggplot.ht.im,ggplot.ht.lab,ggplot.ht.agr, labels = c("(a)","(b)","(c)","(d)"))
pdf("htcount_mean_process.pdf",height = 5,width=17)
plot_grid(ggplot.ht.im,ggplot.ht.lab,ggplot.ht.agr, labels = c("(a)","(b)","(c)"),nrow=1)
dev.off()


# For each of significant pull factors, comparison between NB vs Poisson)
ht.IMMIGRATION.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)],link="log")
ht.EMNLABORREG2.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-2],link="log")
ht.AGRPOLICY.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)],link="log")

pdf("htcount_pit_reg.pdf",height = 10,width=15)
par(mfrow=c(2,3))
pit(ht.IMMIGRATION.Poisson,ylim=c(0,2),main="Immigration Policiy (Poisson)",cex.main=2)
pit(ht.EMNLABORREG2.Poisson,ylim=c(0,2),main="Labor regulation (Poisson)",cex.main=2)
pit(ht.AGRPOLICY.Poisson,ylim=c(0,2),main="Agr. policy (Poisson)",cex.main=2)
pit(ht.IMMIGRATION,ylim=c(0,2),main="Immigration Policy (NB)",cex.main=2)
pit(ht.EMNLABORREG2,ylim=c(0,2),main="Labor regulation (NB)",cex.main=2)
pit(ht.AGRPOLICY,ylim=c(0,2),main="Agr. policy (NB)",cex.main=2)
dev.off()






##Time series plot of significant pull factors.
ggplot.agr <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.covar$EMVAGRPOLICY[c(15:153)]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")
  
ggplot.lab <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.covar$EMNLABORREG[c(15:153)-2]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")

ggplot.im <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.covar$EMVIMMIGRATION[c(15:153)]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")

x11()
plot_grid(ggplot.im,ggplot.lab,ggplot.agr,nrow=3, labels = "")#c("(a)","(b)","(c)"))




