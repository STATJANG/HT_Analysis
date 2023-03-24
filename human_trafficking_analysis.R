library(forecast)
library(tscount)
library(ggplot2)
library(cowplot)
library(anytime)

###Loading the data.
#ht.count = read.csv("ht_count_cleaned.csv")
#ht.covar = read.csv("C:/Users/statj/Documents/Human_trafficking_data/ht_covariate_cleaned.csv")

plot(ht.count[,2],type="l",ylab="",xlab="",main="Counts of Human Trafficking in US")
##Correlation 
par(mfrow=c(1,2))
acf(ht.count[,2]);pacf(ht.count[,2])
par(mfrow=c(1,1))
mean(ht.count$count) ; var(ht.count$count)

#ggplot
ggplot.ht <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")
pdf("htcount_tsplot.pdf",width=12)
ggplot.ht
dev.off()


pdf("htcount_acf.pdf",width=12)
ggAcf(ht.count$count,lag.max=25)+labs(title="")
dev.off()
#ggPacf(ht.count$count,lag.max=25)+labs(title='PACF of monthly human trafficking cases')


##Without covariates
ht.log = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),link="log")
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
ht.cpi = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)]),link="log",distr = "nbinom")
ht.cpi1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-1]),link="log",distr = "nbinom")
ht.cpi2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-2]),link="log",distr = "nbinom")
ht.cpi3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-3]),link="log",distr = "nbinom")
ht.cpi4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-4]),link="log",distr = "nbinom")
ht.cpi5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$cpi[c(14:153)-5]),link="log",distr = "nbinom")
ht.gdp = tsglm(ht.count$count[-139],model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:152)]),link="log",distr = "nbinom")
ht.gdp1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-1]),link="log",distr = "nbinom")
ht.gdp2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-2]),link="log",distr = "nbinom")
ht.gdp3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-3]),link="log",distr = "nbinom")
ht.gdp4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-4]),link="log",distr = "nbinom")
ht.gdp5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:153)-5]),link="log",distr = "nbinom")
ht.unemp = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)])),link="log",distr = "nbinom")
ht.unemp1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-1])),link="log",distr = "nbinom")
ht.unemp2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-2])),link="log",distr = "nbinom")
ht.unemp3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-3])),link="log",distr = "nbinom")
ht.unemp4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-4])),link="log",distr = "nbinom")
ht.unemp5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-5])),link="log",distr = "nbinom")
ht.COMPMAT = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)],link="log",distr = "nbinom")
ht.COMPMAT1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-1],link="log",distr = "nbinom")
ht.COMPMAT2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-2],link="log",distr = "nbinom")
ht.COMPMAT3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-3],link="log",distr = "nbinom")
ht.COMPMAT4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-4],link="log",distr = "nbinom")
ht.COMPMAT5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVCOMPMAT[c(15:153)-5],link="log",distr = "nbinom")
ht.WELFARE = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)],link="log",distr = "nbinom")
ht.WELFARE1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-1],link="log",distr = "nbinom")
ht.WELFARE2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-2],link="log",distr = "nbinom")
ht.WELFARE3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-3],link="log",distr = "nbinom")
ht.WELFARE4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-4],link="log",distr = "nbinom")
ht.WELFARE5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVWELFARE[c(15:153)-5],link="log",distr = "nbinom")
ht.FISCALPOL = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)],link="log",distr = "nbinom")
ht.FISCALPOL1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-1],link="log",distr = "nbinom")
ht.FISCALPOL2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-2],link="log",distr = "nbinom")
ht.FISCALPOL3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-3],link="log",distr = "nbinom")
ht.FISCALPOL4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-4],link="log",distr = "nbinom")
ht.FISCALPOL5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVFISCALPOL[c(15:153)-5],link="log",distr = "nbinom")
ht.NATSEC = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)],link="log",distr = "nbinom")
ht.NATSEC1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-1],link="log",distr = "nbinom")
ht.NATSEC2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-2],link="log",distr = "nbinom")
ht.NATSEC3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-3],link="log",distr = "nbinom")
ht.NATSEC4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-4],link="log",distr = "nbinom")
ht.NATSEC5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVNATSEC[c(15:153)-5],link="log",distr = "nbinom")
ht.AGRPOLICY = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)],link="log",distr = "nbinom")
ht.AGRPOLICY1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-1],link="log",distr = "nbinom")
ht.AGRPOLICY2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-2],link="log",distr = "nbinom")
ht.AGRPOLICY3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-3],link="log",distr = "nbinom")
ht.AGRPOLICY4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-4],link="log",distr = "nbinom")
ht.AGRPOLICY5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-5],link="log",distr = "nbinom")
ht.EMNLABORREG = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)],link="log",distr = "nbinom")
ht.EMNLABORREG1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-1],link="log",distr = "nbinom")
ht.EMNLABORREG2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-2],link="log",distr = "nbinom")
ht.EMNLABORREG3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-3],link="log",distr = "nbinom")
ht.EMNLABORREG4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-4],link="log",distr = "nbinom")
ht.EMNLABORREG5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-5],link="log",distr = "nbinom")
ht.IMMIGRATION = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)],link="log",distr = "nbinom")
ht.IMMIGRATION1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-1],link="log",distr = "nbinom")
ht.IMMIGRATION2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-2],link="log",distr = "nbinom")
ht.IMMIGRATION3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-3],link="log",distr = "nbinom")
ht.IMMIGRATION4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-4],link="log",distr = "nbinom")
ht.IMMIGRATION5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)-5],link="log",distr = "nbinom")
ht.LaborForceWomen = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)],link="log",distr = "nbinom")
ht.LaborForceWomen1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-1],link="log",distr = "nbinom")
ht.LaborForceWomen2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-2],link="log",distr = "nbinom")
ht.LaborForceWomen3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-3],link="log",distr = "nbinom")
ht.LaborForceWomen4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-4],link="log",distr = "nbinom")
ht.LaborForceWomen5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceWomen[c(15:153)-5],link="log",distr = "nbinom")
ht.LaborForceBlack = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)],link="log",distr = "nbinom")
ht.LaborForceBlack1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-1],link="log",distr = "nbinom")
ht.LaborForceBlack2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-2],link="log",distr = "nbinom")
ht.LaborForceBlack3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-3],link="log",distr = "nbinom")
ht.LaborForceBlack4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-4],link="log",distr = "nbinom")
ht.LaborForceBlack5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceBlack[c(15:153)-5],link="log",distr = "nbinom")
ht.LaborForceLatino = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)],link="log",distr = "nbinom")
ht.LaborForceLatino1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-1],link="log",distr = "nbinom")
ht.LaborForceLatino2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-2],link="log",distr = "nbinom")
ht.LaborForceLatino3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-3],link="log",distr = "nbinom")
ht.LaborForceLatino4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-4],link="log",distr = "nbinom")
ht.LaborForceLatino5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$LaborForceLatino[c(15:153)-5],link="log",distr = "nbinom")
ht.EmpPopRatioMen = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)],link="log",distr = "nbinom")
ht.EmpPopRatioMen1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-1],link="log",distr = "nbinom")
ht.EmpPopRatioMen2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-2],link="log",distr = "nbinom")
ht.EmpPopRatioMen3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-3],link="log",distr = "nbinom")
ht.EmpPopRatioMen4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-4],link="log",distr = "nbinom")
ht.EmpPopRatioMen5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioMen[c(15:153)-5],link="log",distr = "nbinom")
ht.EmpPopRatioWomen = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)],link="log",distr = "nbinom")
ht.EmpPopRatioWomen1 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-1],link="log",distr = "nbinom")
ht.EmpPopRatioWomen2 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-2],link="log",distr = "nbinom")
ht.EmpPopRatioWomen3 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-3],link="log",distr = "nbinom")
ht.EmpPopRatioWomen4 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-4],link="log",distr = "nbinom")
ht.EmpPopRatioWomen5 = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EmpPopRatioWomen[c(15:153)-5],link="log",distr = "nbinom")


summary(ht.cpi)
summary(ht.cpi1)
summary(ht.cpi2)
summary(ht.cpi3)
summary(ht.cpi4)
summary(ht.cpi5)

summary(ht.gdp)
summary(ht.gdp1)
summary(ht.gdp2)
summary(ht.gdp3)
summary(ht.gdp4)
summary(ht.gdp5)

summary(ht.unemp)
summary(ht.unemp1)
summary(ht.unemp2)
summary(ht.unemp3)
summary(ht.unemp4)
summary(ht.unemp5)

summary(ht.COMPMAT)
summary(ht.COMPMAT1)
summary(ht.COMPMAT2)
summary(ht.COMPMAT3)
summary(ht.COMPMAT4)
summary(ht.COMPMAT5)

summary(ht.WELFARE)
summary(ht.WELFARE1)
summary(ht.WELFARE2)
summary(ht.WELFARE3)
summary(ht.WELFARE4)
summary(ht.WELFARE5)

summary(ht.FISCALPOL)
summary(ht.FISCALPOL1)
summary(ht.FISCALPOL2)
summary(ht.FISCALPOL3)
summary(ht.FISCALPOL4)
summary(ht.FISCALPOL5)

summary(ht.NATSEC)
summary(ht.NATSEC1)
summary(ht.NATSEC2)
summary(ht.NATSEC3)
summary(ht.NATSEC4)
summary(ht.NATSEC5)

summary(ht.AGRPOLICY)
summary(ht.AGRPOLICY1)
summary(ht.AGRPOLICY2)
summary(ht.AGRPOLICY3)
summary(ht.AGRPOLICY4)
summary(ht.AGRPOLICY5)

summary(ht.EMNLABORREG)
summary(ht.EMNLABORREG1)
summary(ht.EMNLABORREG2)
summary(ht.EMNLABORREG3)
summary(ht.EMNLABORREG4)
summary(ht.EMNLABORREG5)

summary(ht.IMMIGRATION)
summary(ht.IMMIGRATION1)
summary(ht.IMMIGRATION2)
summary(ht.IMMIGRATION3)
summary(ht.IMMIGRATION4)
summary(ht.IMMIGRATION5)

summary(ht.LaborForceWomen)
summary(ht.LaborForceWomen1)
summary(ht.LaborForceWomen2)
summary(ht.LaborForceWomen3)
summary(ht.LaborForceWomen4)
summary(ht.LaborForceWomen5)

summary(ht.LaborForceBlack)
summary(ht.LaborForceBlack1)
summary(ht.LaborForceBlack2)
summary(ht.LaborForceBlack3)
summary(ht.LaborForceBlack4)
summary(ht.LaborForceBlack5)

summary(ht.LaborForceLatino)
summary(ht.LaborForceLatino1)
summary(ht.LaborForceLatino2)
summary(ht.LaborForceLatino3)
summary(ht.LaborForceLatino4)
summary(ht.LaborForceLatino5)

summary(ht.EmpPopRatioMen)
summary(ht.EmpPopRatioMen1)
summary(ht.EmpPopRatioMen2)
summary(ht.EmpPopRatioMen3)
summary(ht.EmpPopRatioMen4)
summary(ht.EmpPopRatioMen5)

summary(ht.EmpPopRatioWomen)
summary(ht.EmpPopRatioWomen1)
summary(ht.EmpPopRatioWomen2)
summary(ht.EmpPopRatioWomen3)
summary(ht.EmpPopRatioWomen4)
summary(ht.EmpPopRatioWomen5)








####Significant variables
####Significant variables
####Significant variables
####Significant variables
####Significant variables
summary(ht.IMMIGRATION)
summary(ht.gdp)
summary(ht.unemp5)
summary(ht.EMNLABORREG2)
summary(ht.AGRPOLICY2)


summary(ht.IMMIGRATION)$QIC;summary(ht.IMMIGRATION1)$QIC;summary(ht.IMMIGRATION2)$QIC;summary(ht.IMMIGRATION3)$QIC;summary(ht.IMMIGRATION4)$QIC;summary(ht.IMMIGRATION5)$QIC;
summary(ht.gdp)$QIC;summary(ht.gdp1)$QIC;summary(ht.gdp2)$QIC;summary(ht.gdp3)$QIC;summary(ht.gdp4)$QIC;summary(ht.gdp5)$QIC
summary(ht.unemp)$QIC;summary(ht.unemp1)$QIC;summary(ht.unemp2)$QIC;summary(ht.unemp3)$QIC;summary(ht.unemp4)$QIC;summary(ht.unemp5)$QIC
summary(ht.EMNLABORREG)$QIC;summary(ht.EMNLABORREG1)$QIC;summary(ht.EMNLABORREG2)$QIC;summary(ht.EMNLABORREG3)$QIC;summary(ht.EMNLABORREG4)$QIC;summary(ht.EMNLABORREG5)$QIC 
summary(ht.AGRPOLICY)$QIC;summary(ht.AGRPOLICY1)$QIC;summary(ht.AGRPOLICY2)$QIC;summary(ht.AGRPOLICY3)$QIC;summary(ht.AGRPOLICY4)$QIC;summary(ht.AGRPOLICY5)$QIC





###Fitted lines
###Fitted lines
###Fitted lines
ggplot.ht.null <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.log.nb$fitted.values),size=1,colour="black",linetype=2)+geom_point()
ggplot.ht.im <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.IMMIGRATION$fitted.values),size=1,colour="black",linetype=2)+geom_point()
ggplot.ht.gdp <- ggplot(data = data.frame(month = anydate(ht.count$month[-139]),count=ht.count$count[-139]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month[-139]),count=ht.gdp$fitted.values[-139]),size=1,colour="black",linetype=2)+geom_point()
ggplot.ht.unemp5 <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.unemp5$fitted.values),size=1,colour="black",linetype=2)+geom_point()
ggplot.ht.lab2 <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.EMNLABORREG2$fitted.values),size=1,colour="black",linetype=2)+geom_point()
ggplot.ht.agr2 <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.count$count), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="Month",y="Convictions")+
  geom_line(data = data.frame(month = anydate(ht.count$month),count=ht.AGRPOLICY2$fitted.values),size=1,colour="black",linetype=2)+geom_point()

pdf("htcount_mean_process.pdf",height = 17,width=17)
plot_grid(ggplot.ht.im,ggplot.ht.gdp,ggplot.ht.unemp5,ggplot.ht.lab2,ggplot.ht.agr2, labels = c("(a)","(b)","(c)","(d)","(e)","(f)"),nrow=5)
dev.off()





# For each of significant pull factors, comparison between NB vs Poisson)
ht.IMMIGRATION.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVIMMIGRATION[c(15:153)],link="log")
ht.EMNLABORREG2.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMNLABORREG[c(15:153)-2],link="log")
ht.AGRPOLICY.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)],link="log")
ht.AGRPOLICY2.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=ht.covar$EMVAGRPOLICY[c(15:153)-2],link="log")
ht.gdp.Poisson = tsglm(ht.count$count[-139],model = list(past_mean=1,past_obs=1),xreg=diff(ht.covar$gdp[c(14:152)]),link="log")
ht.unemp5.Poisson = tsglm(ht.count$count,model = list(past_mean=1,past_obs=1),xreg=diff(log(ht.covar$unemp[c(14:153)-5])),link="log")




pdf("htcount_pit_reg.pdf",height = 8,width=17)
par(mfrow=c(2,5))
pit(ht.IMMIGRATION.Poisson,ylim=c(0,2),main="Immigration Policy (Poisson)",cex.main=2)
pit(ht.gdp.Poisson,ylim=c(0,2),main="GDP (Poisson)",cex.main=2)
pit(ht.unemp5.Poisson,ylim=c(0,2),main="Unemp. rate (Poisson)",cex.main=2)
pit(ht.EMNLABORREG2.Poisson,ylim=c(0,2),main="Labor regulation (Poisson)",cex.main=2)
pit(ht.AGRPOLICY2.Poisson,ylim=c(0,2),main="Agr. policy (Poisson)",cex.main=2)

pit(ht.IMMIGRATION,ylim=c(0,2),main="Immigration Policy (NB)",cex.main=2)
pit(ht.gdp,ylim=c(0,2),main="GDP (NB)",cex.main=2)
pit(ht.unemp5,ylim=c(0,2),main="Unemp. rate (NB)",cex.main=2)
pit(ht.EMNLABORREG2,ylim=c(0,2),main="Labor regulation (NB)",cex.main=2)
pit(ht.AGRPOLICY2,ylim=c(0,2),main="Agr. policy (NB)",cex.main=2)
dev.off()






##Time series plot of significant pull factors.
ggplot.agr2 <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.covar$EMVAGRPOLICY[c(15:153)-2]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")
  
ggplot.lab2 <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.covar$EMNLABORREG[c(15:153)-2]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")

ggplot.im <- ggplot(data = data.frame(month = anydate(ht.count$month),count=ht.covar$EMVIMMIGRATION[c(15:153)]), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")

ggplot.gdp <- ggplot(data = data.frame(month = anydate(ht.count$month[-139]),count=diff(ht.covar$gdp[c(14:152)])), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")

ggplot.unemp5 <- ggplot(data = data.frame(month = anydate(ht.count$month),count=diff(log(ht.count$count[c(14:153)]))), aes(x = month, y = count)) + 
  geom_point()+  geom_line(size=1) + labs(x="",y="")



pdf("htcount_pullfactors.pdf",width=17,height=17)
plot_grid(ggplot.im,ggplot.gdp,ggplot.unemp5,ggplot.lab2,ggplot.agr2,nrow=5, labels ="")#= c("(a)","(b)","(c)","(d)","(e)"))
dev.off()


