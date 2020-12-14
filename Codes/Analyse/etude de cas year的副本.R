dfy = read.csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/R/year.csv',head =TRUE)
rownames(dfy)<-dfy[,1]
dfy = dfy[,c(2,3)]
View(dfy)
#train data - test data
trainsetY= dfy[c(1:600),]
testsetY= dfy[c(601:775),]
View(trainsetY)
View(testsetY)
library(urca)
library(tseries)
#France
yTrain = trainsetY[1]
yTest = testsetY[1]
# 1)  ACF, PACF
par(mfrow=c(1,1))
yTrain <- ts(yTrain, start=1, frequency = 1)
par(mfrow=c(3,1))
plot(yTrain, main='Série.France')
acf(yTrain, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(yTrain, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# RU  > 0.05 => RU exist
#2)如果有单位根，则不稳定.非平稳序列一定不是白噪声序列
adf.test(yTrain)
#3)different
disFYR = diff(yTrain)
par(mfrow=c(3,1))
plot.ts(disFYR,main = 'Dis_France')
acf(disFYR, main = "Dis_ACF")
pacf(disFYR, main = "Dis_PACF")
#4) 检验差分后是否平稳，si，下一步
adf.test(disFYR)
# 5）检验差分后是否为白噪 p <0.05 => non bruit blanc，non，下一步
Box.test(disFYR,type = 'Ljung-Box') # hyposes null : bruit blanc
# 6) test l'effet ARCH
library(zoo)
library(FinTS)
library(forecast)
# i)建立均值方程如有必要，对收益率序列建立一个计量经济
#    模型（如ARMA）来消除任何线形依赖
auto.arima(disFYR,trace = T)
fityTrain = arima(disFYR, order =c(0,0,0),method='ML')
# ii) 对均值方程的残差进行ARCH效应检验
ryTrain = fityTrain$residuals
ArchTest(ryTrain,lag=10) 
##
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch) 
# 7) description of data (rate of changing)
plot(density(disFYR),main= "gauch skew_leptokurtic_grosse queue")
# 1. moyen
uFY = sum(disFYR)/length(disFYR)
uFY
# 2. st
eFY = sqrt(sum((disFYR - uFY)^2)/(length(disFYR)-1))
eFY
# 3. Asymétrie
sFY = sum((disFYR-uFY)^3)/((length(disFYR)-1)*e^3)
sFY
# 4. Kurtosis
kFY<-sum((disFYR-uFY)^4)/((length(disFYR)-1)*e^4)
kFY
# 5. Test de normalité JB, p-value < 2.2e-16,拒绝原假设，so不服从正态分布
jarque.bera.test(disFYR)

# 8) choix des modèles 
#  i) meilleur-ARCH(10, 0), comparant des ARCH(4,0), ARCH(9,0)
# YFA<-garchFit(formula= ~ garch(4,0),data=disFYR,trace=F) 
# YFA<-garchFit(formula= ~ garch(9,0),data=disFYR,trace=F) 
YFA<-garchFit(formula= ~ garch(9,0),data=disFYR,trace=F) 
summary(YFA)
# ii) residus
resi = residuals(YFA, standardize = T)
res = ts(resi, frequency = 1, start = 1)
par(mfrow=c(3,1))
plot.ts(res,main = 'Residus_France_ARCH')
acf(res, main = "Residus_ACF_ARCH")
pacf(res, main = "Residus_PACF_ARCH")
#iii) residus ^2
par(mfrow=c(3,1))
plot.ts(res^2,main = 'Carré_du_Residus_France_ARCH')
acf(res^2, main = "Carré_du_Residus_ACF_ARCH")
pacf(res^2, main = "Carré_du_Residus_PACF_ARCH")
## iv) Ljung-Box 自相关检验的原假设是序列中不存在自相关。对标准化残差
# 平方用进行 Ljung-Box 方法进行检验在 0.05 的显著水平下，滞后项为
# 为 10,15,20 时分别得到的 P 值分别为：0.9102，0.8663，0.576,不能拒
# 绝原假设，可接受原假设,标准化残差平方不存在序列相关性。
Box.test(resi^2, lag =10, type = "Ljung")
Box.test(resi^2, lag =15, type = "Ljung")
Box.test(resi^2, lag =20, type = "Ljung")
#v) meilleur-GARCH(2, 1), comparant des GARCH(1,1),GARCH(1,2),GARCH(2,2)
# YFGA<-garchFit(formula= ~ garch(1,1),data=disFYR,trace=F) 
# YFGA<-garchFit(formula= ~ garch(1,2),data=disFYR,trace=F) 
# YFGA<-garchFit(formula= ~ garch(2,2),data=disFYR,trace=F) 
YFGA<-garchFit(formula= ~ garch(2,1),data=disFYR,trace=F) 
summary(YFGA)

#### 9) Qualité GARCH(2,1)
# i) residus
resiGA = residuals(YFGA, standardize = T)
resGA = ts(resiGA, frequency = 1, start = 1)
par(mfrow=c(3,1))
plot.ts(resGA,main = 'Residus_France_GARCH')
acf(resGA, main = "Residus_ACF_GARCH")
pacf(resGA, main = "Residus_PACF_GARCH")
# ii) residus  carré
par(mfrow=c(3,1))
plot.ts(resGA^2,main = 'Carré_du_Residus_France_GARCH')
acf(resGA^2, main = "Carré_du_Residus_ACF_GARCH")
pacf(resGA^2, main = "Carré_du_Residus_PACF_GARCH")
### 10) prevision
eFTrain = disFYR-mean(disFYR)
eFTrain2=eFTrain**2
eFTrain2

predvarFTrain = rep(NA, 600)
predvarFTrain[1] = var(disFYR)
predvarFTrain[1] 
predvarFTrain[2] =0.7974628

for (t in 3:600){
  predvarFTrain[t] = 0*eFTrain2[t-1] + 1.720e-01*eFTrain2[t-2]+ 8.403e-01*predvarFTrain[t-1]
}
predvarFTrain = sqrt(predvarFTrain)
predvarFTrainTS<- ts(predvarFTrain, frequency = 1, start = 0)

plot(abs(disFYR), xlab = '1/2/2007 - 5/21/2009 (600 journée) ', ylab = ' ', main= 'GARCH(2,1)')
lines(predvarFTrainTS,col = 'red')
lines(log(yTrain), col = 'blue')
text(100,4.38, "blue: Prix de l'électricité(log)",col ='blue')
text(75,4.21, "Red: Volatilité prévue",col ='red')
text(180,4.55, "Black: Taux de changement de prix de l’électricité",col ='black')

#### l'échantillon restant 601 - 775
yTest = testsetY[1]
FYtest <- ts(yTest, start=1, frequency = 1)

# Equation moyenne
FYtestDis = diff(FYtest)
EFYtestDis = FYtestDis-mean(FYtestDis)
EFYtestDis2=EFYtestDis**2


predvarFYTest = rep(1, 175)
predvarFYTest[1] = var(FYtestDis)
predvarFYTest[1]
predvarFYTest[2] = 0.4295568
#8.403e-01*0.5111946 = 0.4295568
for (t in 3:175){
  predvarFYTest[t] = 0*EFYtestDis2[t-1] + 1.720e-01*EFYtestDis2[t-2]+ 8.403e-01*predvarFYTest[t-1]
}
predvarFYTest = sqrt(predvarFYTest)
predvarFYTestTS<- ts(predvarFYTest, frequency = 1, start = 0)

plot(abs(FYtestDis), xlab = '5/21/2009 - 1/28/2010 (175 journée) ', ylab = ' ', main= 'GARCH(2,1)')
lines(predvarFYTestTS,col = 'red')
lines(log(FYtest), col = 'blue')
text(29,5.3, "blue: Prix de l'électricité(log)",col ='blue')
text(22,5.6, "Red: Volatilité prévue",col ='red')
text(52,5, "Black: Taux de changement de prix de l’électricité",col ='black')
## OK 
##Germany

yGerTrain = trainsetY[2]
yGerTest = testsetY[2]
# 1)  ACF, PACF
par(mfrow=c(1,1))
yGerTrain <- ts(yGerTrain, start=1, frequency = 1)
par(mfrow=c(3,1))
plot(yGerTrain, main='Série.France')
acf(yGerTrain, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(yGerTrain, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# RU  > 0.05 => RU exist
#2)如果有单位根，则不稳定.非平稳序列一定不是白噪声序列
adf.test(yGerTrain)
#3)different
disGeYR = diff(yGerTrain)
par(mfrow=c(3,1))
plot.ts(disGeYR,main = 'Dis_Germany')
acf(disGeYR, main = "Dis_ACF")
pacf(disGeYR, main = "Dis_PACF")
#4) 检验差分后是否平稳，si，下一步
adf.test(disGeYR)
# 5）检验差分后是否为白噪 p <0.05 => non bruit blanc，non，下一步
Box.test(disGeYR,type = 'Ljung-Box') # hyposes null : bruit blanc
# 6) test l'effet ARCH
library(zoo)
library(FinTS)
library(forecast)
# i)建立均值方程如有必要，对收益率序列建立一个计量经济
#    模型（如ARMA）来消除任何线形依赖
auto.arima(disGeYR,trace = T)
fitGeyTrain = arima(disGeYR, order =c(0,0,0),method='ML')
# ii) 对均值方程的残差进行ARCH效应检验
ryGeTrain = fitGeyTrain$residuals
ArchTest(ryGeTrain,lag=10) 
##
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch) 
# 7) description of data (rate of changing)
plot(density(disGeYR),main= "gauch skew_leptokurtic_grosse queue")
# 1. moyen
uGY = sum(disGeYR)/length(disGeYR)
uGY
# 2. st
eGY = sqrt(sum((disGeYR - uGY)^2)/(length(disGeYR)-1))
eGY
# 3. Asymétrie
sGY = sum((disGeYR-uGY)^3)/((length(disGeYR)-1)*e^3)
sGY
# 4. Kurtosis
kGY<-sum((disGeYR-uGY)^4)/((length(disGeYR)-1)*e^4)
kGY
# 5. Test de normalité JB, p-value < 2.2e-16,拒绝原假设，so不服从正态分布
jarque.bera.test(disGeYR)
# 8) choix des modèles 
#  i) meilleur-ARCH(10, 0), comparant des ARCH(4,0), ARCH(9,0)
# YFA<-garchFit(formula= ~ garch(4,0),data=disGeYR,trace=F) 
# YFA<-garchFit(formula= ~ garch(9,0),data=disGeYR,trace=F) 
YGA<-garchFit(formula= ~ garch(9,0),data=disGeYR,trace=F) 
summary(YGA)
# ii) residus
resiGG = residuals(YGA, standardize = T)
resGG = ts(resiGG, frequency = 1, start = 1)
par(mfrow=c(3,1))
plot.ts(resGG,main = 'Residus_Germany_ARCH')
acf(resGG, main = "Residus_ACF_ARCH")
pacf(resGG, main = "Residus_PACF_ARCH")
# iii) residus^2
par(mfrow=c(3,1))
plot.ts(resGG^2,main = 'Carré_du_Residus_Germany_ARCH')
acf(resGG^2, main = "Carré_du_Residus_ACF_ARCH")
pacf(resGG^2, main = "Carré_du_Residus_PACF_ARCH")

#v) meilleur-GARCH(1, 1), comparant des GARCH(1,1),GARCH(1,2),GARCH(2,2)
# YFGA<-garchFit(formula= ~ garch(2,1),data=disGeYR,trace=F) 
# YFGA<-garchFit(formula= ~ garch(1,2),data=disGeYR,trace=F) 
# YFGA<-garchFit(formula= ~ garch(2,2),data=disGeYR,trace=F) 
YGGA<-garchFit(formula= ~ garch(1,1),data=disGeYR,trace=F) 
summary(YGGA)

#### 9) Qualité GARCH(1,1)
# i) residus
resiGAG = residuals(YGGA, standardize = T)
resGGAG = ts(resiGAG, frequency = 1, start = 1)
par(mfrow=c(3,1))
plot.ts(resGGAG,main = 'Residus_Germany_GARCH')
acf(resGGAG, main = "Residus_ACF_GARCH")
pacf(resGGAG, main = "Residus_PACF_GARCH")
# ii) residus  carré
par(mfrow=c(3,1))
plot.ts(resGGAG^2,main = 'Carré_du_Residus_Germany_GARCH')
acf(resGGAG^2, main = "Carré_du_Residus_ACF_GARCH")
pacf(resGGAG^2, main = "Carré_du_Residus_PACF_GARCH")
### 10) prevision
eGGTrain = disGeYR-mean(disGeYR)
eGGTrain2=eGGTrain**2
eGGTrain2

predvarGGTrain = rep(NA, 600)
predvarGGTrain[1] = var(disGeYR)
predvarGGTrain[1] 
predvarGGTrain[2] =0.122442*eGGTrain2[1] + 0.877236*predvarGGTrain[1]
predvarGGTrain[2]
for (t in 2:600){
  predvarGGTrain[t] = 0.122442*eGGTrain2[t-1] + 0.877236*predvarGGTrain[t-1]
}
predvarGGTrainS = sqrt(predvarGGTrain)
predvarGGTrainTS<- ts(predvarGGTrainS, frequency = 1, start = 0)
predvarGGTrainTS

plot(abs(disGeYR), xlab = '1/2/2007 - 5/21/2009 (600 journée) ', ylab = ' ', main= 'GARCH(1,1)')
lines(predvarGGTrainTS,col = 'red')
lines(log(yGerTrain), col = 'blue')
text(100,4.38, "blue: Prix de l'électricité(log)",col ='blue')
text(75,4.21, "Red: Volatilité prévue",col ='red')
text(180,4.55, "Black: Taux de changement de prix de l’électricité",col ='black')

#### l'échantillon restant 601 - 775
yGerTest = testsetY[2]
GYtestG <- ts(yGerTest, start=1, frequency = 1)

# Equation moyenne
GYtestDisG = diff(GYtestG)
EGYtestDisG = GYtestDisG-mean(GYtestDisG)
EGYtestDisG2=GYtestDisG**2

predvarGYTest = rep(1, 175)
predvarGYTest[1] = var(GYtestDisG)
predvarGYTest[1]
predvarGYTest[2] = 0.122442*EGYtestDisG2[1] +   0.877236*predvarGYTest[1]
predvarGYTest[2]

for (t in 2:175){
  predvarGYTest[t] = 0.122442*EGYtestDisG2[t-1] +   0.877236*predvarGYTest[t-1]
}
predvarGYTest = sqrt(predvarGYTest)
predvarFYTestTS<- ts(predvarGYTest, frequency = 1, start = 1)

plot(abs(GYtestDisG), xlab = '5/21/2009 - 1/28/2010 (175 journée) ', ylab = ' ', main= 'GARCH(1,1)')
lines(predvarFYTestTS,col = 'red')
lines(log(GYtestG), col = 'blue')
text(29,8.5, "blue: Prix de l'électricité(log)",col ='blue')
text(22,8.9, "Red: Volatilité prévue",col ='red')
text(52,8.1, "Black: Taux de changement de prix de l’électricité",col ='black')

##OKK


