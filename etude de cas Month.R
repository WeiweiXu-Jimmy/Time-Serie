dM = read.csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/R/month.csv',head =TRUE)
rownames(dM)<-dM[,1]
dM = dM[,c(2,3)]
#train data - test data
trainsetM= dM[c(1:600),]
testsetM= dM[c(601:775),]
View(trainsetM)
View(testsetM)
library(urca)
library(tseries)
#France
yMtrain = trainsetM[1]
FMtrain <- ts(yMtrain, start=1, frequency = 1)

# 1)  ACF, PACF
par(mfrow=c(3,1))
plot(FMtrain, main='Série.France')
acf(FMtrain, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(FMtrain, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# RU  > 0.05 => RU exist
#2)如果有单位根，则不稳定.非平稳序列一定不是白噪声序列
adf.test(FMtrain)
#3)different
disFMR = diff(FMtrain)
par(mfrow=c(3,1))
plot.ts(disFMR,main = 'Dis_France_Month')
acf(disFMR, main = "Dis_ACF_Month")
pacf(disFMR, main = "Dis_PACF_Month")
#4) 检验差分后是否平稳，si，下一步
adf.test(disFMR)
# 5）检验差分后是否为白噪 p <0.05 => non bruit blanc，non，下一步
Box.test(disFMR,type = 'Ljung-Box') # hyposes null : bruit blanc
# 6) test l'effet ARCH
library(zoo)
library(FinTS)
# i)建立均值方程如有必要，对收益率序列建立一个计量经济
#    模型（如ARMA）来消除任何线形依赖
auto.arima(disFMR,trace = T)
fitFMTrain = arima(disFMR, order =c(0,0,0),method='ML')
# ii) 对均值方程的残差进行ARCH效应检验
rFMTrain = fitFMTrain$residuals
ArchTest(rFMTrain,lag=20)
## p-value = 0.9934，没有arch 效应 sur l'echantillon train

#### l'échantillon restant 601 - 775
FMtest = testsetM[1]
FMTest <- ts(FMtest, start=1, frequency = 1)

# 1)  ACF, PACF
par(mfrow=c(3,1))
plot(FMTest, main='Série.France')
acf(FMTest, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(FMTest, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# RU  > 0.05 => RU exist
#2)如果有单位根，则不稳定.非平稳序列一定不是白噪声序列
adf.test(FMTest)
#3)different
disFMTe = diff(FMTest)
par(mfrow=c(3,1))
plot.ts(disFMTe,main = 'Dis_France_Month')
acf(disFMTe, main = "Dis_ACF_Month")
pacf(disFMTe, main = "Dis_PACF_Month")
#4) 检验差分后是否平稳，si，下一步
adf.test(disFMTe)
# 5）检验差分后是否为白噪 p <0.05 => non bruit blanc，non，下一步
Box.test(disFMTe,type = 'Ljung-Box') # hyposes null : bruit blanc
# 6) test l'effet ARCH
library(zoo)
library(FinTS)
library(forecast)
# i)建立均值方程如有必要，对收益率序列建立一个计量经济
#    模型（如ARMA）来消除任何线形依赖
auto.arima(disFMTe,trace = T)
fitFMTest = arima(disFMTe, order =c(2,0,2),method='ML')
# ii) 对均值方程的残差进行ARCH效应检验
rFMTest = fitFMTest$residuals
ArchTest(rFMTest,lag=25)
# p-value = 0.02996 <0.05,l'effet ARCH exist

## ARCH模型认为模型的序列值波动与其过去p期的波动有关
#   Xt = Wtεt
#   W2t = α0 + α1X2 t-1+…+  αpX2 t-p
#  εt ~ N(0, 1) bruit blanc gaussien
## GARCH模型认为时间序列每个时间点的波动率是最近p个时间点残差平方的线性组合，
#   与最近q个时间点变量波动率的线性组合加起来得到
#    Xt = Wtεt
#   W2t = α0 + αiX2 t-i +  βjW2 t - j

# 7) description of data (rate of changing)
plot(density(disFMTe),main= "droite skew_leptokurtic_grosse queue")
# 1. moyen
uFMTe = sum(disFMTe)/length(disFMTe)
uFMTe
# 2. st
e = sqrt(sum((disFMTe - uFMTe)^2)/(length(disFMTe)-1))
e
# 3. Asymétrie
s = sum((disFMTe-uFMTe)^3)/((length(disFMTe)-1)*e^3)
s
# 4. Kurtosis
k<-sum((disFMTe-uFMTe)^4)/((length(disFMTe)-1)*e^4)
k
# 5. Test de normalité JB, p-value < 2.2e-16,拒绝原假设，so不服从正态分布
jarque.bera.test(disFMTe)
# 8) choix des modèles 

#  estimation du modele GARCH
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch) 
## 9) ARCH
FMAR<-garchFit(formula= ~ garch(1,0),data=disFMTe,trace=F) 
summary(FMAR)
## GARCH(1,1)
mG<-garchFit(formula= ~ garch(1,1),data=disFMTe,trace=F) 
summary(mG)
#10) Qualité du modele ARCH(1,0)

# i) residus
resiFMAR = residuals(FMAR, standardize = T)
resiFMAR = ts(resiFMAR, frequency = 1, start = 1)
par(mfrow=c(3,1))
plot.ts(resiFMAR,main = 'Residus_France_ARCH')
acf(resiFMAR, main = "Residus_ACF_ARCH")
pacf(resiFMAR, main = "Residus_PACF_ARCH")
# ii) residus  carré
par(mfrow=c(3,1))
plot.ts(resiFMAR^2,main = 'Carré_du_Residus_France_ARCH')
acf(resiFMAR^2, main = "Carré_du_Residus_ACF_ARCH")
pacf(resiFMAR^2, main = "Carré_du_Residus_PACF_ARCH")
## 11 ) prevision
e = disFMTe-mean(disFMTe)
e2=e**2
predvar = rep(NA, 175)
 predvar[1] = var(disFMTe)
 for (t in 2:175){
   predvar[t] = 3.4071 + 0.4150*e2[t-1]
 }
predvol = sqrt(predvar)
predvol<- ts(predvol)

plot(abs(disFMTe), xlab = '5/21/2009 - 1/28/2010 (175 journée) ', ylab = ' ', main= 'ARCH(1,0)')
lines(predvol,col = 'red')
lines(log(FMtrain), col = 'blue')
text(40,8, "blue: Prix de l'électricité(log)",col ='blue')
text(32,8.5, "Red: Volatilité prévue",col ='red')
text(65,9.2, "Black: Taux de changement de prix de l’électricité",col ='black')

##   OK 

##################### Germany
#  1-600 echantillon train
GermanyMtrain = trainsetM[2]
GMtrain <- ts(GermanyMtrain, start=1, frequency = 1)
##1. ACF, PACF
par(mfrow=c(3,1))
plot(GMtrain, main='Série.Germany')
acf(GMtrain, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(GMtrain, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# 2. RU  > 0.05 => RU exist
adf.test(GMtrain)
#3)different
GMtrainDis = diff(GMtrain)
par(mfrow=c(3,1))
plot.ts(GMtrainDis,main = 'Differenced_Germany')
acf(GMtrainDis,main= 'Differenced_ACF')
pacf(GMtrainDis,main= 'Differenced_PACF')
#4) RU
adf.test(GMtrainDis)
# 5) test Box-Lj  
Box.test(GMtrainDis,type = 'Ljung-Box')
# 6) test l'effet ARCH
library(zoo)
library(FinTS)
# i)建立均值方程如有必要，对收益率序列建立一个计量经济
#    模型（如ARMA）来消除任何线形依赖
auto.arima(GMtrainDis,trace = T)

fitGMTrain = arima(GMtrainDis, order =c(0,0,0),method='ML')
# ii) 对均值方程的残差进行ARCH效应检验
rGMTrain = fitGMTrain$residuals
ArchTest(rGMTrain,lag=21) 
# df = entre 21 et 54, L'effet ARCH exist, sinon, pas du tout
## p-value = 0.01423，arch 效应 sur l'echantillon train
# 7) description of data (rate of changing)
plot(density(GMtrainDis),main= "droite skew_leptokurtic_grosse queue")
# 1. moyen
GMTe = sum(GMtrainDis)/length(GMtrainDis)
GMTe
# 2. st
eG = sqrt(sum((GMtrainDis - GMTe)^2)/(length(GMtrainDis)-1))
eG
# 3. Asymétrie
sG = sum((GMtrainDis-GMTe)^3)/((length(GMtrainDis)-1)*e^3)
sG
# 4. Kurtosis
kG<-sum((GMtrainDis-GMTe)^4)/((length(GMtrainDis)-1)*e^4)
kG
# 5. Test de normalité JB, p-value < 2.2e-16,拒绝原假设，so不服从正态分布
jarque.bera.test(GMtrainDis)
# 8) choix des modèles 

#  estimation du modele GARCH
## 9) ARCH
GMAR<-garchFit(formula= ~ garch(10,0),data=GMtrainDis,trace=F) 
summary(GMAR)
## GARCH(2,2)
GermanyG<-garchFit(formula= ~ garch(2,2),data=GMtrainDis,trace=F) 
summary(GermanyG)
#10) Qualité du modele GARCH(2,2)

# i) residus
resiGEGA = residuals(GermanyG, standardize = T)
resiGEGA = ts(resiGEGA, frequency = 1, start = 1)
par(mfrow=c(3,1))
plot.ts(resiGEGA,main = 'Residus_Germany_ARCH')
acf(resiGEGA, main = "Residus_ACF_ARCH")
pacf(resiGEGA, main = "Residus_PACF_ARCH")
# ii) residus  carré
par(mfrow=c(3,1))
plot.ts(resiGEGA^2,main = 'Carré_du_Residus_Germany_ARCH')
acf(resiGEGA^2, main = "Carré_du_Residus_ACF_ARCH")
pacf(resiGEGA^2, main = "Carré_du_Residus_PACF_ARCH")
## 11 ) prevision
eGTrain = GMtrainDis-mean(GMtrainDis)
eGTrain2=eGTrain**2

predvarGTrain = rep(NA, 600)
predvarGTrain[1] = var(GMtrainDis)
predvarGTrain[2] =2.777559
for (t in 3:600){
    predvarGTrain[t] = 1.084 +0*eGTrain2[t-1] + 5.237e-01*eGTrain2[t-2]+ 0*predvarGTrain[t-1]+3.909e-01*predvarGTrain[t-2]
}
predvarGTrain = sqrt(predvarGTrain)
predvarGTrainTS<- ts(predvarGTrain)

plot(abs(GMtrainDis), xlab = '1/2/2007 - 5/21/2009 (600 journée) ', ylab = ' ', main= 'GARCH(2,2)')
lines(predvarGTrainTS,col = 'red')
lines(log(GMtrain), col = 'blue')
text(100,14, "blue: Prix de l'électricité(log)",col ='blue')
text(72,14.7, "Red: Volatilité prévue",col ='red')
text(185,15.35, "Black: Taux de changement de prix de l’électricité",col ='black')

#### l'échantillon restant 601 - 775

GermanyMtest = testsetM[2]
GMtest <- ts(GermanyMtest, start=1, frequency = 1)
# Equation moyenne
GMtestDis = diff(GMtest)
EGMtestDis = GMtestDis-mean(GMtestDis)
EGMtestDis2=EGMtestDis**2

predvarGTest[2] = 1.084 + 5.237e-01*EGMtestDis2[1]+ +3.909e-01*predvarGTest[1]
predvarGTest[2]

predvarGTest = rep(1, 175)
predvarGTest[1] = var(GMtestDis)
predvarGTest[2] =1.63357
for (t in 3:175){
    predvarGTest[t] = 1.084 + 0*EGMtestDis2[t-1] + 5.237e-01*EGMtestDis2[t-2]+ 0*predvarGTest[t-1]+3.909e-01*predvarGTest[t-2]
}
predvarGTest = sqrt(predvarGTest)
predvarGTestTS<- ts(predvarGTest)

predvarGTestTS

plot(abs(GMtestDis), xlab = '5/21/2009 - 1/28/2010 (175 journée) ', ylab = ' ', main= 'GARCH(2,2)')
lines(predvarGTestTS,col = 'red')
lines(log(GMtest), col = 'blue')
text(47,5.3, "blue: Prix de l'électricité(log)",col ='blue')
text(35,5.7, "Red: Volatilité prévue",col ='red')
text(60,6.1, "Black: Taux de changement de prix de l’électricité",col ='black')






