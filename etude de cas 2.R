df = read.csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/R/day.csv',head =TRUE)
rownames(df)<-df[,1]
df = df[,c(2,3)]
df
View(df)
#train data - test data
trainset= df[c(1:600),]
testset= df[c(601:775),]
View(trainset)
View(testset)
library(urca)
library(tseries)
#France
y1 = trainset[1]
y2 = testset[1]
#France
par(mfrow=c(1,1))
x1 <- ts(y1, start=1, frequency = 1)
plot(x1, main='Série No. 1')
par(mfrow=c(3,1))
plot(x1, main='Série No. 1')
acf(x1, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(x1, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# RU  > 0.05 => RU exist
#2)如果有单位根，则不稳定.非平稳序列一定不是白噪声序列
adf.test(x1)
#3)different
disF = diff(x1)
par(mfrow=c(3,1))
plot.ts(disF)
acf(disF)
pacf(disF)
#4) 检验差分后是否平稳，si，下一步
adf.test(disF)
# 5）检验差分后是否为白噪 p <0.05 => non bruit blanc，non，下一步
Box.test(disF,type = 'Ljung-Box') # hyposes null : bruit blanc
#6) 选取模型
auto.arima(x1,trace = T)
# 7）检验模型下的残差是否为白噪，如果是，则残差里没有有用信息（服从正态分布），进行下一步
#    ARIMA模型都采用极大似然方法估计,so, methold = ML
fit = arima(x1, order =c(5,1,5),method='ML')
r1 = fit$residuals
Box.test(r1,type = 'Ljung-Box')
# 8)prevision深色区域分别为80%和95%的置信区间
library(forecast)
fo=forecast(fit,175)
x2 <- ts(y2, start=601, frequency = 1)
plot(forecast(fit,175),xlab = "Journée",ylab= "Prix de l’électricit")
lines(x2,col = 'red')
text(625,155, "Red:les valeurs réelles",col ='red')
text(630,145, "Blue:les valeurs prévues",col ='blue')
## OK
pre =fo$mean
pre
# 9) Qualite du model: 
#    1.erreur quadratique moyenne + 2.residuls est bruit blanc
#    3. La valeur prédite se situe dans l'intervalle de confiance à 95%
#erreur quadratique moyenne
mae =mean(abs(pre- y2$France))
mae
#Proportion des erreurs quadratiques moyennes
mape = mean(abs(pre-y2$France)/y2$France)
mape

###################day germany
df = read.csv('/Users/xudawei/Desktop/devoirs en M2/Fwd ETUDE DE CAS/Serie temporelle/Series price élec/R/day.csv',head =TRUE)
rownames(df)<-df[,1]
df = df[,c(2,3)]
df
#train data - test data
trainset= df[c(1:600),]
testset= df[c(601:775),]
library(urca)
library(tseries)
#Germany
yg = trainset[2]
yg2 = testset[2]
yg2
#Germany
par(mfrow=c(1,1))
xg <- ts(yg, start=1, frequency = 1)
plot(xg, main='Série.Germany')
par(mfrow=c(3,1))
plot(xg, main='Série.Germay')
acf(xg, main='Autocorrelation', ylab='', ylim=c(-1,1),
    ci.col="red")
pacf(xg, main='Partial Autocorrelation', ylab='', ylim=c(- 1,1), ci.col="blue")
# RU  > 0.05 => RU exist
#2)如果有单位根，则不稳定.非平稳序列一定不是白噪声序列
adf.test(xg)
#3)different
dg = diff(xg)
par(mfrow=c(3,1))
plot.ts(dg)
acf(dg)
pacf(dg)
#4) 检验差分后是否平稳，si，下一步
adf.test(dg)
# 5）检验差分后是否为白噪 p <0.05 => non bruit blanc，non，下一步
Box.test(dg,type = 'Ljung-Box') # hyposes null : bruit blanc
#6) 选取模型
auto.arima(xg,trace = T)
library(forecast)
# 7）检验模型下的残差是否为白噪，如果是，则残差里没有有用信息（服从正态分布），进行下一步
#    ARIMA模型都采用极大似然方法估计,so, methold = ML
fitg = arima(xg, order =c(5,1,3),method='ML')
fitg
rdg = fitg$residuals
Box.test(rdg,type = 'Ljung-Box')
## 8) comparaison ARMA(4,1,4)
#    ARIMA模型都采用极大似然方法估计,so, methold = ML
fitg414 = arima(xg, order =c(4,1,4),method='ML')
fitg414
rdg414 = fitg414$residuals
Box.test(rdg414,type = 'Ljung-Box')
# 9)prevision深色区域分别为80%和95%的置信区间
fog=forecast(fitg,175)
yg22 <- ts(yg2, start=601, frequency = 1)
plot(forecast(fitg,175),xlab = "Journée",ylab= "Prix de l’électricit")
lines(yg22,col = 'red')
text(585,235, "Red:les valeurs réelles",col ='red')
text(600,220, "Blue:les valeurs prévues",col ='blue')
## OK
preg =fog$mean
preg
# 9) Qualite du model: 
#    1.erreur quadratique moyenne + 2.residuls est bruit blanc
#    3. La valeur prédite se situe dans l'intervalle de confiance à 95%
#erreur quadratique moyenne
maeg =mean(abs(preg- yg2$Germany))
maeg
#Proportion des erreurs quadratiques moyennes
mapeg = mean(abs(preg-yg2$Germany)/yg2$Germany)
mapeg









