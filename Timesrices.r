#ARIMA: Example1

library(tseries)
library(zoo)
library(forecast)
air <- AirPassengers
ts.plot(air)
acf(air)
pacf(air)
x<-decompose(air)
plot(x)
plot(x$seasonal)
air.fit <- arima(air,order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
tsdiag(air.fit)
air.forecast <- forecast(air.fit,12)
plot.forecast(air.forecast)

##ARIMA: Example2
require(RJSONIO)
require(WDI)
require(ggplot2)
require(scales)
require(useful)
require(zoo)
require(Matrix)
require(forecast)
#pull the data
gdp<-WDI(country=c("US","CA","GB","DE","CN","JP","SG","IL"),
        indicator=c("NY.GDP.PCAP.CD","NY.GDP.MKTP.CD"),
        start=1960,end=2011)
names(gdp)<-c("iso2c","Country","Year","PerCapGDP","GDP")
ggplot(gdp,aes(Year,PerCapGDP,color=Country,linetype=Country))+geom_line()+scale_y_continuous(label=dollar)
ggplot(gdp,aes(Year,GDP,color=Country,lineetype=Country))+geom_line()+scale_y_continuous(label=multiple_format(extra=dollar,multiple="M"))
us<-gdp$PerCapGDP[gdp$Country=="United States"]
us<-ts(us,start=min(gdp$Year),end=max(gdp$Year))
plot(us,ylab="Per Capita GDP",xlab="Year")
acf(us)
pacf(us)
ndiffs(x=us)
plot(diff(us,2))
#arima
usBest<-auto.arima(x=us)
acf(usBest$residuals)
pacf(usBest$residuals)
par(mfrow=c(2,1))
pacf(us)
pacf(usBest$residuals)
#系数
coef(usBest)
#预测
predict(usBest,n.ahead=5,se.fit=TRUE)
theForecast<-forecast(object=usBest,n=5)
plot(theForecast)

##Example3: VAR
require(reshape2)
gdpCast<-dcast(Year ~ Country,data=gdp[,c("Country","Year","PerCapGDP")],
               value.var="PerCapGDP")
gdpTS<-ts(data=gdpCast[,-1],start=min(gdpCast$Year),end=max(gdpCast$Year))
#不同曲线画在同一图上
plot(gdpTS,plot.type="single",col=1:8)
legend("topleft",legend=colnames(gdpTS),ncol=2,lty=1,col=1:8,cex=.6)
#NA procession
gdpTS<-gdpTS[,which(colnames(gdpTS)!="Germany")]
numDiffs<-ndiffs(gdpTS)
gdpDiffed<-diff(gdpTS,differences=numDiffs)
dim(gdpDiffed)
plot(gdpDiffed,plot.type="single",col=1:7)
legend("bottomleft",legend=colnames(gdpDiffed),ncol=2,lty=1,col=1:7,cex=0.6)
#VAR
require(MASS)
require(strucchange)
require(lmtest)
require(sandwich)
require(urca)
require(vars)
require(coefplot)



gdpVar<-VAR(gdpDiffed,lag.max=12)
gdpVar$p
gdpVar$type
names(gdpVar$varresult)
class(gdpVar$varresult$Canada)
plot(gdpVar$varresult$Canada)
head(coef(gdpVar$varresult$Canada))
coefplot(gdpVar$varresult$Canada)
#predict
predict(gdpVar,n.ahead=5)

#Example4: ARIMA 
wape = function(pred,test)
{ 
  len<-length(pred) 
  errSum<-sum(abs(pred[1:len]-test[1:len])) 
  corSum<-sum(test[1:len])
  result<-errSum/corSum
  result
}


mae = function(pred,test)
{ 
  errSum<-mean(abs(pred-test))    #注意  和wape的实现相比是不是简化了很多
  errSum
}

rmse = function(pred,test)
{ 
  res<- sqrt(mean((pred-test)^2) )
  res
}

#
library(tseries)
library(zoo)
library(forecast)
air <- AirPassengers
ts.plot(air)
passenger<-air

p<-unlist(passenger)
pt<-ts(p,frequency=12,start=2001)
plot(pt)
train<-window(pt,start=2001,end=2011+11/12)
test<-window(pt,start=2012)

library(forecast)
pred_meanf<-meanf(train,h=12)
rmse(test,pred_meanf$mean) #226.2657
pred_naive<-naive(train,h=12)
rmse(pred_naive$mean,test)#102.9765
pred_snaive<-snaive(train,h=12)
rmse(pred_snaive$mean,test)#50.70832
pred_rwf<-rwf(train,h=12, drift=T)
rmse(pred_rwf$mean,test)#92.66636
pred_ses <- ses(train,h=12,initial='simple',alpha=0.2)
rmse(pred_ses$mean,test) #89.77035
pred_holt<-holt(train,h=12,damped=F,initial="simple",beta=0.65)
rmse(pred_holt$mean,test)#76.86677  without beta=0.65 it would be 84.41239
pred_hw<-hw(train,h=12,seasonal='multiplicative')
rmse(pred_hw$mean,test)#16.36156
fit<-ets(train)
accuracy(predict(fit,12),test) #24.390252    结果不对...
pred_stlf<-stlf(train)
rmse(pred_stlf$mean,test)#22.07215
plot(stl(train,s.window="periodic"))  #Seasonal Decomposition of Time Series by Loess

fit2<-auto.arima(train)
accuracy(forecast(fit2,h=12),test) #23.538735
ma = arima(train, order = c(0, 1, 3),   seasonal=list(order=c(0,1,3), period=12))
p<-predict(ma,12)
accuracy(p$pred,test)  #18.55567
BT = Box.test(ma$residuals, lag=30, type = "Ljung-Box", fitdf=2)
