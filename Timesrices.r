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
