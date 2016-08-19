#baidu wenku
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingsTS<-ts(kings)
plot.ts(kingsTS)
#how to use ts()
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthsTS<-ts(births,frequency = 12,start=c(1946,4))
birthsTS
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
#平滑法
require(TTR)
kingsTS_SMA<-SMA(kingsTS,n=3)
plot(kingsTS_SMA)

birthsTS_DECOMPOSE <- decompose(birthsTS)
plot.ts(birthsTS_DECOMPOSE)
##简单指数平滑法
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot(rainseries)
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
plot(rainseriesforecasts)
rainseriesforecasts$SSE

library(tseries)
library(zoo)
library(forecast)
data("UKNonDurables")
require(tseries)
require(car)
require(lmtest)
require(sandwich)
require(survival)
require(AER)
data("PepperPrice")
plot(PepperPrice, plot.type = "single", col = 1:2)
adf.test(log(PepperPrice[, "white"]))

adf.test(diff(log(PepperPrice[, "white"])))
