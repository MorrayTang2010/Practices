#ARIMA
require(tseries)
dat1<-read.csv('E:/R/studydata/ARIMA.csv',header=TRUE,stringsAsFactors = FALSE)
x<-ts(dat1$x,start=1964)
plot.ts(x)

#平稳性检验
adf.test(x)
pp.test(x)
kpss.test(x)

#ARIMA(p,q)的参数确定
acf(x)     #p=4
pacf(x)    #q=4

auto.arima(x)
auto.arima(x)
#估计模型参数
fit=arima(x,order=c(4,0,0))
#模型检验
tsdiag(fit)
#预测
predict(fit,n.ahead=3)
#AIC选择
require(MASS)
require(plyr)
require(pROC)
require(dplyr)

AIC(fit)
