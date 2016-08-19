#forecast:
#https://www.otexts.org/fpp/4/8
library(fpp)
library(forecast)

fit.ex3 <- tslm(consumption ~ income, data=usconsumption)

plot(usconsumption, ylab="% change in consumption and income",
     plot.type="single", col=1:2, xlab="Year")
plot(consumption ~ income, data=usconsumption, 
     ylab="% change in consumption", xlab="% change in income")
abline(fit.ex3)
summary(fit.ex3)
coef(fit.ex3)

#predict
fcast<-forecast(fit.ex3,newdata=data.frame(income=c(-1,1)))
plot(fcast,ylab="% change in consumption",xlab="% change in income")

fit.ex4 <- tslm(austa ~ trend)
f <- forecast(fit.ex4, h=5,level=c(80,95))
plot(f, ylab="International tourist arrivals to Australia (millions)",
     xlab="t")

########
par(mfrow=c(2,2))
res3<-ts(resid(fit.ex3),s=1970.25,f=4)
plot.ts(res3,ylab="res(Consumption)") #第1张图
abline(0,0)
Acf(res3)  #第2张图
res4<-resid(fit.ex4)
plot(res4,ylab="res(Tourism)") #第3张图
abline(0,0)
Acf(res4)  #第4张图

###https://www.otexts.org/fpp/9/1
require(fpp)
plot(insurance, main="Insurance advertising and quotations", xlab="Year")
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert)<-paste("AdLag",0:3,sep="")
# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1],xreg=Advert[4:40,2],d=0)
fit3 <- auto.arima(insurance[4:40,1],xreg=Advert[4:40,3],d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)

fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")


##fpp/9/2
require(vars)
VARselect(usconsumption, lag.max=8, type="const")$selection
var <- VAR(usconsumption, p=3, type="const")
serial.test(var, lags.pt=10, type="PT.asymptotic")
summary(var)
fcst <- forecast(var)
plot(fcst, xlab="Year")

##fpp/9/3
require(caret)
creditlog  <- data.frame(score=credit$score,
                         log.savings=log(credit$savings+1),
                         log.income=log(credit$income+1),
                         log.address=log(credit$time.address+1),
                         log.employed=log(credit$time.employed+1),
                         fte=credit$fte, single=credit$single)

fit_nnet<-avNNet(score ~ log.savings+log.income+log.employed+log.address,
                 data=creditlog,repeats = 25,size=3,decay=0.1,linout=TRUE)

actual_nnet<-creditlog$score
pred_nnet<-predict(fit_nnet,creditlog)

act_pred_score <- data.frame(actual_score=actual_nnet,pred_score=pred_nnet)
plot(act_pred_score$actual_score,type='o',col='red')
lines(act_pred_score$pred_score)

fit_net2 <- nnetar(sunspotarea)
plot(forecast(fit_net2,h=20))
####实验楼######
require(forecast)
plot(AirPassengers)
apts<-ts(AirPassengers,frequency = 12)
f <- decompose(apts)
f$figure
plot(f$figure,type='o',xaxt="n",xlab="year")
monthNames <- months(ISOdate(2011,1:12,1))
axis(1, at=1:12, labels=monthNames, las=2)
plot(f)