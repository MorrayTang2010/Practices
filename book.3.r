#book: Chapter14 Time secries
require(zoo)
ts<-zoo(x,dt)
dates<-seq(from=as.Date("1970-01-01"),to=as.Date("1979-12-31"),by=1)
empty<-zoo(,dates)

prices<-c(132.45,130.85,130.00,129.55,130.85)
dates<-as.Date(c("2010-01-04","2010-01-05","2010-01-06","2010-01-07","2010-01-08"))
ibm.daily<-zoo(prices,dates)
print(ibm.daily)

index(ibm.daily)
plot(ibm.daily)
#滞后
lag(ibm.daily,k=-1,na.pad=TRUE)
#差分
diff(ibm.daily)
#时间序列计算
diff(ibm.daily)/ibm.daily
#ma<-rollmean(ibm.daily,1,align='right')
#在时间范围内运用函数
require(xts)
apply.weekly(ibm.daily,mean)
