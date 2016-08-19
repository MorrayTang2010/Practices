x<-seq(10,100,10)
plot(x,y=x,yaxt="n",type="b",xlab="",ylab="")
y2<-c(10,40,60,72,81,90,92,95,98,100)
points(x,y2,type="o",pch=22)

lines(x,rep(20,10))
lines(x,rep(30,10))
lines(x,rep(40,10))
lines(x,rep(50,10))
lines(x,rep(60,10))
lines(x,rep(70,10))
lines(x,rep(80,10))
lines(x,rep(90,10))

lines(rep(40,10),seq(10,100,10))

axis(2,seq(20,100,20),labels=c("20%","40%","60%","80%","100%"))
title(main="评分模型的K-S指标",ylab="累计比例",xlab="评分分数")

legend(75,70,c("正类"),merge=TRUE,pch='o',lwd=2)
legend(75,40,c("负类"),merge=TRUE,pch=22,lwd=2)


#画正态分布图
x<-seq(0,100,0.1)
curve(dnorm(x,30,10),xlim=c(0,100),col="blue",lwd=3)
curve(dnorm(x,70,10),xlim=c(0,100),col="red",lwd=3)
