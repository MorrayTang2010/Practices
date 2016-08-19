#practice
dat1<-c(7.3, 6.8, 0.005, 9, 12, 2.4, 18.9, 0.9)
# (a) 计算数列的均值；
# (b) 将均值从数列中减去；
# (c) 找出数列中本身大于其平方根的所有元素；
# (d) 将每个元素的平方根近似到小数点后两位 (使用函数 round ).
m1<-mean(dat1)
m2<-quantile(dat1)
m3<-median(dat1)
dat1[-which(dat1==mean(dat1))] #?
m_sd<-sd(dat1)
m4<-dat1[which(dat1>=m_sd)]
m5<-round(sqrt(dat1),2)

#2.	定义 area = state.x77[,"Area"]. 请将 area 以逆序的形式显示出来
area=state.x77[,"Area"]
rev(area)
#3.计算 2 的前 50 次方和 1到50的平方。这两个序列有那几个元素相等？
out3_1<-2^(seq(1,50,1))
out3_2<-(seq(1,50,1))^2
out3_1[out3_1 == out3_2]
# 4.	读入数据 country.frame 。这是欧洲4个国家的数据。其中 GDP 代表国内生产总值, Pop 代表人口,  Inflation 为 2000年的消费者物价增长指数，area 为面积, EU 代表该国家是否是为欧盟成员国。请完成下列任务：
# (a)	计算人均国内生产总值并将其加入到 country.frame 中;
# (b)	那个国家拥有最高的人均国内生产总值？
# (c)	以变量人均 GDP 将 country.frame 排序。 
#无数据

#5.	请问 1:7 * 1:2 的结果是什么？你能解释吗？
# warning message length is not equal


#6.	请将一个数值型向量在3的倍数的位置上的元素变为这个元素的负值。
dat6<-c(1,3,5,6,9,12,20)
flag_3<-as.logical(dat6 %% 3)
replace(dat6,which(!dat6 %% 3),dat6[!flag_3]*(-1))
#7.	写出一个函数计算数值型向量的方差，方差的公式如下：
CalVar<-function(x){
  return(sum((x-mean(x))^2)/(length(x)-1))
}
#8.	使用 curve 函数画出 sin 函数在 [-pi,pi] 的图像，再将cos 函数在此区间的图像加上。
curve(sin,-pi,pi)


#9.此题利用数据 state.x77.
#(a)	用 plot 函数对变量 Illiteracy 和Murder 画散点图；
#(b)	在上面的散点图上，对 Income 位于前25%的州标上字符”high”
