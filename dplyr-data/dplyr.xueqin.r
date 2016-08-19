

setwd("E:/R/practice/new/New/dplyr-data")
#chapter 1 数据导入
read.table("./read.table/file3.txt",header=TRUE,fill=TRUE)

#chapter 2 tbl对象
order.data<-read.csv("./order.csv",header = T,sep=",")
require(dplyr)
order_tbl<-tbl_df(order.data)

#c3 : filter
df<-data.frame(
  color=c("blue","black","blue","blue","black"),
  value=1:5
)
tbl<-tbl_df(df)

filter(tbl,color=="blue")
filter(df,color=="blue")

filter(tbl,value %in% c(1,4))
filter(df,value %in% c(1,4))

#chapter4 :select
select(df,value)  #直接写需要保留的变量
select(tbl,value)
select(df,-color) #直接去掉不要的变量
select(tbl,-color)
#示例example:
names(order_tbl)
select(order_tbl,orderdate,state)
#更换列名
select(order_tbl,Data=orderdate,Country=state)
select(order_tbl,starts_with('order')) #选取包含order的变量
select(order_tbl,contains("id")) #选取包含id的变量
select(order_tbl,one_of("orderid","city","orderdate")) #选取one_of所构成的vector中的其中

###chapter 5 : arrange
arrange(df,value)
arrange(df,desc(value))

tbl1<-select(order_tbl,data=orderdate,price=totalprice)
arrange(tbl1,data,desc(price))
tail(arrange(tbl1,data,desc(price)))

####chapter 6: mutate
mutate(tbl,double=2*value)
transmute(tbl,double=2*value)

###chapter7: summarise
summarise(tbl,avg=mean(value),total=sum(value))
summarise(tbl1,first=first(data),last=last(data))

####chapter8: group_by
by_color<-group_by(tbl,color)
summarise(by_color,total=sum(value))

####chapter9: colwise and do
library(dplyr)
order=read.csv("order.csv")
order=order %>% select(orderdate,totalprice) %>%
  mutate(year=substr(orderdate,1,4))
order %>%  group_by(year) %>%
  summarise(max(totalprice))

colwise(round)(iris[,1:4]) %>% head