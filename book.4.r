#《R语言代码示例》
#Chapter12
#1
x<-rnorm(1000)
breaks<-c(-3,-2,-1,0,1,2,3)
f<-cut(x,breaks)
summary(f)
f2<-cut(x,breaks,labels=c("Bottom","Low","Neg","Pos","High","Top"))
summary(f2)

#2
vec<-c(100,90,80,70,60,50,90,30,20,10)
match(90,vec) #找到第一个返回值
which(vec==90) #找到所有的返回值

#3
sides<-factor(c("Heads","Tails"))
faces<-factor(c("1 pip",paste(2:6,"pips")))
others<-factor(c("1","2"))
b<-expand.grid(sides,faces,others)
attributes(b)

#4
attributes(b)<-NULL

#5
vec2<-c(1,3,5,7,9)
mean(vec2)
numbers<-list(1,3,5,7,9)
mean(numbers)
vec3<-list(col1=list(7,8,9),col2=list(70,80,90),col3=list(700,800,900))
mean(unlist(vec3))
vec4<-cbind(vec3)
do.call(cbind,vec3)
do.call(mean,vec3)

list <- list(matrix(1:25, ncol = 5), matrix(4:28, ncol = 5), matrix(21:45, ncol=5)) 
list.sum<-do.call(sum,list)
