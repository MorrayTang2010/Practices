# 1.	画出 triangle.pdf 中的图。其中的三角形是由直线 y = 2x - 3, x=8 和 y=1 围成的。
#然后随机产生此三角形内的10个点。将这 10个点和由这 10个点生成的凸形加到已有的图形上。
# 
# 提示：
# 1) 先使用 plot 画出一个空白作图纸； 
# 2) 可以使用  plot, curve, lines 函数画出三角形；
# 3) 使用课件中的指令生成凸形；
# 4) 如何生成三角形内的随机点。先产生 [2,8] 上的均匀分布的 10 个随机数作为 10 个点的横坐标，
#再对每个已生成的 x, 产生一个 [1, 2*x -3 ] 上均匀分布的随机数作为对应的纵坐标。
x0<-seq(1:10)
y0=2*x0-3
y1<-1
x2<-8
y2<-seq(1:10)
#plot(x0,y0,x0,y1,x2,y2,type='l')
plot(x0,y0,type='l')
abline(-3,2)
abline(h=1)
abline(v=8)
#----------------答案-------------------
#法1
x<-runif(10)*6 + 2
y<-runif(10)*(2*x-4) +1 
plot(x,y,xlim=c(1,8),ylim=c(0,13))
X<-cbind(x,y)
h<-chull(X)
h<-c(h,h[1])
lines(X[h,],lwd=3)
lines(c(2,8),c(1,13),lwd=1)
lines(c(2,8),c(1,1),lwd=1)
lines(c(8,8),c(1,13),lwd=1)

#法2
y<-function(x) 2*x - 3
curve(y,from=2,to=8,add=T)
y<-function(x) 1+0*x
curve(y,from=2,to=8,add=T)
x<-rep(8,13)
y<-1:13
lines(x,y)

# 
# 2.	以                cv <- function(x,  na.rm=F) 
#   开头定义一个函数 cv。此函数计算样本 x 的变异系数 CV。CV的定义如下： 
# CV =  sd(x) / mean(x)。
# 对此函数的要求如下：
# 1)	如果自变量 na.rm 取默认值 F, 则当 x 中有 NA 值时给出下列输出：
# Missing values detected with na.rm=F
# CV = NA;    (注意这里是两行输出)
# 如果 x 中无 NA 值，则计算 CV 值，且给出下列输出：
# CV = cv 的函数值
# 2) 如果自变量 na.rm 取值 T, 则当 x 中有 NA 值时将 NA 从样本 x 中去掉，然后计算 CV 值；
#如果 x 无 NA 值时则直接计算 CV 值。最后给出下列输出：
# CV = cv 的函数值。
# 提示：
# 1)	is.na(x) 检验 x 中的值是否为 NA,  !is.na(x) 检验 x 中的值是否不是 NA; 
# 2)	any(is.na(x)) 检验 x 中是否有至少一个 NA值；
# 3)	可以使用 cat 函数给出输出，注意换行符号的应用。
cv<-function(x,na.rm=F)
{
  if(na.rm=F){
    if(any(is.na(x))==TRUE){
      cat(sprintf('Missing values detected with na.rm=F\n'))
      cat(sprintf('CV=NA\n'))
    }else{
      return(sd(x)/mean(x))
    }
  }else{
    if(any(is.na(x))==TRUE){
      x.filt<-x[!is.na(x)]
      return(sd(x.fit)/mean(x.fit))
    }else{
      return(sd(x)/mean(x))
    }
  }
}

cv<-function(x,na.rm=F){
  print('cv')
  
}

test<-function(x,na.rm=F)
{
  print('hello world')
  if(na.rm=F){
      cat(sprintf('Missing values detected with na.rm=F\n'))
      #cat(sprintf('CV=NA\n'))
  }
}
#------------------答案----------------------------------
cv<-function(x, na.rm=F){
  l=length(x)
  a=1:l
  if(na.rm==F){
    if(any(is.na(x))) cat("Missing values detected with na.rm=F\nCV = NA")
    else {CV=sd(x) / mean(x)
    cat("CV=",CV)}
  }
  else{
    if(any(is.na(x))){x<-x[-which(is.na(x))]
    CV=sd(x) / mean(x)
    cat("CV=",CV)}
    else {CV=sd(x) / mean(x)
    cat("CV=",CV)}
  }
}

 
# 
# 3.	对一个二维列联表，Pearson 卡方统计量的形式为
# T  =  (n_{ij} – e_{ij})^2 / e_{ij}
# 其中 e_{ij} = n_{i.} n_{.j} / n . 假设已知 m x k 的矩阵 f = (n_{ij}) , 其中 n_{ij} 为
# 第 (i.j) 个格子的计数。 请用 Vectorizing calculation 的原则，
# 给出Pearson 卡方统计量的表达式。
# 
# 提示：
# 1)	行和向量可以表达为 n_{i.} = f %*%  rep(1, ncol(f)), 列和也有类似的表达式；
# 2)	行和及列和的计算也可以用 apply 函数；
# 3)	设 a 和 b 为两个矩阵。则 a*b 表示两个矩阵对应元素相乘，a/b 表示两个矩阵对应元素相除，
#a %*% b 表示通常意义下的矩阵乘法。
a<-matrix(1:12,nrow=3,ncol=4)
b<-matrix(20:31,nrow=3,ncol=4)
n1=a %*% rep(1,ncol(a))
n2=rep(1,nrow(a)) %*% a

n1_2=apply(a,1,sum)
n2_2=apply(a,2,sum)

n3=colSums(a)
n4=rowSums(a)

##解答：
#法1：
f<-a
n<-sum(f)
#col<- as.matrix(apply(f,1,sum))
#row<- matrix(apply(f,2,sum),nrow=1)
col<- f %*% rep(1,ncol(f))
row<- rep(1,nrow(f)) %*% f
e<-col%*%row/n
sum((f-e)^2/e)
#法2：
sum((f-as.matrix(apply(f,1,sum))%*%matrix(apply(f,2,sum),nrow=1)/sum(f))^2/
      (as.matrix(apply(f,1,sum))%*%matrix(apply(f,2,sum),nrow=1)/sum(f)))
