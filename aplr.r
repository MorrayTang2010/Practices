#练习plyr
require(plyr)
require(DT)
a=matrix(1:21,nrow=3,ncol=7)

#1.         aaply与apply的对比
#aaply(.data, .margins, .fun, ..., .progress = "none")
#apply(.data,margins,fun)

datatable(a)
datatable(
  head(iris),
  caption = htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    'Table 2: ', htmltools::em('This is a simple caption for the table.')
  )
)
datatable(a,caption=htmltools::tags$caption(style='caption-side: bottom;text-align:center;',
                                            'Table 1:',htmltools::em('This is a example for the table')))
a_out1=aaply(a,1,mean)
a_out2=apply(a,1,mean)

a_out3=aaply(a,2,mean)
a_out4=apply(a,2,mean)

a_out5=aaply(a,2,function(x) length(x)*x)
a_out6=apply(a,2,function(x) length(x)*x)

a_out7=aaply(a,2,.progress="none",function(x) x*x)
a_out8=apply(a,2,.progress="text",function(x) x*x)

a_out9=aaply(a,1,stats::quantile)
a_out10=apply(a, 1, stats::quantile)

aaply(a,1,mean,.progress="none")
aaply(a,1,mean,.progress="text")
aaply(a,1,mean,.progress="win")

###2.       daply 与dapply的对比 （没有dapply函数）
#d*ply(.data, .variables, .fun, ..., .progress = "none")
names=c("John","Mary","Alice","Peter","Roger","Phyillis")
age=c(13,15,14,13,14,13)
sex=c("Male","Female","Female","Male","Male","Female")
data_1=data.frame(names,age,sex)

amean=function(data){
  agemean=mean(data[,2])
  return(agemean)
}

d_out1=daply(data_1,.(sex),.fun=amean)
d_out2=daply(data_1,.(sex),.fun=amean)
d_out3=ddply(data_1,.(sex),.fun=amean)
d_out4=ddply(data_1,.(age,sex),.fun=amean,.progress = "text")
d_out5=daply(data_1,1,.fun=amean)
d_out6=daply(data_1,2,.fun=amean)

#3. llply,laply,ldply  与lapply
#l*ply(.data, .fun, ..., .progress = "none")
#laply --> array
#llply --> list
#ldply --> dataframe
x<-list(a=1:10,beta=exp(-3:3),logic=c(TRUE,FALSE,FALSE,TRUE,TRUE,FALSE))
#x_out1=lapply(x,length)
x_out1=lapply(x,table)
x_out1=lapply(x,mean)
x_out1=lapply(x,fivenum)
a=c(1,2,3,4,1,5,7,8,9,4,2)
b=c(1,2,5,7,6,4,8,7)
c=c(4,8,9,1,2,3,1)
y<-list(a,b,c)

x_out2=llply(y,mean)
x_out3=laply(y,mean)
x_out4=ldply(y,mean)


#4.    mlply,mdply,maply 与mapply的对比
#m*ply(.data,.fun=NULL,.inform=FALSE,…)
#mlply ==> list
#maply --> array
#mdply --> dataframe

data2=data.frame(n=c(10,10,10),mean=c(5,5,10),sd=c(1,2,1))
y_out1=mlply(data2,rnorm)
y_out2=mdply(data2,rnorm)
y_out3=maply(data2,rnorm)


#5.  splat()
hp_per_cyl<-function(hp, cyl, ...) hp/ cyl
mpg_per_cyl <- function(mpg,cyl,...) mpg/cyl
splat(hp_per_cyl)(mtcars)
splat(mpg_per_cyl)(mtcars)

#6.  each()
a=c(1,2,3,4,1,5,7,8,9,4,2)
each(min,max,mean)(a)
fun<-function(x){c(min = min(x), max = max(x),mean=mean(x))}

#7.  colwise()
head(baseball,5)
nmissing=function(x) (sum(is.na(x))/length(x))
nmissing=function(x) sum(is.na(x))
colwise(nmissing)(baseball)

ddply(baseball,.(year),colwise(nmissing))
ddply(baseball,.(year),colwise(nmissing,.(sb,cs,so)))
z1<-ddply(baseball,.(year),colwise(nmissing,.(sb)))
#8.  failwith()

#9.  arrange()

#10. rename()

#11.  match_df()

#12.  join()