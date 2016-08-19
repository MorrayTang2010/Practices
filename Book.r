require(doParallel)
require(foreach)
#others:
sqrts.1to5<-foreach(i=1:5) %dopar% sqrt(i)
`%mydo%`<-function(a,b){2*a + 2*b}
1 %mydo% 1

l<-list(a=1,b=2,c=3,d=4,e=5,f=6,g=7,h=8,i=9,j=10)
l$j

sample.formula<-as.formula(y~x1+x2+x3)

#捕获错误
res<-try({x<1},silent=TRUE)
res2<-try({open("file that doesn't exist!\n")},silent=TRUE)
res2
#函数
a<-1:7
sapply(a,sqrt)

apply.to.three<-function(f){f(3)}
apply.to.three(function(x) {x*7})
#data anlizy
get.quotes<-function(ticker,from=(Sys.Date()-365),
                      to=(Sys.Date()),
                      interval="d")
{
  base<-"http://ichart.finace.yahoo.com/table.csv?"
  symbol<-paste("s=",ticker,sep="")
  
  from.month<-paste("&a=",formatC(as.integer(format(from,"%m"))-1,width=2,flag="0"),
                    sep="");
  from.day<-paste("&b=",format(from,"%d"),sep="");
  from.year<-paste("&c=",format(from,"%Y"),sep="");
  to.month<-paste("&d=",formatC(as.integer(format(to,"%m"))-1,width=2,flag="0"),
                  sep="");
  to.day<-paste("&e=",format(to,"%d"),sep="");
  to.year<-paste("&f=",format(to,"%Y"),sep="");
  inter<-paste("&g=",interval,sep="")
  last<-"&ignore=.csv";
  
  #paste into together
  url<-paste(base,symbol,from.month,from.day,from.year,to.month,to.day,inter,last,sep="")
  
  tmp<-read.csv(url);
  
  cbind(symbol=ticker,tmp)
}