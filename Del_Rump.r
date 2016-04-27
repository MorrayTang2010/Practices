#冗余函数改写
switcheroo.switch<-function(x){
  switch(x,
         a="alligator",
         b="bear",
         c="camle",
         "moose")
}

x<-c("a","b","c")
x.out<-lapply(x,switcheroo.switch)

