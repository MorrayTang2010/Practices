#practise

swithcheroo.if.then<-function(x){
  if(x=='a'){
    "camel"
  }else if(x=="b"){
    "bear"
  }else
    "moose"
}

swithcheroo.switch<-function(x){
  switch(x,
         a="alligator",
         b="bear",
         c="camel",
         "moose")
}

x<-c("a","b","c","d","e","f")
lapply(x,swithcheroo.switch)

swithcheroo.apply<-function(x){
  apply(x,1,switch(x,
         a="alligator",
         b="bear",
         c="camel",
         "moose"))
}

