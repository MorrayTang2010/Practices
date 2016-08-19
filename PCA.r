#Run PCA
#Time: 2016.04.24

data(iris)
ir<-iris[,-(length(iris))]
y=cor(ir)
e=eigen(y)

scale(as.matrix(y) %*% e$vector)
p=prcomp(ir,scale=T)
summary(p)
predict(p)
