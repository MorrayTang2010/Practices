download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", 
              "./german.data")
data <- read.table("./german.data")
str(data)
library("neuralnet")
#只用数值变量
NNModelAllNum <- neuralnet(V21 ~ V2 + V5 + V8, data)
NNModelAllNum

#放入定性变量
NNModelAllNum.2 <- neuralnet(V21 ~ V1 + V2 + V5 + V8, data)

#对V1进行哑变量处理
dummyV1 <- model.matrix(~V1, data)

#将哑变量加入到模型中
#因为model.matrix对数值型和分类为2的变量没有变化，因此可以写在一起
modelData <- model.matrix(~V1 + V2 + V5 + V8 + V21, data)

NNModel.3 <- neuralnet(V21 ~ V1A12 + V1A13 + V1A14, modelData)
