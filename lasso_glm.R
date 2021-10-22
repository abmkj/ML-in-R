rm(list=ls())
setwd( "/Desktop/" )

#Test data
load('test.RData')

library(glmnet)
x<- LASSO.list[[1]]
y<- LASSO.list[[2]]

y.train<- y[1:697]
x.train<- x[1:697,]

y.valid<- y[-c(1:697)]
x.valid<- x[-c(1:697),]


##alpha = 1 is the LASSO
##alpha = 0 is Ridge
train.model <- cv.glmnet(x = x.train, y = y.train, alpha = 1, nfolds=5, family="binomial", type.measure="mse")

predict.new<- predict(train.model, newx = x.valid, s = train.model$lambda.min)

predict.probs<- 1/(1 + exp(-predict.new))
final.pred<- ifelse(predict.probs>0.46, 1, 0) 

