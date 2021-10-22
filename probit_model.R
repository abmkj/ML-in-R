rm(list=ls())
setwd( "/Desktop/" )

###Probit likelihood
prob_lik<- function(params, X, Y){
	beta<- params
	y.tilde<- X%*%beta
	y.prob<- pnorm(y.tilde)
	out<- Y%*%log(y.prob) + (1- Y)%*%log((1- y.prob)) #These are inner products
	return(out)
	}
	
	



	