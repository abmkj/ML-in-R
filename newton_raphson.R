setwd( "/Users/Home/" )
library(MASS)

#Score function
score_func&amp;lt;- function(beta_0,beta_1, sigma, X, Y){
	part1&amp;lt;- (Y - beta_0 - beta_1*X)/sigma
	part2&amp;lt;- ((Y - beta_0 - beta_1*X)/sigma)*X
	part3&amp;lt;- -1/(2*sigma) + ((Y - beta_0 - beta_1*X)^2 )/(2*sigma^2)
	out&amp;lt;- c(sum(part1), sum(part2), sum(part3))
	return( out )
	}
	
#Hessian function
hes_func&amp;lt;- function(beta_0, beta_1, sigma, X, Y){
	start&amp;lt;- matrix(0, nrow = 3, ncol = 3)
	start[1,1]&amp;lt;- length(Y)/sigma
	start[1,2]&amp;lt;- start[2,1]&amp;lt;- sum(X)/sigma
	start[2,2]&amp;lt;-  sum(X^2)/sigma
	start[3,3]&amp;lt;- length(Y)/(2 * sigma^2)
	return(- start) #Returning the negative as usual for the Hessian
	}	

#Newton-Raphson algorithm
nr_func&amp;lt;- function(guess, X, Y){
	c&amp;lt;- 0 
	count&amp;lt;- 0 
	old&amp;lt;- guess
	while(c==0){
	count&amp;lt;- count + 1
		if(count&amp;gt;1){
			old&amp;lt;- new
			}
		part1&amp;lt;- score_func(old[1], old[2], old[3], X, Y)
		part2&amp;lt;- hes_func(old[1], old[2], old[3], X, Y)
		new&amp;lt;- old - solve(part2)%*%part1
		if(count&amp;gt;2){ #Comparing old and new values
			diff&amp;lt;- abs(old - new)
			if(max(diff)&amp;lt;1e-5){
				c&amp;lt;- 1
				}
			}
		}
	return(new)
}

#Illustration
X&amp;lt;- rnorm(1000)
Y&amp;lt;- 0.5 + 0.25*X + rnorm(1000)	
guess&amp;lt;- mvrnorm(1, c(0,0, 0), diag(1,3)) #Initial guess
guess[3]&amp;lt;- exp(guess[3]) #Ensuring positive value

get_params&amp;lt;- nr_func(rnorm(3, 0.5, sd= 0.1), X, Y)
mle_linear&amp;lt;- function(x, y){
out&amp;lt;- solve(t(x)%*%x)%*%t(x)%*%y
return(out)
}

mle_sigma&amp;lt;- function(x, y){
coef&amp;lt;- mle_linear(x, y)
preds&amp;lt;- x%*%coef
diff&amp;lt;- y - preds
ssr&amp;lt;- sum( (diff)^2)/length(y)
return(ssr)
}
analytic_sol&amp;lt;- c(mle_linear(cbind(1, X), Y), mle_sigma(cbind(1, X), Y))
	

	
	
