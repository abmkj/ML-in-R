rm(list=ls())
setwd( "/Users/Home/" )
library(MASS)

#Score function
score_func&lt;- function(beta_0,beta_1, sigma, X, Y){
	part1&lt;- (Y - beta_0 - beta_1*X)/sigma
	part2&lt;- ((Y - beta_0 - beta_1*X)/sigma)*X
	part3&lt;- -1/(2*sigma) + ((Y - beta_0 - beta_1*X)^2 )/(2*sigma^2)
	out&lt;- c(sum(part1), sum(part2), sum(part3))
	return( out )
	}
	
#Hessian function
hes_func&lt;- function(beta_0, beta_1, sigma, X, Y){
	start&lt;- matrix(0, nrow = 3, ncol = 3)
	start[1,1]&lt;- length(Y)/sigma
	start[1,2]&lt;- start[2,1]&lt;- sum(X)/sigma
	start[2,2]&lt;-  sum(X^2)/sigma
	start[3,3]&lt;- length(Y)/(2 * sigma^2)
	return(- start) #Returning the negative as usual for the Hessian
	}	

#Newton-Raphson algorithm
nr_func&lt;- function(guess, X, Y){
	c&lt;- 0 
	count&lt;- 0 
	old&lt;- guess
	while(c==0){
	count&lt;- count + 1
		if(count&gt;1){
			old&lt;- new
			}
		part1&lt;- score_func(old[1], old[2], old[3], X, Y)
		part2&lt;- hes_func(old[1], old[2], old[3], X, Y)
		new&lt;- old - solve(part2)%*%part1
		if(count&gt;2){ #Comparing old and new values
			diff&lt;- abs(old - new)
			if(max(diff)&lt;1e-5){
				c&lt;- 1
				}
			}
		}
	return(new)
}

#Illustration
X&lt;- rnorm(1000)
Y&lt;- 0.5 + 0.25*X + rnorm(1000)	
guess&lt;- mvrnorm(1, c(0,0, 0), diag(1,3)) #Initial guess
guess[3]&lt;- exp(guess[3]) #Ensuring positive value

get_params&lt;- nr_func(rnorm(3, 0.5, sd= 0.1), X, Y)
mle_linear&lt;- function(x, y){
out&lt;- solve(t(x)%*%x)%*%t(x)%*%y
return(out)
}

mle_sigma&lt;- function(x, y){
coef&lt;- mle_linear(x, y)
preds&lt;- x%*%coef
diff&lt;- y - preds
ssr&lt;- sum( (diff)^2)/length(y)
return(ssr)
}
analytic_sol&lt;- c(mle_linear(cbind(1, X), Y), mle_sigma(cbind(1, X), Y))
	

	
	
	
</pre></body></html>Ztext/plain