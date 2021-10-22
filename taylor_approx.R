##Implementation of Taylor Series approximation

#Taylor Series for exponential
taylor_approx<- function(a, x, order){
	out<- 0
	for(z in 0:order){
	out<- out + (exp(a)/factorial(z))*(x - a)^z
	}
	return(out)
	}


x<- seq(-10, 10, len = 1000)

plot(exp(x)~x, type='l', lwd = 3)

lines(taylor_approx(1, x, 0)~x, col='red')
lines(taylor_approx(1, x, 1)~x, col='red')
lines(taylor_approx(1, x, 2)~x, col='red')
lines(taylor_approx(1, x, 3)~x, col= 'red')
lines(taylor_approx(1, x, 4)~x, col='red')
lines(taylor_approx(1, x, 5)~x, col='red')
lines(taylor_approx(1, x, 6)~x, col='red')
lines(taylor_approx(1, x, 7)~x, col='red')
lines(taylor_approx(1, x, 8)~x, col='red')


#Taylor Series for cosine
approx_cos<- function(a, x, order){
	out<- 0 
	int<- 0 
	vals<- c(cos(a), -sin(a), -cos(a), sin(a))
	for(z in 0:order){
		int<- int +1
		out<- out + (vals[int]/factorial(z))*(x - a)^z
		if(int==4){
			int<- 0
			}
		}
	return(out)
	
	}


x<- seq(-10, 10, len = 1000)
plot(cos(x)~x, type='l', lwd = 3)

lines(approx_cos(0, x, 1)~x, col='red')
lines(approx_cos(0, x, 2)~x, col='red')
lines(approx_cos(0, x, 3)~x, col= 'red', lwd = 3)
lines(approx_cos(0, x, 4)~x, col='red', lwd = 3)
lines(approx_cos(0, x, 5)~x, col='red', lwd = 3)
lines(approx_cos(0, x, 6)~x, col='red', lwd = 3)
lines(approx_cos(0, x, 7)~x, col='red', lwd = 3)
lines(approx_cos(0, x , 20)~x, col='red', lwd=3)
lines(approx_cos(0, x, 100)~x, col='red', lwd = 3)


