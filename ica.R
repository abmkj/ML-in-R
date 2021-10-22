# Implementation of the Fast ICA algorithm developed by Hyvärinen (1999)
#######################################

fastICA_hyv &lt;- function(X, max_iters)
{
  X &lt;- scale(X, center = TRUE, scale = FALSE) # Centering
  
  S &lt;- cov(X) #Covariance matrix
  s &lt;- svd(S) #SVD
  K &lt;- s$u %*% diag((sqrt(s$d))^(-1)) %*% t(s$v)
  X &lt;- X %*% K
  
  N &lt;- dim(X)[1]
  p &lt;- dim(X)[2]
  
  # Want W = X^{-1}S
  W &lt;- matrix(runif(p^2), p, p)
  e &lt;- matrix(1, N, 1)
  
  for (iter in 1:max_iters)
  {
    for (j in 1:p) {
      W[, j] &lt;- 1/N * t(X) %*% tanh(X %*% W[, j]) - 
        1/N * (t(e) %*% (1 - tanh(X %*% W[, j])^2))[1] * W[, j]
    }
    svd &lt;- svd(W)
    W &lt;- svd$u %*% t(svd$v)
  }
  S &lt;- X %*% W
  return(S)
}
</pre></body></html>Ztext/plainUUTF-8