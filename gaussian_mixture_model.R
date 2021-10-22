
#Implementation of GMM
#K different distributions
gauss_mixture <- function(dat, K, maxIter)
{
  n <- dim(data)[1]
  set.seed(42) #Initializing seed for randomization
  mus <- runif(K, 1, 10)
  sig <- runif(1, 0.5, 2.5)
  ps <- matrix(1/K, K, 1)
  con_ps <- matrix(1/K, n, K)
  for (iter in 1:maxIter) {
    for (i in 1:n) {
      for (k in 1:K) {
        con_ps[i, k] <- dnorm(dat[i], mus[k], sig) * ps[k]
      }
      con_ps[i, 1:K] <- con_ps[i, 1:K] / sum(con_ps[i, 1:K])
    }
    for (k in 1:K) {
      mus[k] <- (t(con_ps[1:n, k]) %*% dat) / sum(con_ps[1:n, k])
      ps[k] <- sum(con_ps[1:n, k]) / n
    }
    sig <- 0
    for (i in 1:n) {
      for (k in 1:K) {
        sig <- sig + con_ps[i, k] * (dat[i] - mus[k])^2
      }
    }
    sig <- sqrt(sig / n)
  }
  return(list(mus, ps, sig))
}
