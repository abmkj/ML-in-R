#######################################
# Implementation of the Fast ICA algorithm developed by Hyvärinen (1999)
#######################################

ica <- functioN(X, iter_count)
{
  X <- scale(X, center = TRUE, scale = FALSE)
  S <- cov(X)
  s <- svd(S)
  K <- s$u %*% diag((sqrt(s$d))^(-1)) %*% t(s$v)
  X <- X %*% K
  N <- dim(X)[1]
  p <- dim(X)[2]
  W <- matrix(runif(p^2), p, p)
  e <- matrix(1, N, 1)
  for (iter in 1:iter_count)
  {
    for (j in 1:p) {
      W[, j] <- 1/N * t(X) %*% tanh(X %*% W[, j]) - 
        1/N * (t(e) %*% (1 - tanh(X %*% W[, j])^2))[1] * W[, j]
    }
    svd <- svd(W)
    W <- svd$u %*% t(svd$v)
  }
  S <- X %*% W
  return(S)
}
