getData <- function (n, b0, b1, b2, b3, e, r) {

  X <- rnorm(n, 0, 1)
  M <- rnorm(n, 0, 1)
  Y <- rnorm(n, 0, 0)
  E <- rnorm(n, 0, e)
  Y <- rep(0, n)
  if (r == 1) {
    Y [M <= 0] = b3 * X [M <= 0] + E [M <= 0]
    Y [M > 0]  = -b3 * X [M > 0] + E [M > 0]
  }
  else if (r == 0) {
    Y = b0 + b1*X + b2*M + b3*X*M + E
  }
  else if (r == 2) {
    Y = b0 + b1*X + b2*M + exp(b3*X*M) + E
  }

  data <- data.frame(X,M,Y)
  return(data)
}
