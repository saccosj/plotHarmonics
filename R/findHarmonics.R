findHarmonics <- function (res, nbin, power = TRUE, alpha = .05, pro = FALSE){
  if (pro == FALSE) {
    if (power == FALSE) {
      res1<- res
    }
    else{
      for (i in 1:nbin){
        res1<- rep(NA, nbin)
        if (sum(abs(res[i,1:i]) < alpha) == 0) {
          res1[i] = 1
        } else {
          res1[i] = 0
        }
      }
    }
  }
  else {
    if (power == FALSE) {
      res1<- res
    }
    else{
      for (i in 1:nbin){
        res1<- rep(NA, nbin)

        res1[i] = sum(abs(res[i,1:i]) < alpha) / nbin
      }
    }
  }
  return(res1)
}
