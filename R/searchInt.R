searchInt <- function (data, nbin) {
  d <- data [order(data$M),]
  step <- round(length(d$M)/nbin,0)

  if (step >= 30) {
    res <- array(NA, dim=c(nbin,4))
    rank <- round(rank(d$M) / length (d$M) * 100,0)
    d$rank <- rep(0,length(d$M))

    c<-1
    for (i in seq(from=1, to=length(d$M), by=step)){

      if ((i+step-1)<= length(d$M)){
        d$rank [i:(i+step-1)] = c
      }
      else {
        d$rank [i:length(d$M)] = c
      }
      c = c + 1
    }
    if ( c > nbin) {
      d$rank[d$rank > nbin ] = nbin
    }

    for (i in 1:nbin) {
      res [i,] = summary(lm("Y~X+M+X*M",data=d[which(d$rank==i),]))$coefficients[4,]
    }
    return (res)
  }
  else {
    print ("please reduce nbins, n per bin < 25")
  }

}
