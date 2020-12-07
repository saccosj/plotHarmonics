bootCA <- function (n, b0 = 0, b1 = 0, b2 = 0, b3, e = 1, r, nbin, boot, power = TRUE, type = "p", directory) {
  if (power == TRUE){
    res1 <- array (NA, dim=c(nbin,boot))
    for (i in 1:boot){
      file_name = paste(directory, "sim_n", n, "_int", b3*100,
                        "_e", e, "_lin", r, "_k", boot, "_i", i, ".csv", sep="")
      data = getData (n, b0, b1, b2, b3, e, r)
      write.csv(data, file_name)

      res1[,i] = findHarmonics (res = searchLoop (data, type=type, nbin=nbin), power=power,  nbin=nbin)
      cat("\014")
      print(i/boot*100)
    }
    print(rowMeans(res1, dims=1))
    return(rowMeans(res1, dims=1))
  }
  else {
    res1 <- array (NA, dim=c(nbin, nbin, boot))
    out <- array (NA, dim=c(nbin,nbin))
    for (i in 1:boot){
      file_name = paste(directory, "sim_n", n, "_int", b3*100,
                        "_e", e, "_lin", r, "_k", boot, "_i", i, ".csv", sep="")
      data = getData (n, b0, b1, b2, b3, e, r)
      write.csv(data, file_name)

      res1[,,i] = findHarmonics (res = searchLoop (data, type=type, nbin=nbin), power=power,  nbin=nbin)
      cat("\014")
      print(i/boot*100)
    }
    for (i in 1:nbin){
      for (j in 1:i){
        out [i,j] = mean(res1[i,j,])
      }
    }
    return(out)
  }
}
