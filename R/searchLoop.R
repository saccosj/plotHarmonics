searchLoop <- function (data, nbin, type= "p") {
  res <- array (NA, dim=c(nbin,nbin))
  for (i in 1:nbin){
    if(type == "b") { res[i,1:i] = searchInt(data=data, nbin=i)[,1] }
    else if(type == "se") { res[i,1:i] = searchInt(data=data, nbin=i)[,2] }
    else if(type == "t") { res[i,1:i] = searchInt(data=data, nbin=i)[,3] }
    else if(type == "p") { res[i,1:i] = searchInt(data=data, nbin=i)[,4] }
  }
  return(res)
}
