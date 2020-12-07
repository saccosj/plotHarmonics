plotHarmonics <- function (X, Y, M, data = NULL, nbin = 3, smooth = FALSE, minmax = NULL, labels = NULL, sim = NULL, alpha = .05, main = NULL) {

  type = "b"

  if (is.null(sim) == TRUE & is.null(data) == FALSE){
    d <- data
    d$X <- X
    d$Y <- Y
    d$M <- M
    search <- searchLoop (data = d, nbin = nbin, type = type )
  }
  else if (is.null(sim) == FALSE & is.null(data) == TRUE){
    search <- sim
  }
  else {
    print("ERROR: NO DATA PROVIDED! PLEASE DEFINE DATA OR SIM")
  }
  binlength <- array(NA,dim=c(10,10))
  binlength [1,1]     =   100
  binlength [2,1:2]   = c(50,50)
  binlength [3,1:3]   = c(33,33,34)
  binlength [4,1:4]   = c(25,25,25,25)
  binlength [5,1:5]   = c(20,20,20,20,20)
  binlength [6,1:6]   = c(16,17,17,17,17,16)
  binlength [7,1:7]   = c(15,14,14,14,14,14,15)
  binlength [8,1:8]   = c(11,13,13,13,13,13,13,11)
  binlength [9,1:9]   = c(11,11,11,11,11,11,11,11,12)
  binlength [10,1:10] = c(10,10,10,10,10,10,10,10,10,10)

  if (is.null(minmax) == TRUE) {
    if (type == "b") {graphY <- seq(from=-1, to=1, by=.2)}
    else if (type == "se") {graphY <- seq(from=-1, to=1, by=.2)}
    else if (type == "t") {graphY <- seq(from=-5, to=5, by=1)}
    else if (type == "p") {graphY <- seq(from=0, to=1, by=.1)}
  }
  else {
    graphY <- seq(from=minmax[1], to=minmax[2], by=abs(minmax[1]-minmax[2])/10)
  }

  if (is.null(sim) == TRUE) {
    par(mfrow=c(1,3))
    # ----------- Figure 1 -----------
    for (get in 1:nbin) {
      if (get == 1) {
        plres <- findHarmonics (res = searchLoop (data = d, nbin = get, type = "p" ), power=TRUE,  nbin=get, alpha=alpha, pro=TRUE)[get]
      }
      else {
        plres = rbind(plres, findHarmonics (res = searchLoop (data = d, nbin = get, type = "p" ), power=TRUE,  nbin=get, alpha=alpha, pro=TRUE)[get])
      }
    }
    plot(plres, xlim=c(1,nbin), ylim=c(0,1), xaxt="n", xlab= "Included Regions", ylab="Proportion of Significant Interactions")
    axis(1, seq(from=1,to=nbin, by=1))
    abline(h=seq(from=0, to=1, by=.1), lty=1, col="gray90")
    lines(plres, lty=1)

    # ----------- Figure 2 -----------

    img <- t(findHarmonics (res = searchLoop (data = d, nbin = nbin, type = "p" ), power=FALSE,  nbin=nbin, alpha=alpha, pro=TRUE))

    plot(seq(from=0,to=nbin,by=100), seq(from=0, to=1, by=100), type='n', xlim=c(1,nbin),ylim=c(0,1), xlab="Included Regions", ylab="p-value", xaxt="none")
    axis(1, seq(from=1,to=nbin, by=1))
    abline(h=seq(from=0, to=1, by=.1), lty=1, col="gray90")
    points(rep(1,nbin), img[,1], pch=18, cex=1.5)
    for (run in 2:nbin) {
      text(rep(run,nbin), img[,run], label=seq(from=1,to=nbin,by=1))
    }
  }


  # ----------- Figure 3 -----------

  if(is.null(labels) == TRUE) {
    plot(seq(from=0,to=100,by=10), graphY, type='n', xlab = "Moderator (percentile)", ylab = type, main = main)
  }
  else {
    plot(seq(from=0,to=100,by=10), graphY, type='n', xlab=labels[1], ylab=labels[2], main = main)
  }
  if (is.null(sim) == TRUE) {
    int <- summary(lm("Y~X+M+X*M",data=d))$coefficients[4,]
    xplot <- c(1,100,100,1)
    yplot <- c((int[1]-1.96*int[2]), (int[1]-1.96*int[2]),(int[1]+1.96*int[2]), (int[1]+1.96*int[2]))
    polygon(xplot,yplot, border=NA,col="gray90")
    abline(h=graphY, lty=1, col="gray90")
  }


  for (i in 1:nbin) {
    for (j in 1:i) {
      if (j == 1){
        graph <- rep(search[i,j],binlength[i,j])
      }
      else{
        graph <- c(graph,rep(search[i,j],binlength[i,j]))
      }
    }
    if (smooth == TRUE) {
      lines (smooth.spline(graph,df=10), lty=i)
    }
    else {
      lines (graph, lty=i)
    }
  }
  if(is.null(minmax)==TRUE){
    if (type == "b") {legendY <- 0}
    else if (type == "se") {legendY <- 0}
    else if (type == "t") {legendY <- 0}
    else if (type == "p") {legendY <-.5}
  }
  else {
    if (type == "b") {legendY <- (minmax[2]*.98)}
    else if (type == "se") {legendY <- 0}
    else if (type == "t") {legendY <- 0}
    else if (type == "p") {legendY <-.5}
  }
  if (is.null(sim)==TRUE){
    legend(x=1, y=legendY, legend=seq(from=1,to=nbin, by=1), lty=1:nbin, cex=1, bty = "n", title = "Included Regions" )
  }
}
