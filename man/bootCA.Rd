\name{bootCA}
\alias{bootCA}

\title{Bootstrap Simulated Continuous Assumption Plots}

\description{bootCA outputs the results of defined simluation conditions derived from randomly generated datasets.}

\usage{
bootCA (n, b0 = 0, b1 = 0, b2 = 0, b3, e = 1, r, nbin, boot, power = TRUE, type = "p", directory)
}

\arguments{
  \item{n}{The sample size of simulated datasets to be generated.}
  \item{b0}{The intercept coefficient in the linear interaction model. The default is 0.}
  \item{b1}{The main effect of the predictor coefficient in the linear interaction model. The default is 0.}
  \item{b2}{The main effect of the moderator coefficient in the linear interaction model. The default is 0.}
  \item{b3}{The interaction effect coefficient in the linear interaction model.}
  \item{e}{The standard error of linear interaction model. The default is 1.}
  \item{r}{Defines the underlying interaction model will be continuous (linear) = 0, discontinuous = 1, or continous (exponential) = 2.}
  \item{nbin}{The maximum number of moderator regions created within sets.}
  \item{boot}{The number of random samples to generate within the simulation condition.}
  \item{power}{If TRUE, the function returns the percentage of regions within a set that are below alpha. If FALSE, the function returns a region-by-set p-values. The default is TRUE.}
  \item{type}{The type of metric to assess. The default is "p" for p-value. Parameter within bootCA feeds this parameter to the findHarmonic function. Changing this parameter in the current coding scheme is not recommended. In future uses of these functions, type may be changed to "b", "t", or "se".}
  \item{directory}{Define the directory to save the simulated datasets.}
}

\value{
  \item{power }{If power = TRUE, returns a list of values the length of nbin, where the last value is the percentage of regions within a set that are below alpha, averaged across random simulations}
  \item{p-values }{If power = FALSE, returns a matrix of p-values for regions by sets, averaged across random simulations}
}

 \author{
Shane J. Sacco, M.A.
University of Connecticut
}

\examples{
bootCA (n = 500, b0 = 0, b1 = 0, b2 = 0, b3 = 0.5, e = 1, r = 0, nbin = 4, boot = 1000, power = FALSE, type = "p", directory = "C:/Users/SSacco/Desktop/HUB/MANUSCRIPTS/WORKING/Statistics - Continuous v. Discrete Visual/plotHarmonics R Package/simulation data/raw/")

#OUTPUT
              [,1]       [,2]       [,3]       [,4]
[1,] 2.534825e-15         NA         NA         NA
[2,] 1.462437e-03 0.00175973         NA         NA
[3,] 2.359864e-02 0.24737449 0.02645954         NA
[4,] 6.294226e-02 0.34640879 0.34696437 0.06971307

}

\keyword{ hplot }
\keyword{ distribution }
