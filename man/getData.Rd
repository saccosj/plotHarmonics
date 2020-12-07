\name{getData}
\alias{getData}

\title{Generate random data for Monte Carlo Simulation}
\description{
getData returns a randomly generated dataframe based upon defined parameters.
}
\usage{
getData (n, b0, b1, b2, b3, e, r)
}

\arguments{
  \item{n}{The sample size of simulated dataset.}
  \item{b0}{The intercept coefficient in the linear interaction model.}
  \item{b1}{The main effect of predictor coefficient in the linear interaction model.}
  \item{b2}{The main effect of moderator coefficient in the linear interaction model.}
  \item{b3}{The interaction effect coefficient in the linear interaction model.}
  \item{e}{The standard error of linear interaction model.}
  \item{r}{Defines the underlying interaction model will be continuous (linear) = 0, discontinuous = 1, or continous (exponential) = 2.}
}

\value{
  \item{data }{returns a dataframe containing n rows and columns representing X, M, Y}
}

\author{
Shane J. Sacco, M.A.
University of Connecticut
}

\examples{
Dataset <- getData (n = 500, b0 = 0, b1 = 0, b2 = 0, b3 = 0.5, e = 1, r = 0)

Dataset [1:5,]

#OUTPUT
           X          M          Y
1  0.8143536 -1.5162052 -1.0605156
2  0.2167773  1.4209432  0.1572903
3 -1.3769867  1.7570338 -1.2116197
4 -0.7798888  0.7395610 -1.0528022
5  1.4533584  0.6394392 -1.3378827

}


\keyword{ hplot }
\keyword{ distribution }
