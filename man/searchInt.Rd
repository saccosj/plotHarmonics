\name{searchInt}
\alias{searchInt}

\title{Perform linear interaction model and extract interaction results}

\description{
searchInt outputs the results of a linear interaction model within generated regions.
}

\usage{
searchInt (data, nbin)
}

\arguments{
  \item{data}{Dataframe containing X, Y, and M.}
  \item{nbin}{The number of percentile-ranked moderator regions to create.}
}

\value{
  \item{interaction values}{returns a matrix of values containing columns representing the interaction beta coefficient, standard error of that coefficient, the one sample t-statistic, and p-value. Each row represents a defined region of the moderator, given nbin rows}
}

\author{
Shane J. Sacco, M.A.
University of Connecticut
}

\examples{
Dataset <- getData (n = 500, b0 = 0, b1 = 0, b2 = 0, b3 = 0.5, e = 1, r = 0)

searchInt (Dataset, nbin = 5)

#OUTPUT
           [,1]      [,2]       [,3]         [,4]
[1,]  0.8574155 0.1994616  4.2986503 4.124666e-05
[2,]  0.7834068 0.5661802  1.3836705 1.696683e-01
[3,] -1.5197237 0.7763976 -1.9574038 5.320415e-02
[4,] -0.3371944 0.5792316 -0.5821408 5.618365e-01
[5,]  0.7108064 0.1836914  3.8695687 1.987147e-04

}

\keyword{ hplot }
\keyword{ distribution }
