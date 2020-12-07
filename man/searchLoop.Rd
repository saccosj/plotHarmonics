\name{searchLoop}
\alias{searchLoop}

\title{Loops the searchInt function to find Interaction results for multiple regions *and* sets}

\description{
searchLoop outputs the results of a linear interaction model within generated regions for multiple regions and sets.
}

\usage{
searchLoop (data, nbin, type= "p")
}

\arguments{
  \item{data}{Dataframe containing X, Y, and M.}
  \item{nbin}{The maximum number of moderator regions to create in sets.}
  \item{type}{The type of metric returned by searchInt. Default is "p" for interaction significance (for use within CALMplot). Other options include: "b" for beta coefficient, "t" for t-statistic, and "se" for standard error of beta coefficient.}
}

\value{
  \item{interaction values}{returns a matrix of values (i.e., p-values) for each region given each set of regions }
}


\author{
Shane J. Sacco, M.A.
University of Connecticut
}

\examples{
Dataset <- getData (n = 500, b0 = 0, b1 = 0, b2 = 0, b3 = 0.5, e = 1, r = 0)

searchLoop (Dataset, nbin = 5, type = "p")

#OUTPUT
            [,1]         [,2]         [,3]         [,4]         [,5]
[1,] 3.566037e-21           NA           NA           NA           NA
[2,] 5.568555e-14 2.930241e-06           NA           NA           NA
[3,] 3.914263e-09 7.624385e-01 7.621266e-05           NA           NA
[4,] 5.404151e-05 5.407523e-02 7.247969e-02 0.0000152546           NA
[5,] 4.124666e-05 1.696683e-01 5.320415e-02 0.5618365181 0.0001987147

}

\keyword{ hplot }
\keyword{ distribution }
