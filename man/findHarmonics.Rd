\name{findHarmonics}
\alias{findHarmonics}

\title{Find interactions below alpha in regions and sets for a randomly simulated dataset}

\description{
findHarmonics returns the percentage or total number of regions containing an interaction below alpha, within defined sets of a single simulation.
}
\usage{
findHarmonics (res, nbin, power = TRUE, alpha = .05, pro = FALSE)
}

\arguments{
  \item{res}{The output from the searchLoop function.}
  \item{nbin}{The maximum number of moderator regions to create in sets.}
  \item{power}{If TRUE, the function returns the percentage or total number of regions within a set that are below alpha, dependent upon whether pro = TRUE/FALSE. If FALSE, the function returns the returns results of searchLoop. The default is TRUE.}
  \item{alpha}{The alpha criteria for two-sided statistical tests. The default is .05.}
  \item{pro}{If TRUE, the function returns the percentage of moderator regions within a set that contain an interaction below alpha. If FALSE, returns the total number of regions within a set that contain an interaction below alpha. The default is FALSE.}
}

\value{
  \item{percentage}{If power and pro are TRUE, returns a list of values the length of nbin, which the last value is the percentage of regions within a set that contain an interaction below alpha.}
  \item{total}{If power is TRUE, and pro is FALSE, returns a list of values the length of nbin, which the last value is the total number of regions within a set that contain an interaction below alpha.}
 \item{p-values}{If power is FALSE, returns a matrix of values nbin by nbin, which are p-values of each region for each set. It is equivalent to the output of searchLoop.}
}

\author{
Shane J. Sacco, M.A.
University of Connecticut
}

\examples{
Dataset <- getData (n = 500, b0 = 0, b1 = 0, b2 = 0, b3 = 0.5, e = 1, r = 0)

Search_Res <- searchLoop (Dataset, type = "p", nbin = 5)

findHarmonics (Search_Res, power = TRUE,  nbin = 5)

#OUTPUT
[1]  NA  NA  NA  NA 0.2

}

\keyword{ hplot }
\keyword{ distribution }
