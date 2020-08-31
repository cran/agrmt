\name{polarization}
\alias{polarization}
\title{Calculate polarization}
\description{Calculates polarization, based on measure of agreement A}
\usage{polarization(V, old = FALSE)}
\arguments{
  \item{V}{A frequency vector}
  \item{old}{Specify \code{old=TRUE} to use the deprecated algorithm for agreement A}
}
\details{This function calculates polarization by re-scaling agreement A introduced by Cees van der Eijk. Whereas agreement A ranges from -1 to 1, polarization ranges from 0 to 1. If all observations are in the same category, polarization is 0. With half the observations in one category, and half the observations in a different (non-neighbouring) category, polarization is 1. Polarization is 0.5 for a uniform distribution over all categories.}
\value{The function returns a polarization score}
\author{Didier Ruedin}
\seealso{\code{\link{agreement}}}
\examples{
V <- c(30,40,210,130,530,50,10)
# Calculate polarization
polarization(V)
# The rate of polarization is given as 0.1943333
}
