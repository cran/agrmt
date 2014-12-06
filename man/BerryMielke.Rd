\name{BerryMielke}
\alias{BerryMielke}
\title{Calculate IOV}
\description{Calculate Berry and Mielke's IOV.}
\usage{BerryMielke(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates Berry and Mielke's IOV, a measure of dispersion based on squared Euclidean distances. This function follows the presentation by Blair and Lacy 2000, but includes the adjustment for Tmax omitted by Blair and Lacy as there is no reason to leave it out. The derived measure COV by Kvalseth is implemented as \code{\link{Kvalseth}}. Usually, the IOV is equivalent to 1-\code{\link{lsquared}}.}
\value{The function returns the IOV.}
\references{Blair, J., and M. Lacy. 2000. Statistics of Ordinal Variation. Sociological Methods \& Research 28 (3): 251-280.

Berry, K., and P. Mielke. 1992. Assessment of Variation in Ordinal Data. Perceptual and Motor Skills 74 (1): 63-66.
}
\author{Didier Ruedin}
\seealso{\code{\link{lsquared}}, \code{\link{Kvalseth}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
BerryMielke(V)
}
