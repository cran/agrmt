\name{Kvalseth}
\alias{Kvalseth}
\title{Calculate Kvalseth's COV}
\description{Calculate Kvalseth's COV.}
\usage{Kvalseth(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates Kvalseth's COV, a measure of dispersion based on linear Euclidean distances. It is based on the IOV measure, implemented as \code{\link{BerryMielke}}. This function follows the presentation by Blair and Lacy 2000.}
\value{The function returns the COV.}
\references{Blair, J., and M. Lacy. 2000. Statistics of Ordinal Variation. Sociological Methods & Research 28 (3): 251-280.}
\author{Didier Ruedin}
\seealso{\code{\link{BerryMielke}}, \code{\link{lsquared}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
Kvalseth(V)
}
