\name{lsquared}
\alias{lsquared}
\title{Calculate l-squared}
\description{Calculate Blair and Lacy's l-squared.}
\usage{lsquared(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates Blair and Lacy's l-squared, a measure of concentration based on squared Euclidean distances. This function follows the presentation by Blair and Lacy 2000. The measure `l' by Blair and Lacy is implemented as \code{\link{BlairLacy}}.}
\value{The function returns the l-squared.}
\references{Blair, J., and M. Lacy. 2000. Statistics of Ordinal Variation. Sociological Methods \& Research 28 (3): 251-280.}
\author{Didier Ruedin}
\seealso{\code{\link{BerryMielke}}, \code{\link{BlairLacy}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
lsquared(V)
}
