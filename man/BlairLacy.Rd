\name{BlairLacy}
\alias{BlairLacy}
\title{Calculate l}
\description{Calculate Blair and Lacy's l.}
\usage{BlairLacy(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates Blair and Lacy's l, a measure of concentration based on linear Euclidean distances. This function follows the presentation by Blair and Lacy 2000. The measure l-squared by Blair and Lacy is implemented as \code{\link{lsquared}}.}
\value{The function returns the l.}
\references{Blair, J., and M. Lacy. 2000. Statistics of Ordinal Variation. Sociological Methods & Research 28 (3): 251-280.}
\author{Didier Ruedin}
\seealso{\code{\link{lsquared}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
BlairLacy(V)
}
