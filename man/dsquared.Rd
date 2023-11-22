\name{dsquared}
\alias{dsquared}
\title{Calculate d-squared}
\description{Calculate Blair and Lacy's d-squared.}
\usage{dsquared(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates Blair and Lacy's d-squared, a measure of concentration based on squared Euclidean distances. This function follows the presentation by Blair and Lacy 2000. The measure l-squared normalizes the values and is implemented as \code{\link{lsquared}}.}
\value{The function returns the d-squared.}
\references{Blair, J., and M. Lacy. 2000. Statistics of Ordinal Variation. Sociological Methods & Research 28 (3): 251-280.}
\author{Didier Ruedin}
\seealso{\code{\link{lsquared}}, \code{\link{BlairLacy}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
dsquared(V)
}
