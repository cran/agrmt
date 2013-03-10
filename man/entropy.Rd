\name{entropy}
\alias{entropy}
\title{Calculate Shannon entropy}
\description{Calculate Shannon entropy, following Tastle and Wierman.}
\usage{entropy(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates the Shannon entropy following Tastle and Wierman (2007). A frequency vector describes the number of observations in a given category. For example, the vector [10,20,30,15,4] describes 10 observations with position 1, 20 observations with position 2, 30 observations with position 3, 15 observations with position 4, and 4 observations with position 5.

This function follows Tastle and Wierman and ignores categories with zero observations. This does not follow the formula indicated.

See \code{\link{consensus}} for a function that considers the order of categories.}
\value{The function returns the Shannon entropy.}
\references{Tastle, W., and M. Wierman. 2007. Consensus and dissention: A measure of ordinal dispersion. International Journal of Approximate Reasoning 45 (3): 531-545.}
\author{Didier Ruedin}
\seealso{\code{\link{consensus}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
# Calculate entropy
entropy(V)
}
