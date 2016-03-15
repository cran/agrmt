\name{consensus}
\alias{consensus}
\title{Calculate Tastle and Wierman's measure of consensus}
\description{Calculate consensus in ordered rating scales. This function implements Tastle and Wierman's (2007) measure of consensus (ordinal dispersion), which can be used to describe agreement, consensus, dispersion, or polarization among respondents.}
\usage{consensus(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates consensus following Tastle and Wierman (2007). The measure of consensus is based on the Shannon entropy. A frequency vector describes the number of observations in a given category. For example, the vector [10,20,30,15,4] describes 10 observations with position 1, 20 observations with position 2, 30 observations with position 3, 15 observations with position 4, and 4 observations with position 5.

If you come across an error that the vector supplied does not contain whole numbers, try \code{round(V,0)} to remove any detritus from calculating the frequency vector.}
\value{The function returns the measure of consensus. It is 1 if there is perfect uniformity; it is 0 if there is perfect bimodality (=lack of agreement)}
\references{Tastle, W., and M. Wierman. 2007. Consensus and dissention: A measure of ordinal dispersion. International Journal of Approximate Reasoning 45(3): 531-545.}
\author{Didier Ruedin}
\seealso{\code{\link{agreement}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
# Calculate consensus
consensus(V)
# The degree of consensus is given as 0.7256876
}
