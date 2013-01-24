\name{agreement}
\alias{agreement}
\title{Calculate van der Eijk's measure of agreement A}
\description{Calculate agreement in ordered rating scales. This function implements van der Eijk's (2001) measure of agreement A, which can be used to describe agreement, consensus, or polarization among respondents.}
\usage{agreement(V, old = FALSE)}
\arguments{
  \item{V}{A frequency vector}
  \item{old}{Optional argument if you wish to use the old algorithm for agreement A, as outlined in van der Eijk's article. There is normally no reason to set the \code{old} argument.}
}
\details{This is the main function to calculate agreement. A frequency vector describes the number of observations in a given category. For example, the vector [10,20,30,15,4] describes 10 observations with position 1, 20 observations with position 2, 30 observations with position 3, 15 observations with position 4, and 4 observations with position 5. At least three categories are required to calculate agreement.}
\value{The function returns the measure of agreement A. A is 1 if there is perfect unimodality (=agreement); A is 0 if there is perfect uniformity; A is -1 if there is perfect bimodality (=lack of agreement)}
\references{van der Eijk, C. (2001) Measuring agreement in ordered rating scales, Quality and Quantity 35(3):325-341.}
\author{Didier Ruedin}
\seealso{\code{\link{polarization}}}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
# Calculate agreement A
agreement(V)
# The rate of agreement is given as 0.6113333
}
