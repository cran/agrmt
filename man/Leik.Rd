\name{Leik}
\alias{Leik}
\title{Calculate ordinal dispersion}
\description{Calculates ordinal dispersion as introduced by Leik (1966)}
\usage{Leik(V)}
\arguments{
  \item{V}{A frequency vector}
}
\details{This function calculates ordinal dispersion as introduced by Robert K. Leik (1966). It uses the cumulative frequency distribution to determine ordinal dispersion. The extremes (agreement, polarization) largely correspond to the types used by Cees van der Eijk. By contrast, the mid-point depends on the number of categories: it tends toward 0.5 as the number of categories increases. Leik defends this difference by highlighting the increased probability of falling into polarized patterns when there are fewer categories. If all observations are in the same category, ordinal dispersion is 0. With half the observations in one extreme category, and half the observations in the other extreme, Leik's measure gives a value of 1.

The dispersion measure is a percentage, and can be interpreted accordingly. Ordinal dispersion can be used to express consensus or agreement, simply by taking: 1 - ordinal dispersion.}
\value{The function returns the ordinal dispersion}
\references{Leik, R. (1966) A measure of ordinal consensus, Pacific Sociological Review 9(2):85-90.}
\author{Didier Ruedin}
\seealso{\code{\link{polarization}}, \code{\link{agreement}}}
\examples{
# Example 1:
V <- c(30,40,210,130,530,50,10)
# Calculate polarization
Leik(V)
# The ordinal dispersion is given as 0.287
polarization(V)
# Polarization is given as 0.194 (as contrast)
}
