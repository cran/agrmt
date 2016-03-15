\name{MRQ}
\alias{MRQ}
\title{Calculates MRQ polarization index}
\description{This function calculates the MRQ polarization index from a population vector.}
\usage{MRQ(Z)}
\arguments{ \item{Z}{(Standardized) frequency vector}}
\details{This function implements the polarization index introduced by Garcia-Montalvo
and Reynal-Querol (2005), also known as the Reynal-Querol index of polarization (RQ). It is a measure of dispersion based on squared Euclidean distances. The frequency vector needs to be standardized for the Reynal-Querol index to work; if the sum of the frequency vector is not 1 (i.e. it is not standardized), the function automatically standardizes the frequency vector by dividing each element of the vector by the sum of the vector. The assumption is that the frequencies are complete.}
\value{Index of polarization (RQ).}
\references{Garcia-Montalvo, J., and Reynal-Querol, M. 2005. Ethnic Diversity and Economic Development. Journal of Development Economics 76:293-323.

Reynal-Querol, Marta. 2002. Ethnicity, Political Systems, and Civil Wars. Journal of Conflict Resolution 46 (1): 29-54.}
\author{Didier Ruedin}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
MRQ(V)
}
