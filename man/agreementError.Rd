\name{agreementError}
\alias{agreementError}
\title{Simulated coding error for agreement A}
\description{Calculate agreement in ordered rating scales, but simulates coding error.}
\usage{agreementError(V, n=500, e=0.01, pos=FALSE)}
\arguments{
  \item{V}{A vector with an entry for each individual, not a frequency vector}
  \item{n}{Number of samples in the simulation}
  \item{e}{Proportion of samples for which errors are simulated}
  \item{pos}{Vector of possible positions. If FALSE (default), the values occurring in V are set as the possible values}}
\details{This function calculates agreement A, but simulates coding error. This can be useful to estimate standard errors and central tendency if certain positions are not observed. If all positions are observed in the vector V, bootstrapping can be used to estimate standard errors. If certain positions are not observed, bootstrapping is limited. Take an extreme example: [3 0 0 0 0]. Here we have three observations at the first position, but none at the others. Bootstrapping will always lead to the same agreement score. This can be misleading if coding error can be assumed. For example, if these three observations refer to a `strongly agree' answer, it is usually conceivable that some or all of these answers could refer to `somewhat agree'. This function lets you specify how many of the observations should be assumed to be potentially mis-coded, and calculates agreement accordingly. If an observation is assumed to be potentially mis-coded, it is randomly set to the position to the left, the position to the right, or the position itself. If the first or last observation is chosen, the simulation takes care not to suggest values that could not occur.

You can run the function a few (hundred) times to get summary statistics of the result (mean, median, standard deviation, etc.). The function \code{\link{compareAgreement}} does just this, and compares the result with the agreement score if no coding error is assumed.
}
\value{The function returns the measure of agreement A.}
\author{Didier Ruedin}
\seealso{\code{\link{agreement}}, \code{\link{compareAgreement}}, \code{\link{collapse}}}
\examples{
# Sample data:
V <- c(1,1,1,1,2,3,3,3,3,4,4,4,4,4,4)
# calculate agreement; using collapse() to create the frequency vector
agreement(collapse(V))
# Calculate agreement A with coding error:
agreementError(V)
# Assume that all values could have coding error:
agreementError(V, e=1)
# Run the function a few times and show the mean:
z <- replicate(1000, agreementError(V))
mean(z) 
hist(z) # etc.
# the example mentioned, population vector [3 0 0 0 0]:
V2 <- c(1,1,1)
agreementError(V2, pos=1:5)
# you can also use the compareAgreement function
compareAgreement(V2, pos=1:5)
}
