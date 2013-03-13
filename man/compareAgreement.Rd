\name{compareAgreement}
\alias{compareAgreement}
\title{Compare agreement A with and without simulated coding error}
\description{Calculate agreement in ordered rating scales, and compares this to agreement with simulated coding error.}
\usage{compareAgreement(V, n=500, e=0.01, N=500, pos=FALSE)}
\arguments{
  \item{V}{A vector with an entry for each individual}
  \item{n}{Number of samples in the simulation of coding errors}
  \item{e}{Proportion of samples for which errors are simulated}
  \item{N}{Number of replications for calculating mean and standard deviation}
  \item{pos}{Vector of possible positions. If FALSE, the values occurring in V are set as the possible values}}
\details{This function calculates agreement on a vector, and compares the value with agreement with simulated coding error. It runs the function \code{\link{agreementError}} N times. The other arguments (n, e, pos) are passed down to the \code{agreementError} function.}
\value{The function returns a list with agreement A without simulated coding errors, the mean of agreement with simulated coding error, and the standard deviation of agreement with simulated coding error.}
\author{Didier Ruedin}
\seealso{\code{\link{agreement}}, \code{\link{agreementError}}}
\examples{
# Sample data:
V <- c(1,1,1,1,2,3,3,3,3,4,4,4,4,4,4)
compareAgreement(V)
}
