\name{ajusCheck}
\alias{ajusCheck}
\title{Sensitivity test for AJUS}
\description{Check sensitivity of AJUS to different tolerance parameters.}
\usage{ajusCheck(V, t=seq(from=0.05, to=0.2, by=0.05), variant="modified")}
\arguments{
  \item{V}{A frequency vector}
  \item{t}{A vector of tolerance parameters to check. Differences smaller than or equal to the tolerance are ignored.}
  \item{variant}{Strict AJUS following Galtung, or modified to include F and L types (default)}
}
\details{This function runs the AJUS system with a range of tolerance parameters. You can easily check how sensitive the classification of the distribution is to the tolerance parameter.}
\value{The function returns a list. The \code{tolerance} returns the tolerance parameters tested. The \code{type} returns a series of strings corresponding to the pattern described by Galtung (A,J,U,S) or (F, L) for each tolerance parameter. The \code{skew} returns a number to describe the direction of the skew. See \code{\link{ajus}} for a description of the different arguments and the AJUS types.}
\author{Didier Ruedin}
\seealso{\code{\link{ajus}}}
