\name{isd}
\alias{isd}
\title{Classify changes over time}
\description{Classify changes over time using the ISD-system introduced by Galtung (1969).}
\usage{isd(V, tolerance=0.1)}
\arguments{
  \item{V}{A vector with length 3}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional). Differences smaller than or equal to the tolerance are ignored.}
}
\details{This function implements the ISD-system introduced by Galtung (1969). The input is a vector of length 3. Each value stands for a different point in time. The ISD-system examines the two transition points, and classifies the changes over time.}
\value{The function returns a list. The \code{type} returns a number corresponding to the pattern described by Galtung. The \code{description} returns a string where the two transitions are spelled out (increase, flat, decrease).}
\references{Galtung, J. (1969) Theory and Methods of Social Research. Oslo: Universitetsforlaget.}
\author{Didier Ruedin}
\seealso{\code{\link{ajus}}}
