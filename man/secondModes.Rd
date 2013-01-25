\name{secondModes}
\alias{secondModes}
\title{Most common and second most common values}
\description{Identifies the most common (multiple) modes for frequency vectors as well as the second most common values.}
\usage{secondModes(V, pos=FALSE, tolerance=0.1)}
\arguments{
  \item{V}{A frequency vector}
  \item{pos}{Categories of frequency vector (optional)}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional).  Differences smaller than or equal to the tolerance are ignored.}
}
\details{This function identifies which positions of a frequency vector correspond to the mode(s) as implemented in the \code{\link{modes}} function. It also reports the second most common position in the same manner.}
\value{The function returns a list for the most common and the second most common value(s). The output corresponds to that of the \code{\link{modes}} function.}
\author{Didier Ruedin}
\seealso{\code{\link{modes}}}
