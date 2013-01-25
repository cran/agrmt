\name{patternVector}
\alias{patternVector}
\title{Creates pattern vector}
\description{Helper function to create a pattern vector from a frequency vector.}
\usage{patternVector(V)}
\arguments{\item{V}{A frequency vector}}
\details{This is a helper function to create a pattern vector from a frequency vector. A pattern vector reduced all values greater or equal to 1 to 1, and values of 0 remain 0. A frequency vector (0,0,18,59,0,34,2) is turned into a pattern vector (0,0,1,1,0,1,1).}
\value{The function returns a pattern vector.}
\author{Didier Ruedin}
\seealso{\code{\link{agreement}}}
