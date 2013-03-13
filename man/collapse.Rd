\name{collapse}
\alias{collapse}
\title{Reduces a vector to a frequency vector}
\description{This function reduces a vector to a frequency vector.}
\usage{collapse(D,pos=FALSE)}
\arguments{
  \item{D}{Vector}
  \item{pos}{Optional: position of categories}
}
\details{This function reduces a vector to a frequency vector. This function is similar to the way \code{table} summarizes vectors, but this function can deal with categories of frequency 0 (if the argument \code{pos} is specified). Here we assume a vector with an entry for each individual (sorted in any way). Each entry states the position of an individual. When the number of positions is naturally limited, such as when categorical positions are used, frequency vectors can summarize this information: how many individuals have position 1, how many individuals have position 2, etc. A frequency vector has an entry for each position in the population (sorted in ascending order). Each entry states the number of individuals in the population with this position.

The argument \code{pos} is required if certain positions do not occur in the population. For example, if we have positions on a 7-point scale, and position 3 never occurs in the population, the argument \code{pos} must be specified. In this case, our argument may be \code{pos=1:7}. We can also use categories more generally, as in \code{c(-3,-1,0,.5,1,2,5)}.  Specifying the positions of categories when all positions actually occur in the population has no side-effects.}

\value{A frequency vector}
\author{Didier Ruedin}
\seealso{\code{\link{expand}}}
