\name{expand}
\alias{expand}
\title{Expands a frequency vector to a vector}
\description{This function expands a frequency vector to a vector.}
\usage{expand(F)}
\arguments{
  \item{F}{Frequency vector}
}
\details{This function takes a frequency vector and expands it to a longer vector with one entr for each observation. It is reverses the \code{\link{collapse}} function. A frequency vector has an entry for each position in the population. Each entry states the number of individuals in the population with this position. Here we create a vector with an entry for each individual.}

\value{A vector}
\author{Didier Ruedin}
\seealso{\code{\link{collapse}}}
