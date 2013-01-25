\name{compareValues}
\alias{compareValues}
\title{Compares two values}
\description{This is a helper function to compare two values.}
\usage{compareValues(A,B,tolerance=0.1)}
\arguments{
\item{A}{A number}
\item{B}{A number}
\item{tolerance}{Specify how similar values have to be to be treated as different. Differences smaller than or equal to the tolerance are ignored.}
}
\details{This is a helper function compare two values. Two values are more or less the same, or one of the two is bigger.}
\value{The function returns number to describe the relationship: -1 if A is bigger, 1 if B is bigger, and 0 if the two are more or less the same.}
\author{Didier Ruedin}
