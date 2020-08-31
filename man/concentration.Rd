\name{concentration}
\alias{concentration}
\title{Measures concentration}
\description{This function measures concentration in a frequency vector}
\usage{concentration(V, metric=2)}
\arguments{
  \item{V}{Vector}
  \item{metric}{city block metric}
}
\details{This function measures concentration in a frequency vector in city blocks. The default metric is 2.}

\value{measure of concentration}
\author{Clem Aeppli}
\examples{
# Sample data
V <- c(30,40,210,130,530,50,10)
concentration(V)
}
