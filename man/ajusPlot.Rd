\name{ajusPlot}
\alias{ajusPlot}
\title{Plot vector with AJUS type}
\description{Plot a frequency vector among with its AJUS type.}
\usage{ajusPlot(V, tolerance=0.1, variant="modified", ...)}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional). Differences smaller than or equal to the tolerance are ignored.}
  \item{...}{Arguments to pass to the plotting function}
  \item{variant}{Strict AJUS following Galtung, or modified to include F and L types (default)}
}
\details{This function plots the frequency vector along with its AJUS classification and skew. See \code{\link{ajus}} for a description of the AJUS system and the different parameters. In contrast to the \code{ajus} function, \code{ajusPlot} can deal with missing values (they are removed when calculating the AJUS type, but considered in the plot). This makes \code{ajusPlot} useful for classifying time series. Additional arguments can be passed to the to the underlying \code{plot} function.}
\author{Didier Ruedin}
\seealso{\code{\link{ajus}}}
