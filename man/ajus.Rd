\name{ajus}
\alias{ajus}
\title{Classify distributions}
\description{Classify distributions using the AJUS-system introduced by Galtung (1969).}
\usage{ajus(V, tolerance=0.1, variant="modified")}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional). Differences smaller than or equal to the tolerance are ignored.}
  \item{variant}{Strict AJUS following Galtung, or modified to include F and L types (default)}
}
\details{This function implements the AJUS-system introduced by Galtung (1969). The input is a frequency vector. Distributions are classified as A if they are unimodal with a peak in the centre, as J if they are unimodal with a peak at either end, as U if they are bimodal with a peak at both ends, and as S if they are multimodal. In addition to Galtung's classification, the function classifies distributions as F if there is no peak and all values are more or less the same (flat). Furthermore, a distinction is drawn between J and L distributions, depending on whether they increase or decrease: J types have a peak on the right, L types have the peak on the left. The skew is given as +1 for a positive skew, as 0 for no skew, and -1 for a negative skew.

The skew is identified by comparing the sum of values left and right of the midpoint respectively. For J-type of distributions, the skew is identified on the basis of the changes between values. This way long tails cannot influence the skew, and a single peak at the left and right-hand end can be differentiated in all cases.

The aim of the AJUS system is to reduce complexity. Initially the intuition was to classify distributions on an ad-hoc basis (i.e. eye-balling). Using an algorithm is certainly more reliable, and useful if one is interested in classifying (and comparing) a large number of distributions. The argument \code{tolerance}, however is not a trivial choice and can affect results. Use the helper function \code{ajusCheck} to check sensitivity to different values of the tolerance parameter.

You can choose between a strict AJUS classification and a modified AJUSFL classification (default). The AJUS classification does not include a type for distributions without peaks (F type), and NA is returned instead. The AJUS classification does not draw a distinction between unimodal distributions with a peak at the end: the skew needs to be considered to distinguish between increasing and decreasing cases. The modified variant (default) includes the F type and the L type along with the original AJUS types.}
\value{The function returns a list. The \code{type} returns a string corresponding to the pattern described by Galtung (A,J,U,S) or (F,L). The \code{skew} returns a number to describe the direction of the skew. The \code{pattern} returns the simplified pattern of the distribution. It indicates whether two values were considered the same (0), or if there was an increase (1) or decrease (-1) between two consecutive values. The length of the pattern is equal to the length of the frequency vector minus one.}
\references{Galtung, J. (1969) Theory and Methods of Social Research. Oslo: Universitetsforlaget.}
\author{Didier Ruedin}
\seealso{\code{\link{isd}}, \code{\link{ajusCheck}}, \code{\link{ajusPlot}}}
