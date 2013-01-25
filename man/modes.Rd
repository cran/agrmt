\name{modes}
\alias{modes}
\title{Identify multiple modes}
\description{Identifies (multiple) modes in a frequency vector.}
\usage{modes(V, pos=FALSE, tolerance=0.1)}
\arguments{
  \item{V}{A frequency vector}
  \item{pos}{Categories of frequency vector (optional)}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional).  Differences smaller than or equal to the tolerance are ignored.}
}
\details{This function identifies which positions of a frequency vector correspond to the mode. If there are multiple modes of the same value, all matching positions will be reported. Use the function \code{\link{collapse}} to create frequency vectors if necessary.}
\value{The function returns a list. The \code{at} returns the categories of the frequency vector. Either these categories were specified using the argument pos, or we assume it to be 1:k (with k the number of categories in the frequency vector). If the length of the \code{pos} argument does not match the length of the frequency vector, a warning is shown, and the \code{pos} argument is ignored. The \code{frequencies} returns the frequency vector. The \code{mode} returns the value of the mode(s). If there are multiple modes, they are listed. Similar frequencies are counted as equal, using the tolerance argument. To prevent similar frequencies to be considered the same, set \code{tolerance} to 0. The \code{positions} returns the positions of the vector that correspond to the mode. This will differ from the \code{mode} if \code{pos} is provided.The \code{contiguous} returns TRUE of all modes are contiguous, and FALSE if there are different values in between. If there is only one mode, it is defined as contiguous (i.e. TRUE).}
\author{Didier Ruedin}
\seealso{\code{\link{secondModes}}}
\examples{
# Example 1: finding the mode
V1 <- c(30,40,210,130,530,50,10)
modes(V1) # will find position 5
# Example 2:
V2 <- c(3,0,4,1)
modes(V2) # will find position 3
# Example 3: providing categories
modes(V2,pos=-1:2) # will still find position 3, but give the value of 1 as mode
# Example 4: similar values
V3 <- c(30,40,500,130,530,50,10) 
modes(V3, tolerance=30) # will find positions 3 and 5 (500 and 530 are nearly the same)
}

