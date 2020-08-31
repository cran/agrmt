\name{agrmt-package}
\alias{agrmt-package}
\alias{agrmt}
\docType{package}
\title{Calculate Concentration and Dispersion in Ordered Rating Scales}
\description{This package calculates concentration and dispersion in ordered rating scales. It implements various measures of concentration and dispersion to describe what researchers variably call agreement, concentration, consensus, dispersion, or polarization among respondents in ordered data. It also implements other related measures to classify distributions.}
\details{In ordered rating scales, concentration occurs if many values on the scale cluster around one value. Dispersion describes the absense of concentration. Various measures exist to calculate concentration and dispersion.

The package provides a generic city-block based (\code{\link{concentration}}) measure, and a generic measure of dispersion (\code{\link{disper}}). To use Van der Eijk's (2001) algorithmic approach agreement "A", call \code{\link{agreement}}. The derived \code{\link{polarization}} lets you calculate a polarization score based on agreement A. Values are inverted and standardized to [0, 1]. Other specific measures: Leik's measure of ordinal dispersion (\code{\link{Leik}}), Tatsle and Wierman's (\code{\link{consensus}}), Blair and Lacy's (\code{\link{dsquared}}, \code{\link{lsquared}}, and \code{\link{BlairLacy}}), the measure by Kvalseth (\code{\link{Kvalseth}}), Berry and Mielke's IOV (\code{\link{BerryMielke}}), Reardon (\code{\link{Reardon}}) or Garcia-Montalvo and Reynal-Querol's (\code{\link{MRQ}}).

The package also includes functions to classify distributions according to Galtung's (1969) AJUS-system (\code{\link{ajus}}), and changes over time according to Galtung's (1969) ISD-system (\code{\link{isd}}). Moreover, the function \code{\link{modes}} can identify the position of multiple modes.}
\author{Didier Ruedin

Contributor: Clem Aeppli

Maintainer: Didier Ruedin <didier.ruedin@unine.ch>
}
\references{van der Eijk, C. (2001) Measuring agreement in ordered rating scales, Quality and Quantity 35(3):325-341.

Galtung, J. (1969) Theory and Methods of Social Research. Oslo: Universitetsforlaget.}
\keyword{ package }
