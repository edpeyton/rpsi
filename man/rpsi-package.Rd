% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rpsi-package.R
\docType{package}
\name{rpsi-package}
\alias{rpsi}
\alias{rpsi-package}
\title{rpsi: Inference using the Population Stability Index}
\description{
Calculation of the population stability index (commonly used in credit risk) and the evaluation of meaningful thresholds for inferring stability. Based on "Statistical properties of the population stability index", Yurdakul & Naranjo (2020) <doi:10.21314/JRMV.2020.227>.
}
\details{
The only function you're likely to need from rpsi is \link[rpsi]{psi}, which returns an object of class \code{rpsi}.

An object of class \code{rpsi} is a list with the following named objects:
\itemize{
 \item{\strong{\code{psi}}}{: the PSI value.}
 \item{\strong{\code{p.val}}}{: the p-value where the null distribution is that the two samples are from the same population.}
 \item{\strong{\code{data}}}{: a data frame of the calculations.}
 \item{\strong{\code{M}}}{: the comparison distribution sample size.}
 \item{\strong{\code{N}}}{: the base distribution sample size.}
 \item{\strong{\code{var}}}{: the name of the column in \code{x} in the call to \code{\link[rpsi:psi]{psi(x, y, ...)}} representing the discrete groups of the distributions.}
 \item{\strong{\code{date}}}{: the name of the column in \code{y} in the call to \code{\link[rpsi:psi]{psi(x, y, ...)}} representing the date variable (if any).}
}
}
\seealso{
Useful links:
\itemize{
  \item Report bugs at \url{https://github.com/edpeyton/rpsi/issues}
}

}
\author{
\strong{Maintainer}: Ed Peyton \email{edppeyton@gmail.com} (\href{https://orcid.org/0000-0002-1427-0306}{ORCID})

}
\keyword{internal}
