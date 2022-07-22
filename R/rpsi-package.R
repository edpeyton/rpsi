
#' @details
#' The only function you're likely to need from rpsi is \link[rpsi]{psi}, which returns an object of class \code{rpsi}.
#' 
#' An object of class \code{rpsi} is a list with the following named objects:
##' \itemize{
##'  \item{\strong{\code{psi}}}{: the PSI value.}
##'  \item{\strong{\code{p.val}}}{: the p-value where the null distribution is that the two samples are from the same population.}
##'  \item{\strong{\code{data}}}{: a data frame of the calculations.}
##'  \item{\strong{\code{M}}}{: the comparison distribution sample size.}
##'  \item{\strong{\code{N}}}{: the base distribution sample size.}
##'  \item{\strong{\code{var}}}{: the name of the column in \code{x} in the call to \code{\link[rpsi:psi]{psi(x, y, ...)}} representing the discrete groups of the distributions.}
##'  \item{\strong{\code{date}}}{: the name of the column in \code{y} in the call to \code{\link[rpsi:psi]{psi(x, y, ...)}} representing the date variable (if any).}
##' }
#' @keywords internal
"_PACKAGE"
