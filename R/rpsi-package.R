
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
##'  \item{\strong{\code{var}}}{: the name of the column in \code{x} in the call to \code{rpsi(x, ...)} representing the discrete groups of the distributions.}
##' }
#' @keywords internal
"_PACKAGE"
