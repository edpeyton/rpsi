



#' @title Calculate PSI metric
#' @description Calculates the PSI metric with thresholds calculated from Yurdakul, Bilal (2018). \loadmathjax
#' @param x A data frame containing the distinct groups and counts of the base distribution. Must contain columns \code{var} and \code{count}.
#' @param y The comparison distribution in the same format as \code{x}.
#' @param var The name of the column in \code{x} and \code{y} representing the discrete groups of the distributions.
#' @param count The name of the column in \code{x} and \code{y} representing the count of observations in each group.
#' @param random_base A logical value indicating whether the base distribution uesd for comparison is considered random or if it is considered a population.
#' @examples
#' library(rpsi)
#' library(dplyr)
#' x = data.frame(x = rnorm(500, 0.1)) %>% mutate(VAL  = factor(ifelse(x< -2,"A", ifelse(x<0, "B", ifelse(x>2, "D", "C"))))) %>% group_by(VAL) %>% summarise(N = n())
#' y = data.frame(x = rnorm(500, 0.1)) %>% mutate(VAL  = factor(ifelse(x< -2,"A", ifelse(x<0, "B", ifelse(x>2, "D", "C"))))) %>% group_by(VAL) %>% summarise(N = n())
#' psi(x, y, var = "VAL", count = "N")
#' @details See \href{https://scholarworks.wmich.edu/cgi/viewcontent.cgi?article=4249&context=dissertations}{Yurdakul, Bilal (2018)} for details.
#' 
#' The PSI is shown to be approximately distributed as
#' \deqn{PSI\sim\chi^{2}}
#' @return An object of class \code{rpsi}. See \link[rpsi]{rpsi} for details.
#' @export
psi = function(x, y, var, count, random_base = TRUE) {
  
  stopifnot(
    "Input 'x' and 'y' must be data frames." = is.data.frame(x) & is.data.frame(y),
    "Inputs 'x' and 'y' must contain column 'var'." = var %in% names(x) & var %in% names(y),
    "Column 'var' in 'x' must be a factor." = is.factor(x[[var]]),
    "Column 'var' in 'y' must be a factor." = is.factor(x[[var]]),
    "Factor levels of 'var' in 'x' and 'y' must be identical." = identical(levels(x[[var]]), levels(y[[var]]))
  )
  
  PSI = x %>% 
    dplyr::group_by(!!rlang::sym(var), .drop = FALSE) %>% 
    dplyr::summarise(N_X = sum(!!rlang::sym(count)), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PROP_X = N_X/sum(N_X))
  
  PSI2 = y %>% 
    dplyr::group_by(!!rlang::sym(var), .drop = FALSE) %>% 
    dplyr::summarise(N_Y = sum(!!rlang::sym(count)), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PROP_Y = N_Y/sum(N_Y))
  
  PSI = PSI %>% 
    dplyr::left_join(PSI2, by = var) %>%
    dplyr::mutate(STAT = (PROP_Y - PROP_X)^2/PROP_X,
                  PSI = (PROP_Y - PROP_X)*log(PROP_Y/PROP_X))
  
  if (random_base) {
    N = sum(PSI[["N_X"]])
  } else {
    N = Inf
  }
  
  M = sum(PSI[["N_Y"]])
  B = nlevels(x[[var]])
  
  psi = sum(PSI[["PSI"]])
  p.val = stats::pchisq(psi/(1/N + 1/M), B - 1, lower.tail = FALSE)
  
  res = list(psi = psi,
             p.val = p.val,
             data = PSI,
             B = B,
             N = N,
             M = M,
             var = var)
  
  class(res) = "rpsi"
  
  return(res)
  
} 

#' @title Print rpsi object
#' @param x An object of class \code{rpsi}.
#' @param ... Redundant argument for consistency with method.
#' @return No return value.
#' @export
print.rpsi = function(x, crit_vals = c(0.95, 0.99), accuracy = 0.001, ...) {
  
  cat("\n")
  cat(paste0("rpsi object\n"))
  cat(paste0("-----------\n"))
  cat(paste0("Unique values: ", paste0(levels(x$data[[x$var]]), collapse = ", "), "\n"))
  cat(paste0("N = ", scales::comma(x$N), "\n"))
  cat(paste0("M = ", scales::comma(x$M), "\n"))
  cat(paste0("B = ", scales::comma(x$B), "\n"))
  cat(paste0("PSI = ", scales::scientific(x$psi, accuracy = accuracy), "\n"))
  cat(paste0("p-value = ", scales::scientific(x$p.val, accuracy = accuracy), "\n"))
  sapply(crit_vals, function(i, x) {cat(paste0(scales::percent(i), " threshold = ", scales::scientific(stats::qchisq(i,  x$B - 1)*(1/x$N + 1/x$M), accuracy = accuracy), "\n"))}, x = x)
  cat("\n")
  
}



#' @title Plot rpsi object
#' @param x An object of class \code{rpsi}.
#' @param ... Redundant argument for consistency with method.
#' @return No return value.
#' @export
plot.rpsi = function(x, crit_vals = c(0.95, 0.99)) {
  
  CV = qchisq(crit_vals, res$B - 1)*(1/res$N + 1/res$M)
  
  ggplot2::ggplot(data = data.frame(x = 1, y = res$psi, yintercept = CV)) +
    ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = yintercept), linetype = "dashed") +
    ggplot2::expand_limits(y = 0)
  
  
}


#' @title rpsi object
#' @description An internal generic function. Methods for \code{rpsi} should only return \code{TRUE} if the class is \code{rpsi}.
#' @param x Object to be tested.
#'
#' @return Boolean. \code{TRUE} when \code{x} is of class \code{rpsi}.
#' @export
is.rpsi = function(x) {
  
  inherits(x, "rpsi")
  
}



#' @title Summarise rpsi object
#' @param object An object of class \code{rpsi}.
#' @param ... Redundant argument for consistency with method.
#' @return No return value.
#' @export
summary.rpsi = function(object, crit_vals = c(0.95, 0.99), ...) {
  
  print.rpsi(x = object, crit_vals = crit_vals, ...)
  
}

