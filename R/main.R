



#' @title Calculate PSI metric
#' @description Calculates the PSI metric with thresholds calculated from Yurdakul, Bilal (2018).
#' @param x A data frame containing the distinct
#' @param y 
#' @param var 
#' @param random_base 
#'
#' @return
#' @export
#'
#' @examples
psi = function(x, y, var, random_base = TRUE) {
  
  stopifnot(
    "Input 'x' and 'y' must be data frames." = is.data.frame(x) & is.data.frame(y),
    "Inputs 'x' and 'y' must contain column 'var'." = var %in% names(x) & var %in% names(y),
    "Column 'var' in 'x' must be a factor." = is.factor(x[[var]]),
    "Column 'var' in 'y' must be a factor." = is.factor(x[[var]]),
    "Factor levels of 'var' in 'x' and 'y' must be identical." = identical(levels(x[[var]]), levels(y[[var]]))
  )
  
  PSI = x %>% 
    dplyr::group_by(TYPE, .drop = FALSE) %>% 
    dplyr::summarise(N_X = sum(VALUE), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(PROP_X = N_X/sum(N_X))
  
  PSI2 = y %>% 
    dplyr::group_by(TYPE, .drop = FALSE) %>% 
    dplyr::summarise(N_Y = sum(VALUE), .groups = "keep") %>%
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
  p.val = pchisq(psi/(1/N + 1/M), B - 1, lower.tail = FALSE)
  
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

print.rpsi = function(x, crit_vals = c(0.95), ...) {
  
  
  
  cat("\n")
  cat(paste0("rpsi object\n"))
  cat(paste0("-----------\n"))
  cat(paste0("Unique values: ", paste0(levels(x$data[[x$var]]), collapse = ", "), "\n"))
  cat(paste0("N = ", scales::comma(x$N), "\n"))
  cat(paste0("M = ", scales::comma(x$M), "\n"))
  cat(paste0("B = ", scales::comma(x$B), "\n"))
  cat(paste0("PSI = ", scales::number(x$psi), "\n"))
  sapply(crit_vals, function(i, x) {cat(paste0("PSI = ", scales::number(qchisq(i,  x$B - 1)*(1/x$N + 1/x$M)), "\n"))}, x = x)
  
  
}




plot.rpsi = function(x, crit_vals = c(0.95)) {
  
  CV = qchisq(crit_vals, res$B - 1)*(1/res$N + 1/res$M)
  
  ggplot2::ggplot(data = data.frame(res$))
  
  
}







x = data.frame(TIME = seq.Date(as.Date("2010-01-01"), as.Date("2019-12-01"), "month")) %>% 
  mutate(A = sapply(1:120, function(i) {round(runif(1, 0, 1000), 0)}), 
         B = sapply(1:120, function(i) {round(runif(1, 0, 1000), 0)})) %>% 
  tidyr::pivot_longer(cols = c("A", "B"), names_to = "TYPE", values_to = "VALUE")

index = "TIME"
var = "TYPE"

x = data.frame(POP = sapply(1:120, function(i) {rbinom(1, 1, 0.3)})) %>% 
  mutate(A = sapply(1:120, function(i) {round(runif(1, 0, 1000), 0)}), 
         B = sapply(1:120, function(i) {round(runif(1, 0, 1000), 0)})) %>% 
  tidyr::pivot_longer(cols = c("A", "B"), names_to = "TYPE", values_to = "VALUE") %>%
  mutate(TYPE = factor(TYPE))

y = data.frame(POP = sapply(1:120, function(i) {rbinom(1, 1, 0.3)})) %>% 
  mutate(A = sapply(1:120, function(i) {round(runif(1, 0, 1000), 0)}), 
         B = sapply(1:120, function(i) {round(runif(1, 0, 1000), 0)})) %>% 
  tidyr::pivot_longer(cols = c("A", "B"), names_to = "TYPE", values_to = "VALUE") %>%
  mutate(TYPE = factor(TYPE))
