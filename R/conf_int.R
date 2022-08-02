




#' @title Get formatted method names
#'
#' @param x Method name from \code{method} argument of \link[DescTools]{MultinomCI}.
#'
#' @return A formatted string.
#' @export
method_names = function(x) {
  
  names = c("sisonglaz" = "Sison & Glaz", "cplus1" = "", "goodman" = "Goodman", "wald" = "Wald", "waldcc" = "Wald (continuity correction)", "wilson" = "Wilson")
  names[[x]]
  
}





#' @title Get the CI for a Multinomial distribution
#' @description A wrapper for \link[DescTools]{MultinomCI} for two-sided confidence intervals of the Multinomial distribution.
#' @inheritParams DescTools::MultinomCI
#'
#' @return A data frame with confidence intervals calculated.
#' @export
get_ci = function(x, method = "sisonglaz", conf.level = 0.95) {
  
  CHK1 = CHK2 = LINE = LWR = UPR = label = PROP_X = N_Y = NULL
  
  method = match.arg(arg = method, choices = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson"))
  
  if ("rpsi_time" %in% class(x)) {
    
    ts = x$data %>% 
      dplyr::filter(N_Y>0) %>%
      dplyr::group_by(!!rlang::sym(x$date)) %>% 
      dplyr::mutate(LWR = DescTools::MultinomCI(N_Y, conf.level = conf.level, method = method, sides = "two.sided")[, 2], 
                    UPR = DescTools::MultinomCI(N_Y, conf.level = conf.level, method = method, sides = "two.sided")[, 3]) %>%
      dplyr::mutate(CHK1 = factor(ifelse(LWR<=PROP_X, "Within interval", "Breach"), levels = c("Within interval", "Breach")),
                    CHK2 = factor(ifelse(UPR>=PROP_X, "Within interval", "Breach"), levels = c("Within interval", "Breach")),
                    LINE = "Estimate (base)") %>%
      dplyr::ungroup()
    
  } else {
    
    ts = x$data %>% 
      dplyr::mutate(LWR = DescTools::MultinomCI(N_Y, conf.level = conf.level, method = method, sides = "two.sided")[, 2], 
                    UPR = DescTools::MultinomCI(N_Y, conf.level = conf.level, method = method, sides = "two.sided")[, 3]) %>%
      dplyr::mutate(CHK1 = factor(ifelse(LWR<=PROP_X, "Within interval", "Breach"), levels = c("Within interval", "Breach")),
                    CHK2 = factor(ifelse(UPR>=PROP_X, "Within interval", "Breach"), levels = c("Within interval", "Breach")),
                    LINE = "Estimate (base)")
    
  }

  return(list(ts = ts,
              method = method))
  
}











