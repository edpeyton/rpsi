



#' @title Calculate PSI metric
#' @description Calculates the Population Stability Index (PSI) metric with thresholds calculated from Yurdakul, Bilal (2018).\loadmathjax 
#' @param x A data frame containing the distinct groups and counts of the base distribution. Must contain columns \code{var} and \code{count}.
#' @param y The comparison distribution in the same format as \code{x}.
#' @param var The name of the column in \code{x} and \code{y} representing the discrete groups of the distributions.
#' @param count The name of the column in \code{x} and \code{y} representing the count of observations in each group.
#' @param date (Optional) The name of a column in \code{y} representing a date variable. The column \code{date} in \code{y} should be of class "Date". This is useful if making multiple
#' comparisons between \code{y} and \code{x} over time.
#' @param random_base A logical value indicating whether the base distribution used for comparison is considered random or if it is considered a population.
#' @examples
#' library(rpsi)
#' library(dplyr)
#' x = data.frame(x = rnorm(500, 0.1)) %>% mutate(VAL  = factor(ifelse(x< -2,"A", ifelse(x<0, "B", ifelse(x>2, "D", "C"))))) %>% group_by(VAL) %>% summarise(N = n())
#' y = data.frame(x = rnorm(500, 0.1)) %>% mutate(VAL  = factor(ifelse(x< -2,"A", ifelse(x<0, "B", ifelse(x>2, "D", "C"))))) %>% group_by(VAL) %>% summarise(N = n())
#' psi(x, y, var = "VAL", count = "N")
#' 
#' #Example over time
#' p = c(0.1, 0.2, 0.5, 0.05, 0.05, 0.1)
#' N = 10000
#' 
#' x = data.frame(TYPE = factor(LETTERS[1:length(p)]), VALUE = N*p)
#' y = sapply(seq.Date(as.Date("2010-01-01"), as.Date("2019-12-01"), "month"), function(i) {
#'   data.frame(TYPE = factor(sample(LETTERS[1:length(p)], 100, replace = TRUE, prob = p))) %>% mutate(DATE = i)
#' }, simplify = FALSE) %>% bind_rows() %>% group_by(DATE, TYPE, .drop = FALSE) %>% summarise(VALUE = n(), .groups = "keep") %>% ungroup()
#' res = psi(x, y, var = "TYPE", count = "VALUE", date = "DATE")
#' plot(res, crit_vals = 0.95)
#' 
#' @details See \href{https://scholarworks.wmich.edu/cgi/viewcontent.cgi?article=4249&context=dissertations}{Yurdakul, Bilal (2018)} for details.
#' 
#' The PSI is shown to be distributed as
#' 
#' \mjdeqn{\text{PSI}\overset{\cdot}{\sim}\chi^{2}_{B-1}\cdot(1/M + 1/N)}{ascii}
#' 
#' where \mjeqn{B}{ascii} is the number of discrete values of the distribution and \mjeqn{M}{ascii}, \mjeqn{N}{ascii} are the sizes of the comparison and base distributions, respectively.
#' @return An object of class \code{rpsi}. See \link[rpsi]{rpsi} for details.
#' @export
psi = function(x, y, var, count, date = NULL, random_base = TRUE) {
  
  stopifnot(
    "'var' must be a column in 'x' and 'y'." = var %in% names(x) & var %in% names(y),
    "'count' must be a column in 'x' and 'y'." = count %in% names(x) & count %in% names(y),
    "'date' must be a column in 'y'." = is.null(date) || date %in% names(y),
    "Input 'x' and 'y' must be data frames." = is.data.frame(x) & is.data.frame(y),
    "Inputs 'x' and 'y' must contain column 'var'." = var %in% names(x) & var %in% names(y),
    "Column 'date' in 'y' must be of class 'Date'." = (is.null(date) | !is.null(date) && "Date" %in% class(y[[date]])),
    "Column 'var' in 'x' must be a factor." = is.factor(x[[var]]),
    "Column 'var' in 'y' must be a factor." = is.factor(x[[var]]),
    "Factor levels of 'var' in 'x' and 'y' must be identical." = identical(levels(x[[var]]), levels(y[[var]]))
  )
  
  if (!is.null(date)) {
    
    y = y %>% dplyr::group_by(!!!rlang::syms(c(date, var)), .drop = FALSE) %>% dplyr::summarise(!!count:=sum(!!rlang::sym(count)), .groups = "keep")
    
    res = sapply(unique(y[[date]]), function(i, x, y, var, count, random_base) {
      
      res = psi(x, 
                y = y %>% dplyr::filter(!!rlang::sym(date)==i), 
                var = var, 
                count = count, 
                date = NULL,
                random_base = random_base)
      
      return(res$data %>% dplyr::mutate(!!date:=i))
      
    }, x = x, y = y, var = var, count = count, random_base = random_base, simplify = FALSE) %>% dplyr::bind_rows()
    
    if (random_base) {
      N = sum(x[[count]])
    } else {
      N = Inf
    }
    
    M = res %>% dplyr::group_by(!!rlang::sym(date), .drop = FALSE) %>% dplyr::summarise(N_Y = sum(N_Y), .groups = "keep") %>% dplyr::ungroup()
    B = nlevels(x[[var]])
    
    psi = res %>% dplyr::group_by(!!rlang::sym(date), .drop = FALSE) %>% dplyr::summarise(PSI = sum(PSI), .groups = "keep") %>% dplyr::ungroup()
    p.val = psi %>% dplyr::mutate(p.val = stats::pchisq(psi$PSI/(1/N + 1/M$N_Y), B - 1, lower.tail = FALSE))
    
    res = list(psi = psi,
               p.val = p.val,
               data = res,
               B = B,
               N = N,
               M = M,
               var = var,
               date = date)
    
    class(res) = "rpsi"
    return(res)
    
  }
  
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
             var = var,
             date = date)
  
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
  cat(paste0("M = ", ifelse(is.data.frame(x$M), "Many values", scales::comma(x$M)), "\n"))
  cat(paste0("B = ", scales::comma(x$B), "\n"))
  cat(paste0("PSI = ", ifelse(is.data.frame(x$psi), "Many values", scales::scientific(x$psi, accuracy = accuracy)), "\n"))
  cat(paste0("p-value = ", ifelse(is.data.frame(x$psi), "Many values", scales::scientific(x$p.val, accuracy = accuracy)), "\n"))
  if (!is.data.frame(x$M)) {
    sapply(crit_vals, function(i, x) {cat(paste0(scales::percent(i), " threshold = ", scales::scientific(stats::qchisq(i,  x$B - 1)*(1/x$N + 1/x$M), accuracy = accuracy), "\n"))}, x = x)
  }
  cat("\n")
  
}



#' @title Plot rpsi object
#' @description This function 
#' @param x An object of class \code{pkgdepR}.
#' @param width The width of the vis.js render.
#' @param height The height of the vis.js render.
#' @param main The title. To remove the title, pass \code{list(text = NULL)}.
#' @param submain The subtitle. To remove the subtitle, pass \code{list(text = NULL)}.
#' @param alpha A transparency value to use for colors. Must be between 0 and 1.
#' @param ... 
#' @examples
#' @return A list of object of classes \code{gg} and \code{ggplot}.
#' @export
plot.rpsi = function(x, crit_vals = c(0.99), fill.col = "blues") {
  
  crit_vals = crit_vals[1]
  
  stopifnot("Input 'fill.col' must be one of 'blues', 'reds', 'greens'." = (fill.col %in% rpsi_cols()))
  
  if (!is.null(x$date)) {
    CV = x$p.val %>% dplyr::mutate(CRIT = qchisq(crit_vals, x$B - 1)*(1/x$N + 1/x$M$N_Y))
    
    g = list()
    date = x$date
    var = x$var
    
    g[[1]] = ggplot2::ggplot(data = CV) +
      ggplot2::geom_line(ggplot2::aes(x = !!rlang::sym(x$date), y = PSI), color = "blue", size = 0.8) +
      ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(x$date), y = PSI), color = "blue") +
      sapply(unique(CV[[date]]), function(i, x) {ggplot2::geom_line(data = data.frame(DATE = c(CV[[date]][match(i, CV[[date]])-1], i, CV[[date]][match(i, CV[[date]])+1]), y = CV$CRIT[match(i, CV[[date]])]), ggplot2::aes(x = DATE, y = y), linetype = "dashed")}, x = x) +
      ggplot2::geom_text(data = data.frame(x = max(CV[[date]]), 
                                           y = CV$CRIT[match(max(CV[[date]]), CV[[date]])], 
                                           label = paste0(scales::percent(crit_vals), " threshold")), 
                         ggplot2::aes(x = x, y = y, label = label, vjust = 1, hjust = 1)) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Population stability index",
                    subtitle = "Over time")
    
    g[[2]] = ggplot2::ggplot(data = CV %>% dplyr::mutate(FILL = factor(ifelse(p.val<1-crit_vals, 0, 1)))) +
      ggplot2::geom_point(ggplot2::aes(x = !!rlang::sym(x$date), y = p.val, color = FILL), show.legend = FALSE) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 1 - crit_vals), linetype = "dashed") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = "solid") +
      ggplot2::geom_text(data = data.frame(x = max(CV[[date]]), 
                                           y = 1-crit_vals, 
                                           label = paste0(scales::percent(1 - crit_vals))), 
                         ggplot2::aes(x = x, y = y, label = label, vjust = -1, hjust = 1)) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "p-value",
                    title = "p-values",
                    subtitle = "Over time") +
      ggplot2::scale_color_manual(name = var, values = c("red", "blue"))
    
    g[[3]] = 
      ggplot2::ggplot(data = rbind(x$data %>% dplyr::mutate(POP = "Comparison"), 
                                   x$data %>% dplyr::mutate(POP = "Base") %>% dplyr::mutate(PROP_Y = PROP_X)), 
                      ggplot2::aes(x = !!rlang::sym(date), y = PROP_Y, fill = !!rlang::sym(var))) +
      ggplot2::geom_area() +
      ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0)) +
      ggplot2::scale_x_date(expand = c(0, 0)) +
      ggplot2::labs(y = NULL,
                    title = "Distributions",
                    subtitle = "Over time") +
      ggplot2::facet_wrap(. ~ POP) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank()) +
      ggplot2::scale_fill_manual(values = rpsi_pal(fill.col)(nlevels(x$data[[var]])))
    
    return(invisible(g))
    
  } else {
    CV = qchisq(crit_vals, x$B - 1)*(1/x$N + 1/x$M)
  }

  
  
  ggplot2::ggplot(data = data.frame(x = 1, y = x$psi, yintercept = CV)) +
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


# p = c(0.1, 0.2, 0.5, 0.05, 0.05, 0.1)
# N = 10000
# 
# x = data.frame(TYPE = factor(LETTERS[1:length(p)]), VALUE = N*p)
# y = sapply(seq.Date(as.Date("2010-01-01"), as.Date("2019-12-01"), "month"), function(i) {
#   data.frame(TYPE = factor(sample(LETTERS[1:length(p)], 100, replace = TRUE, prob = p))) %>% mutate(DATE = i)
#   }, simplify = FALSE) %>% bind_rows() %>% group_by(DATE, TYPE, .drop = FALSE) %>% summarise(VALUE = n(), .groups = "keep") %>% ungroup()



#' @export
rpsi_palettes = function() {
  
  list(
    blues = c("#002F6C", "#376EE2", "#84DCE0"),
    reds = c("#002F6C", "#376EE2", "#84DCE0")
  )
  
}

#' @export
rpsi_pal = function(palette = "blues", reverse = FALSE, ...) {
  
  pal = rpsi_palettes()[[palette]]
  
  if (reverse) {
    pal = rev(pal)
  }
  
  return(colorRampPalette(pal, ...))
  
}

#' @export
rpsi_cols = function() {
  
  return(c("blues", "reds", "greens"))
  
}


