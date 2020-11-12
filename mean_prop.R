# Some nice stats by variable type

# d_wide from ./correlations.R

# mean/proportion, CI, SE

#' Calculate mean or proportion, plus 95% confidence intervals for a variable. 
#' Use appropriate child function depending upon whether \code{x} is binomial or 
#' continuous.
#' @param x vector to extract stats for
#' @param islogicalFun function to run on x to determine if x is logical
doStats <- function(x, islogicalFun = function(x) {all(x == 1 | x == 0)}) {
  out <- NULL
  
  if (islogicalFun(x)) {
    test <- prop.test(sum(x), length(x))
    out <- tibble(
      type = 'logical',
      summary = test$estimate,
      SE = sqrt(summary * (1 - summary) / length(x)),
      ci_low = test$conf.int[1],
      ci_high = test$conf.int[2]
    )    
  } else {
    ci <- mean_cl_normal(x)
    se <- mean_se(x)
    out <- tibble(
      type = 'numeric',
      summary = ci$y,
      SE = se$y - se$ymin,
      ci_low = ci$ymin,
      ci_high = ci$ymax
    )
  }
  out
}

d_wide %>%
  pivot_longer(cols = -id) %>%
  nest(d = -name) %>%
  mutate(d = map(d, ~doStats(.$value))) %>%
  unnest(cols = d)
  
