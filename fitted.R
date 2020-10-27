# What does fitted do?
library(tidyverse)

beta <- .2
d <- tibble(
  x = rnorm(1000),
  y = x * beta + rnorm(1000, .1)
)

d <- d %>% mutate(
  y_hat = fitted(lm(y ~ x, data = d)),
  y_err = y - y_hat
)

d

ggplot(d, aes(x = x, y = y)) + geom_point()
ggplot(d, aes(x = x, y = y_hat)) + geom_point()
ggplot(d, aes(x = x, y = y_err)) + geom_point()
