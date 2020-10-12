# Make some example data
library(tidyverse)
d <- tibble(id = 1:10000)

# Add some variables
for (i in 1:10) {
  # generate a random name
  L <- paste0(sample(letters, 3, replace = T), collapse = "")
  N <- paste0(sample(0:9, 3, replace = T, prob = 1/(1:10)), collapse = "")
  v <- paste0(L, '_', N, collapse = "")
  
  # Make up some normal data
  d[, v] <- rnorm(nrow(d))
}

# Delete some data at random
p_missing = .06

d_miss <- mutate(d, across(.cols = -id, function(x) {
  miss <- runif(nrow(d)) < p_missing
  x[miss] <- NA
  x
}))
  
# Visualise missing cases
d_miss %>%
  pivot_longer(cols = -id, names_to = "var") %>%
  mutate(missing = is.na(value)) %>%
  ggplot(aes(x = var, y = id, fill = missing)) +
  geom_tile(colour = NA)

# Add a summary variable for any missing
d_miss$any <- apply(d_miss, 1, function(r) ifelse(any(is.na(r)), NA, F))

# Plot numbers of missing
d_miss %>%
  pivot_longer(cols = -id, names_to = "var") %>%
  mutate(missing = is.na(value)) %>%
  group_by(var) %>%
  summarise(missing = sum(missing)) %>%
  ggplot(aes(x = var, y = missing)) +
  geom_col() +
  scale_y_continuous(limits = c(0, nrow(d)))