# Make some example data
library(tidyverse)
d <- tibble(id = 1:10000)

# Add some variables for dad/mum/child
for (p in c('dad', 'mum', 'child')) {
  for (i in 1:3) {
    # generate a variable name
    v <- paste0(p, '_00', i, collapse = "")
    
    # Make up some normal data
    d[, v] <- rnorm(nrow(d))
  }
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

# Add summaries for missingness at each time point
d_miss <- mutate(
  d_miss, 
  lost_at = case_when(
    is.na(dad_001) | is.na(dad_002) | is.na(dad_003) ~ 'dad',
    is.na(mum_001) | is.na(mum_002) | is.na(mum_003) ~ 'mum',
    is.na(child_001) | is.na(child_002) | is.na(child_003) ~ 'child',
    T ~ NA_character_
  ),
  lost_at = factor(lost_at, levels = c('dad', 'mum', 'child')),
  lost = !is.na(lost_at)
)

# Visualise loss trajectory
d_miss %>% 
  group_by(lost_at) %>%
  summarise(n = n(), lost = any(lost), .groups = 'drop') %>%
  ggplot(aes(x = lost_at, y = n, fill = lost)) +
  geom_col() +
  geom_text(aes(y = n + nrow(d_miss) / 20, label = n)) + 
  scale_y_continuous(limits = c(0, nrow(d_miss))) +
  labs(
    title = 'Loss point of data', 
    subtitle = 'Data lost at a point to the left are not included in rightward points'
  )

# Add a summary variable for any missing
d_miss$any <- apply(d_miss, 1, function(r) ifelse(any(is.na(r)), NA, F))

# Plot numbers of missing
d_miss %>%
  pivot_longer(cols = where(is.logical), names_to = "var") %>%
  mutate(missing = is.na(value)) %>%
  group_by(var) %>%
  summarise(missing = sum(missing), .groups = 'drop') %>%
  ggplot(aes(x = var, y = missing)) +
  geom_col() +
  geom_text(aes(y = missing + nrow(d) / 20, label = missing)) +
  scale_y_continuous(limits = c(0, nrow(d)))

