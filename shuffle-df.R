# Order the example data so we can tell if it's shuffled
d_ordered <- apply(d_miss, 2, function(x) x[order(x)]) %>% as_tibble()
# Shuffle the example data
d_shuffled <- apply(d_miss, 2, function(x) sample(x)) %>% as_tibble()

left_join(d_ordered, d_shuffled, by = 'id') %>% .[, order(colnames(.))]

# Multiple shuffles
d_super_suffled <- d_ordered
n_shuffles <- 70

for (s in 1:n_shuffles) 
  d_super_suffled <- apply(d_super_suffled, 2, function(x) sample(x))

d_super_suffled <- as_tibble(d_super_suffled) %>%
  arrange(id = d_miss$id)  # restore the id field for fun
