# Correlations between logical and continuous variables
library(psych)

d_long <- d %>% 
  pivot_longer(
    cols = -id,
    names_to = c(".value", "timepoint"),
    names_pattern = "(.+)_(.+)"
  ) 

d_long <- d_long %>%
  mutate(
    mum = mum < 0
  ) %>% 
  pivot_wider(names_from = timepoint, values_from = dad:child)

doCor <- function(var1, var2, data) {
  a <- pull(data, var1)
  b <- pull(data, var2)
  
  out <- NULL
  
  if (typeof(a) == 'logical' && typeof(b) == 'logical') {
    out <- phi(table(a, b))
  } else {
    out <- cor(a, b, method = 'pearson')
  }
  
  out
}

getTypes <- function(var1, var2, data) {
  types <- c(
    typeof(pull(data, var1)),
    typeof(pull(data, var2))
  )
  types <- types[order(types)]
  paste0(types[1], ' v ', types[2])
}

df <- crossing(
  a = names(d_long)[-1], b = names(d_long[-1])
) %>%
  mutate(
    cor = map2_dbl(a, b, ~doCor(.x, .y, d_long)),
    types = map2_chr(a, b, ~getTypes(.x, .y, d_long))
  )

mat <- matrix(df$cor, nrow = sqrt(nrow(df)), ncol = sqrt(nrow(df)), byrow = T)
rownames(mat) <- unique(df$a)
colnames(mat) <- unique(df$b)

mat[!lower.tri(mat, diag = F)] <- NA
mat

ggplot(df, aes(x = a, y = b, fill = cor, colour = types)) +
  geom_tile()

# Neater correlation plot
as_tibble(mat) %>%
  mutate(a = unique(df$a)) %>%
  pivot_longer(cols = -a, names_to = 'b', values_to = 'cor') %>%
  mutate(
    types = map2_chr(a, b, ~getTypes(.x, .y, d_long)),
    types = if_else(is.na(cor), NA_character_, types)
  ) %>%
  ggplot(aes(x = a, y = b, fill = cor, colour = types)) +
  geom_tile() +
  geom_text(aes(label = round(cor, 3)))
