# Add a predictor to dummy data
d_aov <- d %>%
  mutate(
    riskCategory = factor(runif(nrow(d)) < .3),
    id = factor(id),
    covar_001 = as.numeric(runif(nrow(d)) < .5)
  ) %>%
  select(id, riskCategory, everything()) %>%
  mutate(id = factor(id))

library(ez)

ezANOVA(
  data = d_aov,
  dv = dad_001,
  wid = id,
  between = riskCategory,
  between_covariates = .(mum_001, child_001, covar_001)
)
