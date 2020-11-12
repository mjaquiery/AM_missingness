# pivot longer to multiple columns
d %>% 
  pivot_longer(
    cols = -id,
    names_to = c(".value", "timepoint"),
    names_pattern = "(.+)_(.+)"
  )
