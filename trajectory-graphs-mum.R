# Plot the trajectory graphs
# - Matt Jaquiery
# 2020-06-24

library(tidyverse)
# library(papaja)

theme_set(theme_light() +
          theme(
            panel.grid = element_blank(),
            panel.border = element_blank(),
            text = element_text(family = 'Times New Roman')
          ))

# Load data
if (!any(ls() == 'mumTrajData')) {
  # Handwritten data in simple form
  n <- tribble(
    ~Class, ~N,
    "Low", 1677,
    "Medium",  825,
    "High-persistent", 114
  )
  mumTrajData <- tribble(
    ~var, ~Variable, ~`Low`, ~`Medium`, ~`High-persistent`,
    "e390", '2 months',	3.22,	8.82, 14.37, 
    "f200", '8 months',	2.48,	8.27,	16.30,
    "g290", '21 months',	2.90, 8.22,	14.51
  ) %>% 
    pivot_longer(c(-var, -Variable), names_to = 'Class', values_to = 'EPDS') %>%
    left_join(n, by = "Class") %>%
    arrange(N) %>% 
    mutate(N = factor(N),
           Class = paste0(Class, "\nN = ", N),
           Class = fct_inorder(Class),
           Class = fct_rev(Class))
}

# Reorder the subscale levels to be consistent with the rest of the paper
mumTrajData <- mumTrajData %>%
  mutate(
    scale_order = case_when(
      Variable == "2 months" ~ 1,
      Variable == "8 months" ~ 2,
      Variable == "21 months" ~ 3
    ),
    scale_order_class = case_when(
      str_starts(Class, "High") ~ 1,
      str_starts(Class, "Medium") ~ 2,
      str_starts(Class, "Low") ~ 3
    )
  ) %>%
  mutate(
    Variable = fct_reorder(Variable, scale_order),
    Class = fct_reorder(Class, scale_order_class)
  )

mumTrajGraph <- mumTrajData %>%
  ggplot(aes(x = Variable, y = EPDS, shape = Class, colour = Class)) +
  geom_hline(yintercept = 0, colour = 'black', size = 0.5) +
  geom_hline(yintercept = 13, colour = 'black', linetype = 'dashed', size = 0.5) +
  geom_point(size = 5) +
  geom_line(aes(group = Class), size = 1.25) +
  annotate(geom = 'label', label = 'Clinical threshold', y = 13, x = "2 months", hjust = 1) +
  scale_y_continuous(limits = c(0, max(mumTrajData$EPDS + 2)), expand = c(0, 0)) +
  scale_color_grey() + 
  #scale_colour_manual(values = c('#0000FF', '#EEEE00', '#FF0000')) +
  theme(legend.position = 'top',
        panel.grid.major.x = element_line(colour = 'grey85', size = 1, linetype = 'solid'),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 16),
        legend.text.align = 0.5
        ) +
  labs(x = "Timepoint", y = "Mean EPDS score")
