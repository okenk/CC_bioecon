library(reshape2)
library(ggplot2)

Catch.df <- melt(Catch, value.name = 'catch') %>%
  left_join(melt(N[,,1], value.name = 'recruitment'))

filter(Catch.df) %>%
  ggplot() +
  geom_line(aes(x=wk, y=catch, group=yr, col = recruitment), alpha = .5) +
  facet_grid(spp ~ fleet, scales = 'free_y') +
  # geom_vline(xintercept = 18) +
  # ylim(0, .05) +
  NULL

# Salmon questions: Some years ships drop off one by one, other years altogether.
# Some years effort stops even though catches were higher than in years when effort doesn't stop.
# Why???

as_tibble(Catch.df) %>%
  filter(spp == 'salmon', fleet == 'salmon') %>%
  arrange(yr, wk) %>%
  filter(catch > .015) %>% View
