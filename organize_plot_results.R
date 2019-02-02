require(reshape2)
require(ggplot2)

Catch.df <- melt(Catch, value.name = 'catch') %>%
  left_join(tibble(spp, price, cost)) %>%
  left_join(melt(wt_at_rec, value.name = 'weight')) %>%
  mutate(profit = ifelse(catch > 0, catch * weight * price - cost, 0)) %>%
  head()

filter(Catch.df) %>%
  ggplot() +
  geom_line(aes(x=wk, y=catch, group=yr)) +
  facet_grid(fleet ~ spp, scales = 'free_y') +
  NULL


filter(Catch.df, spp=='crab', fleet=='crab', wk==1)
