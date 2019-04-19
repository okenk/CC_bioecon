library(reshape2)
library(ggplot2)
library(tidyr)

wts <- as_tibble(t(wt_at_rec)) %>%
  bind_cols(tibble(yr = 1:nyrs)) %>%
  gather(key = spp, value = wt_at_rec, -yr)

Catch.df <- melt(Catch, value.name = 'catch') %>%
  as_tibble() %>%
  left_join(melt(N[,,1], value.name = 'recruitment')) %>%
  left_join(wts)

filter(Catch.df) %>%
  ggplot() +
  geom_line(aes(x=wk, y=catch*wt_at_rec, group=yr, col = recruitment), alpha = .5) +
  geom_point(aes(x=wk, y=catch*wt_at_rec, group=yr), cex = .1,alpha = .5) +
  facet_grid(spp ~ fleet, scales = 'free_y') +
  geom_vline(xintercept = 23) +
  # geom_vline(xintercept = 18) +
  # ylim(0, .05) +
  NULL

# Salmon questions: Some years ships drop off one by one, other years altogether.
# Some years effort stops even though catches were higher than in years when effort doesn't stop.
# Why???
filter(Catch.df, yr==41,spp=='salmon',fleet=='salmon') %>%
  ggplot(aes(x=wk,y=catch)) + geom_line()


as_tibble(Catch.df) %>%
  filter(spp == 'salmon', fleet == 'salmon') %>%
  arrange(yr, wk) %>%
  mutate(wk.diff = c(NA,diff(catch))) %>%
  arrange(wk.diff)
  mutate(second.diff = c(diff(wk.diff), NA)) %>%
  filter(catch > 0, yr==16)
  

N0_constant = N['salmon', 16, 1]
N0_var = N['salmon', 1,1]

N[,,1] <- N_full[,c(1,16),1]
wt_at_rec <- wt_full[,c(1,16)]
