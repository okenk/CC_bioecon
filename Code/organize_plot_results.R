library(reshape2)
library(ggplot2)
library(tidyr)

# Recruitment synchrony ---------------------------------------------------
sim_pars$ships_per_fleet <- rep(100,3)
res.list <- list()
corr.par <- c(0.4, 0, -0.4)
for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$recruit_corr <- corr.par[ii]
  res.list[[as.character(corr.par[ii])]] <- map(1:100, function(.x) run_sim(sim_pars))
}
synchrony <- res.list
save(res.list, file = 'Code/synchrony.RData')
Catch.df <- map_dfr(res.list, function(corr.val)
  map_dfr(corr.val, function(sim.res) {
    melt(sim.res$Catch, value.name = 'catch') %>%
      as_tibble() %>%
      left_join(melt(sim.res$recruitment, value.name = 'recruitment'))
    }, 
    .id = 'sim_number'),
  .id = 'rec_corr')

profit.df <- map_dfr(res.list, function(corr.val)
  map_dfr(corr.val, function(sim.res) {
    melt(sim.res$profit, value.name = 'profit') %>%
      as_tibble() %>%
      left_join(melt(sim.res$recruitment, value.name = 'recruitment'))
  }, 
  .id = 'sim_number'),
  .id = 'rec_corr')

# Plot catch rates through season across years for fleets, species
# Visualized for a single simulated fleet. (There are 10 for each synchrony level)
filter(Catch.df, catch > 0, sim_number == '1') %>%
  # filter(yr %in% sample(50, 5, FALSE)) %>%
  # ggplot() +
  ggplot(aes(col = factor(yr))) +
  geom_line(aes(x=wk, y=catch, group=paste(yr, sim_number)), alpha = .5) +
  # geom_point(aes(x=wk, y=catch*wt_at_rec, group=yr), cex = .1,alpha = .5) +
  facet_grid(paste(spp, fleet) ~ rec_corr, scales = 'free_y') +
  # geom_vline(xintercept = 23) +
  # geom_vline(xintercept = 18) +
  # ylim(0, .05) +
  NULL

# Plot how catch accumulates through season
Catch.df %>%
  group_by(spp, yr, wk) %>%
  summarize(catch.per.wk = sum(catch)) %>%
  mutate(cum.catch = cumsum(catch.per.wk)) %>%
  ggplot() +
  geom_line(aes(x=wk, y=cum.catch, group=yr)) +
  facet_wrap(~spp, scales = 'free_y')

# What proportion of crab are caught?
Catch.df %>%
  group_by(rec_corr, sim_number, spp, yr) %>%
  summarize(catch = sum(catch),
            rec = first(recruitment),
            prop.caught = catch/rec) %>%
  # summarize(mean(prop.caught)) 
  filter(spp == 'crab') %>%
  ggplot() +
  geom_line(aes(x=yr,y=prop.caught, group=paste(sim_number, rec_corr), col = rec_corr), lwd = .25)
  
# What is distribution of profits?
ggplot(profit.df) +
  geom_freqpoly(aes(profit, col = fleet), bins = 70) +
  facet_wrap(~rec_corr) +
  NULL

mutate(profit.df, sim_ship = paste(sim_number, ship)) %>%
  group_by(fleet, rec_corr, sim_ship) %>%
  summarize(profit.sd = sd(profit)) %>%
  summarize(avg.profit.sd = mean(profit.sd))

mutate(profit.df, sim_ship = paste(sim_number, ship)) %>%
  group_by(fleet, rec_corr, sim_ship) %>%
  summarize(profit.sd = sd(profit)) %>%
  ggplot() +
  geom_freqpoly(aes(x = profit.sd, y = ..density.., col = fleet), bins = 50) +
  facet_wrap(~rec_corr)


# Fleet makeup ------------------------------------------------------------

sim_pars$recruit_corr <- 0
fleet_distn <- list()
fleet_distn$"easy access" <- c(50,50,200)
fleet_distn$"even access" <- rep(100,3)
fleet_distn$"hard access" <- c(125,125,50)

for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$ships_per_fleet <- fleet_distn[[ii]]
  names(sim_pars$ships_per_fleet) <- fleets
  sim_pars$nships <- max(fleet_distn[[ii]])
  res.list[[names(fleet_distn)[ii]]] <- map(1:100, function(.x) run_sim(sim_pars))
}

Catch.df <- map_dfr(res.list, function(fleet.distn)
  map_dfr(fleet.distn, function(sim.res) {
    melt(sim.res$Catch, value.name = 'catch') %>%
      as_tibble() %>%
      left_join(melt(sim.res$recruitment, value.name = 'recruitment'))
  }, 
  .id = 'sim_number'),
  .id = 'fleet_distn')

Catch.df <- filter(Catch.df, fleet_distn %in% c('easy access', 'even access', 'hard access'))
    
profit.df <- map_dfr(res.list, function(fleet.distn)
  map_dfr(fleet.distn, function(sim.res) {
    melt(sim.res$profit, value.name = 'profit') %>%
      as_tibble() %>%
      left_join(melt(sim.res$recruitment, value.name = 'recruitment'))
  }, 
  .id = 'sim_number'),
  .id = 'fleet_distn')

profit.df <-  profit.df %>%
  filter(fleet_distn %in% c('easy access', 'even access', 'hard access')) %>% 
  filter((fleet_distn=='easy access' & (ship<=50 | fleet=='both')) |
           (fleet_distn=='hard access' & (ship<=50 | fleet=='salmon' | fleet=='crab')) |
           fleet_distn=='even access')

# Plot catch rates through season across years for fleets, species
# Visualized for a single simulated fleet. (There are 10 for each synchrony level)
filter(Catch.df, catch > 0, sim_number == '1') %>%
  ggplot() +
  geom_line(aes(x=wk, y=catch, group=paste(yr, sim_number), col = recruitment), alpha = .5) +
  # geom_point(aes(x=wk, y=catch*wt_at_rec, group=yr), cex = .1,alpha = .5) +
  facet_grid(paste(spp, fleet) ~ fleet_distn, scales = 'free_y') +
  # geom_vline(xintercept = 23) +
  # geom_vline(xintercept = 18) +
  # ylim(0, .05) +
  NULL

# Plot how catch accumulates through season
Catch.df %>%
  group_by(spp, yr, wk) %>%
  summarize(catch.per.wk = sum(catch)) %>%
  mutate(cum.catch = cumsum(catch.per.wk)) %>%
  ggplot() +
  geom_line(aes(x=wk, y=cum.catch, group=yr)) +
  facet_wrap(~spp, scales = 'free_y')

# What proportion of crab are caught?
Catch.df %>%
  group_by(rec_corr, sim_number, spp, yr) %>%
  summarize(catch = sum(catch),
            rec = first(recruitment),
            prop.caught = catch/rec) %>%
  # summarize(mean(prop.caught)) 
  filter(spp == 'crab') %>%
  ggplot() +
  geom_line(aes(x=yr,y=prop.caught, group=paste(sim_number, rec_corr), col = rec_corr), lwd = .25)

# What is distribution of profits?
ggplot(profit.df) +
  geom_freqpoly(aes(x = profit, y=..density.., col = fleet), bins = 70) +
  facet_wrap(~fleet_distn) +
  NULL

group_by(profit.df, fleet, fleet_distn, sim_num, ship) %>%
  summarize(mean(profit), sd(profit))


test <- melt(xx$profit, value.name = 'profit') %>%
  as_tibble() %>%
  left_join(melt(xx$recruitment, value.name = 'recruitment'))
ggplot(test) +
  geom_freqpoly(aes(x = profit, y=..density.., col = fleet), bins = 70) +
  NULL
