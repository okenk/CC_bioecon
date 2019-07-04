library(reshape2)
library(ggplot2)
library(tidyr)

# Recruitment synchrony ---------------------------------------------------
res.list <- list()
corr.par <- c(0.4, 0, -0.4)
for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$recruit_corr <- corr.par[ii]
  res.list[[as.character(corr.par[ii])]] <- map(1:100, function(.x) run_sim(sim_pars))
}
synchrony <- res.list

# What proportion of crab are caught?
Catch.df %>%
  group_by(rec_corr, sim_number, spp, yr) %>%
  summarize(catch = sum(catch),
            rec = first(rec_dev),
            prop.caught = catch/rec) %>%
  # summarize(mean(prop.caught)) 
  filter(spp == 'crab') %>%
  ggplot() +
  geom_line(aes(x=yr,y=prop.caught, group=paste(sim_number, rec_corr), col = rec_corr), lwd = .25)
  
# Fleet makeup ------------------------------------------------------------

sim_pars$recruit_corr <- 0
fleet_distn <- list()
fleet_distn$"easy access" <- c(109,109,109,25,25,25)
fleet_distn$"even access" <- rep(100,3)
fleet_distn$"hard access" <- c(25,25,25,109,109,109)

res.list <- list()
for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$ships_per_fleet <- fleet_distn[[ii]]
  names(sim_pars$ships_per_fleet) <- fleets
  sim_pars$nships <- max(fleet_distn[[ii]])
  res.list[[names(fleet_distn)[ii]]] <- map(1:100, function(.x) run_sim(sim_pars))
}
access <- res.list


# crab delay --------------------------------------------------------------
sim_pars$ships_per_fleet <- rep(100,3)
names(sim_pars$ships_per_fleet) <- fleets
sim_pars$nships <- 100

res.list <- list()
season_list <- map(1:3, ~pop_seasons)
names(season_list) <- c('normal', 'late opening', 'early closure')
season_list$`late opening`['crab', 1:4] <- 0
season_list$`early closure`['crab', 28:37] <- 0

for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$pop_seasons <- season_list[[ii]]
  res.list[[names(season_list)[ii]]] <- map(1:100, function(.x) run_sim(sim_pars))
}
timing <- res.list


make_half_baked_plots(synchrony, 'sync', 'rec_corr', 0)
make_think_tank_plots(access, 'access', 'access', 'even access')
make_think_tank_plots(timing, 'timing', 'timing', 'normal')


#### To do:
## 1. Add groundfish
## 2. Make salmon vessels more likely to start out season fishing? (Too many forgo fishing for the year.)
## 3. Would within season mortality make the season shortening story more interesting?