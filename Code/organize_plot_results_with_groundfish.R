library(reshape2)
library(tidyverse)
source('Code/functions.R')
source('Code/half_baked_plot_function.R')
source('Code/summarize_sim_results.R')

source('Code/toy_model.R')

nsims <- 10

# Recruitment synchrony ---------------------------------------------------
res.list <- list()
corr.par <- c(0.5, 0, -0.5)
sim_pars$ind_pops <- 0#which(spp.names == 'groundfish')
sim_pars$ships_per_fleet <- rep(67,6)
names(sim_pars$ships_per_fleet) <- fleets
sim_pars$nships <- 67
for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$recruit_corr <- corr.par[ii]
  res.list[[as.character(corr.par[ii])]] <- map(1:nsims, function(.x) run_sim(sim_pars, long_output = TRUE))
}
synchrony <- res.list
save(synchrony, file = 'Data/synchrony_10-8_1k.RData')
rm(synchrony)
gc()
# # What proportion of crab are caught?
# Catch.df %>%
#   group_by(rec_corr, sim_number, spp, yr) %>%
#   summarize(catch = sum(catch),
#             rec = first(rec_dev),
#             prop.caught = catch/rec) %>%
#   # summarize(mean(prop.caught)) 
#   filter(spp == 'crab') %>%
#   ggplot() +
#   geom_line(aes(x=yr,y=prop.caught, group=paste(sim_number, rec_corr), col = rec_corr), lwd = .25)
#   

# Fleet makeup ------------------------------------------------------------

sim_pars$recruit_corr <- 0
sim_pars$ind_pops <- 0
fleet_distn <- list()
fleet_distn$"easy access" <- c(25,25,25,109,109,109)
fleet_distn$"even access" <- rep(67,6)
fleet_distn$"hard access" <- c(109,109,109,25,25,25)

res.list <- list()
for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$ships_per_fleet <- fleet_distn[[ii]]
  names(sim_pars$ships_per_fleet) <- fleets
  sim_pars$nships <- max(fleet_distn[[ii]])
  res.list[[names(fleet_distn)[ii]]] <- map(1:nsims, function(.x) run_sim(sim_pars, long_output = TRUE))
}
access <- res.list
save(access, file = 'Data/access_10-8_1k.RData')
rm(access)
gc()

# Interaction -------------------------------------------------------------

res.list <- list()
sim_pars$ind_pops <- 0#which(spp.names == 'groundfish')
for(ii in c(1,3)) {
  for(jj in c(1,3)) {
  set.seed(53209823)
  sim_pars$ships_per_fleet <- fleet_distn[[ii]]
  names(sim_pars$ships_per_fleet) <- fleets
  sim_pars$nships <- max(fleet_distn[[ii]])
  sim_pars$recruit_corr <- corr.par[jj]
  res.list[[paste(names(fleet_distn)[ii], as.character(corr.par[jj]))]] <- map(1:nsims, function(.x) run_sim(sim_pars, long_output = TRUE))
  }
}

synchrony.access <- res.list
save(synchrony.access, file = 'Data/sync_access_10-8_1k.RData')
rm(synchrony.access)
gc()


# Crab delay --------------------------------------------------------------

sim_pars$recruit_corr <- 0
sim_pars$ind_pops <- 0
sim_pars$ships_per_fleet <- rep(67,6)
names(sim_pars$ships_per_fleet) <- fleets
sim_pars$nships <- 67

res.list <- list()
pop_seasons_hab <- map(1:2, ~pop_seasons[[1]])
names(pop_seasons_hab) <- c('normal', 'late opening')
pop_seasons_hab$`late opening`['crab', 1:16] <- 0

set.seed(53209823)
sim_pars$pop_seasons <- pop_seasons_hab
sim_pars$season_prob <- c(.75, .25)

res.list$hab <- map(1:nsims, function(.x) run_sim(sim_pars))

load('Data/access_10-8_1k.RData')
res.list$normal <- access$`even access`

timing <- res.list
save(timing, file = 'Data/timing_10-8_1k.RData')
rm(timing, access)
gc()

# Summarizing and plotting ------------------------------------------------

load('Data/sync_access_10-8_1k.RData')
sync_access_tibbles <- summarize_sim_results(synchrony.access, 'sync_access')
save(sync_access_tibbles, file = 'Data/sync_access_df_10-8_1k.RData')
rm(synchrony.access, sync_access_tibbles)
gc()

load('Data/sync_access_df_10-8_1k.RData')
make_half_baked_plots(sync_access_tibbles, 'PICES', 'sync_access_3_spp', 'sync_access')

load('Data/access_10-8_1k.RData')
access_tibbles <- summarize_sim_results(access, 'access')
save(access_tibbles, file = 'Data/access_df_10-8_1k.RData')
rm(access, access_tibbles)
gc()

load('Data/access_df_10-8_1k.RData')
make_half_baked_plots(access_tibbles, '10k', 'access_aggregated', 'access')

load('Data/synchrony_10-8_1k.RData')
sync_tibbles <- summarize_sim_results(synchrony, 'synchrony')
save(sync_tibbles, file = 'Data/sync_df_10-8_1k.RData')
rm(synchrony, sync_tibbles)
gc()

load('Data/sync_df_10-8_1k.RData')
make_half_baked_plots(sync_tibbles, 'PICES', 'sync_3_spp', 'synchrony')

load('Data/timing_10-8_1k.RData')
timing_tibbles <- summarize_sim_results(timing, 'season_type')
save(timing_tibbles, file = 'Data/timing_df_10-8_1k.RData')
rm(timing, timing_tibbles)
gc()

load('Data/timing_df_10-8_1k.RData')
make_half_baked_plots(timing_tibbles, 'HABs', 'timing', 'season_type')

one.sim <- map_dfr(access, function(sim.par) 
  apply(sim.par[[1]]$revenue, 1:3, sum) %>%
    melt(value.name = 'revenue') %>%
    as_tibble(),
  .id = 'access') %>%
  group_by(access, spp, yr) %>%
  mutate(cum_catch = cumsum(Catch))

one.sim %>%
  filter(yr == 20) %>%
  ggplot() +
  geom_line(aes(x = wk, y = Catch, col = access)) + 
  facet_wrap(~spp, scales = 'free_y')

#### To do:
## 2. Make salmon vessels more likely to start out season fishing? (Too many forgo fishing for the year.)
## 3. Would within season mortality make the season shortening story more interesting?

# Check that groundfish population is relatively stable for base case. Looks ok!
names(synchrony[[2]]) <- 1:nsims
map_dfr(synchrony[[2]], function(sim) sim$groundfish_bio[,1]) %>%
  mutate(year = 1:50) %>%
  gather(key = 'sim', value = 'biomass', -year) %>%
  ggplot() +
  geom_line(aes(x = year, y = biomass, group = sim)) +
  stat_smooth(aes(x = year, y = biomass)) +
  geom_hline(aes(yintercept = mean(biomass)), col = 'red')