library(reshape2)
library(tidyverse)
library(getBestSpp)
source('Code/functions.R')
source('Code/half_baked_plot_function.R')
source('Code/summarize_sim_results.R')

source('Code/toy_model.R')

args <- commandArgs(TRUE)
nsims <- args[1]

fleet_distn <- list()
fleet_distn$"easy access" <- c(25,25,25,109,109,109)
fleet_distn$"even access" <- rep(67,6)
fleet_distn$"hard access" <- c(109,109,109,25,25,25)

corr.par <- c(0.5, 0, -0.5)

res.list <- list()
sim_pars$ind_pops <- 0#which(spp.names == 'groundfish')
for(ii in c(1,3)) {
  for(jj in c(1,3)) {
    set.seed(53209823)
    sim_pars$ships_per_fleet <- fleet_distn[[ii]]
    names(sim_pars$ships_per_fleet) <- fleets
    sim_pars$nships <- max(fleet_distn[[ii]])
    sim_pars$recruit_corr <- corr.par[jj]
    res.list[[paste(names(fleet_distn)[ii], as.character(corr.par[jj]))]] <- replicate(nsims, run_sim(
      sim_pars = sim_pars, long_output = FALSE), simplify = FALSE)
    print(paste('scenario', ii, jj))
  }
}

synchrony.access <- res.list
print('sims done')
sync_access_tibbles <- summarize_sim_results(synchrony.access, 'sync_access')
save(synchrony.access, file = 'Data/sync_access_10-8_1k.RData')
save(sync_access_tibbles, file = 'Data/sync_access_df_10-8_1k.RData')
