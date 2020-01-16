library(reshape2)
library(tidyverse)
source('Code/functions.R')
source('Code/half_baked_plot_function.R')
source('Code/summarize_sim_results.R')

source('Code/toy_model.R')

args <- commandArgs(TRUE)
nsims <- args[1]

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
access_tibbles <- summarize_sim_results(access, 'access')
save(access, file = 'Data/access_10-8_1k.RData')
save(access_tibbles, file = 'Data/access_df_10-8_1k.RData')

