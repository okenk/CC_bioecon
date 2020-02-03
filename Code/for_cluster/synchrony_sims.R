library(reshape2)
library(tidyverse)
library(getBestSpp)
source('Code/functions.R')
source('Code/half_baked_plot_function.R')
source('Code/summarize_sim_results.R')
source('Code/toy_model.R')

library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(reshape2)
  library(tidyverse)
  library(getBestSpp)
  source('Code/functions.R')
  source('Code/half_baked_plot_function.R')
  source('Code/summarize_sim_results.R')
  source('Code/toy_model.R')
})
args <- commandArgs(TRUE)
nsims <- args[1]

res.list <- list()
corr.par <- c(0.5, 0, -0.5)
sim_pars$ind_pops <- 0#which(spp.names == 'groundfish')
sim_pars$ships_per_fleet <- rep(67,6)
names(sim_pars$ships_per_fleet) <- fleets
sim_pars$nships <- 67
for(ii in 1:3) {
  set.seed(53209823)
  sim_pars$recruit_corr <- corr.par[ii]
  res.list[[as.character(corr.par[ii])]] <- parLapply(cl, 1:nsims, run_sim, sim_pars=sim_pars, 
                                                      long_output = FALSE)
}
print('sims done')
synchrony <- res.list
sync_tibbles <- summarize_sim_results(synchrony, 'synchrony')
save(synchrony, file = 'Data/synchrony_10-8_1k.RData')
save(sync_tibbles, file = 'Data/sync_df_10-8_1k.RData')