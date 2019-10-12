library(reshape2)
library(ggplot2)
library(tidyr)
library(furrr)
source('Code/half_baked_plot_function.R')

nsims <- 5000
#save(synchrony, access, access.synchrony, file = 'Code/long_sim_10-1.RData')
load('Data/sync_access_10-7.RData')
make_half_baked_plots(synchrony.access, '10_19_5k', 'sync_access', 'sync_access', 'easy access 0.7')
rm(synchrony.access)
gc()

load('Data/access_10-7.RData')
make_half_baked_plots(access, '10_19_5k', 'access', 'access', 'even access')
rm(access)
gc()

load('Data/synchrony_10-7.RData')
make_half_baked_plots(synchrony, '10_19_5k', 'sync', 'rec_corr', 0)
rm(synchrony)
gc()
# make_half_baked_plots(timing, '10_19', 'timing', 'timing', 'normal')
