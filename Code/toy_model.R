library(dplyr)
library(mvtnorm)
library(purrr)
source('Code/functions.R')
Rcpp::sourceCpp("Code/getBestSpp.cpp")

# Model parameters --------------------------------------------------------

spp.names <- c('crab', 'salmon')
fleets <- c('crab', 'salmon', 'both')

wks_per_yr <- 52
nyrs <- 50
npops <- length(spp.names)
nfleets <- length(fleets)
ships_per_fleet <- c(1, 1, 1) * 100
nships <- max(ships_per_fleet)
fleet_permits <- matrix(c(1, 0, 0, 1, 1, 1), nrow=2, byrow=FALSE, 
                        dimnames = list(spp = spp.names, fleet = fleets))
names(ships_per_fleet) <- fleets

pop_seasons <- matrix(1, nrow = npops, ncol = wks_per_yr, dimnames = list(spp = spp.names, wk = NULL))

# crab (spp. 1) season = Dec. 1 - Aug. 14
# Assume yr starts Dec. 1
pop_seasons['crab', 37] <- 1
# pop_seasons['crab', 37] <- 0.7
pop_seasons['crab', 38:wks_per_yr] <- 0
# pop_seasons['crab', 1:4] <- 0 # HAB closure

# salmon (spp. 2) season = May 1 - Oct. 31 (actually more complicated)
pop_seasons['salmon', 1:21] <- 0
# pop_seasons['salmon', 22] <- 0.3
pop_seasons['salmon', 22] <- 0
pop_seasons['salmon', 47] <- 1
# pop_seasons['salmon', 47] <- 0.7
pop_seasons['salmon', 48:wks_per_yr] <- 0;

catchability <- c(.0005, .00005); # these need to go down
# proportion of stock that will be caught by one fleet/ship during one week
# of fishing

price <- c(1, 1)
avg_rec <- c(1, 1)
avg_wt <- c(1,1)

weight_cv <- 0 # sqrt(log(0.2^2+1))
recruit_cv <- rep(sqrt(log(0.5^2+1)), 2)
recruit_ar <- c(.3,.3)
recruit_corr <- .25

fixed_costs <- c(.0025, .0001)
cost_per_trip <- rep(NA, 2)
cost_cv <- sqrt(log(.15^2+1))
cost_corr <- 0.7

names(catchability) <- names(price) <- names(avg_rec) <- names(avg_wt) <- names(fixed_costs) <- names(cost_per_trip) <- 
  names(recruit_cv) <- names(recruit_ar) <- spp.names

salmon_tac_rule <- 0.3

crab_price_cutoff <- 0.1
crab_price_pars <- c(1,0)

sim_pars <- list(spp.names = spp.names, fleets = fleets, wks_per_yr = wks_per_yr, nyrs = nyrs, npops = npops,
                 nfleets = nfleets, ships_per_fleet = ships_per_fleet, nships = nships, fleet_permits = fleet_permits,
                 pop_seasons = pop_seasons, catchability = catchability, price = price, avg_rec = avg_rec, 
                 avg_wt = avg_wt, weight_cv = weight_cv, recruit_cv = recruit_cv, recruit_ar = recruit_ar, 
                 recruit_corr = recruit_corr, fixed_costs = fixed_costs, cost_per_trip = cost_per_trip, 
                 cost_cv = cost_cv, cost_corr = cost_corr, salmon_tac_rule = salmon_tac_rule,
                 crab_price_cutoff = crab_price_cutoff, crab_price_pars = crab_price_pars)

# Crab demand function. Currently hard-coded into simulation function. This is not used. -------

# Assume price = 1 first week, everyone fishes
min_price <- 1 # Note this is hard-coded into the calculations right now!

# At 20% of initial catch, prices are 30% higher
price_increase <- 0.3
ref_catch <- 0.2

# linear demand function
beta_price <- price_increase/((1-ref_catch) * catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                sum(ships_per_fleet[c('crab', 'both')]))

alpha_price <- 1 + beta_price * catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] * 
  sum(ships_per_fleet[c('crab', 'both')])

# log function. Price increases too steeply at low catches.
beta_price <- price_increase/(log(catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                    (ships_per_fleet['crab'] + ships_per_fleet['both'])) -
                                log(ref_catch * catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                      (ships_per_fleet['crab'] + ships_per_fleet['both'])))
alpha_price <- 1 + beta_price * log(catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                      (ships_per_fleet['crab'] + ships_per_fleet['both']))

# Calculating variable costs ----------------------------------------------

xx <- uniroot(calc_var_cost, interval = c(-20,0),
              cost_cv = cost_cv, recruits = avg_rec['crab'], wt_at_rec = 1, price = c(2,10), #price['crab'], c(alpha_price, beta_price),
              fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['crab'], catchability = catchability['crab'], tac = NA)
sim_pars$cost_per_trip['crab'] <- exp(xx$root)

xx <- uniroot(calc_var_cost, interval = c(-20, 0),
              cost_cv = cost_cv, recruits = avg_rec['salmon'], wt_at_rec = 1, price = price['salmon'],
              fishing_season = pop_seasons['salmon',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['salmon'], catchability = catchability['salmon'], tac = salmon_tac_rule)
sim_pars$cost_per_trip['salmon'] <- exp(xx$root)

# calc_var_cost(log_avg_cost_per_trip = log(cost_per_trip['crab']),
#               cost_cv = cost_cv, recruits = 1, wt_at_rec = 1, price = 1,#c(alpha_price, beta_price),
#               fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200,
#               fixed_costs = fixed_costs['crab'], catchability = catchability['crab'], tac = NA)

# calc_var_cost(log_avg_cost_per_trip = log(cost_per_trip['salmon']),
#               cost_cv = cost_cv, recruits = 1, wt_at_rec = 1, price = 1,#c(alpha_price, beta_price),
#               fishing_season = pop_seasons['salmon',], in_season_dpltn = TRUE, fleet_size = 200,
#               fixed_costs = fixed_costs['salmon'], catchability = catchability['salmon'], tac = salmon_tac_rule)

# Run model! --------------------------------------------------------------
