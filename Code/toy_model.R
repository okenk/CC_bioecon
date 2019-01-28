require(dplyr)
require(mvtnorm)

set.seed(8902809)

spp <- c('crab', 'salmon')
fleets <- c('crab', 'salmon', 'both')

wks_per_yr <- 52
nyrs <- 50
npops <- length(spp)
nfleets <- length(fleets)
ships_per_fleet <- c(1, 1, 1) * 100
fleet_permits <- matrix(c(1, 0, 0, 1, 1, 1), nrow=2, byrow=FALSE, 
                        dimnames = list(spp = spp, fleet = fleets))
names(ships_per_fleet) <- fleets

pop_seasons <- matrix(1, nrow = npops, ncol = wks_per_yr, dimnames = list(spp = spp, wk = NULL))

# crab (spp. 1) season = Dec. 1 - Aug. 14
# Assume yr starts Dec. 1
pop_seasons['crab', 37] <- 0.7
pop_seasons['crab', 38:wks_per_yr] <- 0

# salmon (spp. 2) season = May 1 - Oct. 31 (actually more complicated)
pop_seasons['salmon', 1:21] <- 0
pop_seasons['salmon', 18] <- 0.3
pop_seasons['salmon', 47] <- 0.7
pop_seasons['salmon', 48:wks_per_yr] <- 0;

catchability <- c(.07, .01);
# proportion of stock that will be caught by one fleet/ship during one week
# of fishing

price <- c(1, 1)
avg_rec <- c(1, 1)
names(catchability) <- names(price) <- names(avg_rec) <- spp

Catch <- array(0, dim = c(npops, nyrs, wks_per_yr, nfleets), 
               dimnames = list(spp = spp, yr = NULL, wk = NULL, fleet = fleets))
N <- array(0, dim = c(npops, nyrs, wks_per_yr), dimnames = list(spp = spp, yr = NULL, wk = NULL))
N[,,1] <- rmvnorm(nyrs, mean = log(avg_rec), sd = c(.2, .2)) %>% 
  t %>% 
  exp
# recruit CV = 20%

wt_at_rec <- rlnorm(npops*nyrs) %>% 
  matrix(nrow = npops, ncol = nyrs, dimnames = list(spp = spp, yr = NULL))
# Right now: white noise, independence between stocks. 
# To do: correlation in time and between stocks.

salmon_tac <- N['salmon',,1] * 0.1
# TAC for troll fishery is 10% of recruitment? (arbitrary) 

avg_rev <- numeric(2)
names(avg_rev) <- spp
avg_rev['crab'] <- avg_rec['crab'] * price['crab'] / 
  (ships_per_fleet['crab'] + ships_per_fleet['both'])

for(yr in 1:nyrs) {
  for(wk in 1:wks_per_yr) {
    exp_util <- rbind(matrix(exp(N[,yr,wk] * wt_at_rec[,yr] * catchability * price - cost) * pop_seasons[,wk], nrow = npops, ncol = nfleets) * fleet_permits, 1)
    prob_fishing <- exp_util / matrix(apply(exp_util, 2, sum), nrow = npops+1, ncol = nfleets, byrow = TRUE)
    Catch[,yr,wk,] <- prob_fishing[1:npops,] * catchability * N[,yr,wk]
    if(wk < wks_per_yr) {
      N[,yr,wk+1] <- N[,yr,wk] - apply(Catch[,yr,wk,], 1, sum)
    }
  }
}

# I need:
# CV wt_at_rec
# CV recruitment
# Figure out units. What parameters actually drive dynamics/which can be fixed to 1?
# E.g., if response variables of interested are all related to catch, then catchability and biomass influence results jointly, never separately
# (But, separately could be useful when thinking about ecological scenarios down the road)
# Better understand prices and costs
# Better understand dynamics of fishing through season, esp. for salmon, for tuning

# Farther down the road:
# Add groundfish (sablefish-ish?)
# Correlation structure to stochastic components
# Once you switch gears you don't go back

# fixed cost is per season (capital of boat, annual maintenance, permits
# important because will allow people to operate at a loss

# extra normal profit (rent): more profit than a normal return on investment, like
# the stock market, when it's positive people entry fishery, use their licenses
# in a normal year, profits should cover costs just barely

# what is the avg revenue? e.g., set the price to 1. You know what the avg. total
# revenue is. Then choose a fleet size. Either have more than 3 in fleet or allow
# for fractions. Dan says easier to make fleet discrete. 
# On an avg year given total catch, divided evenly among vessels, fixed costs are
# 1/4 or total revenue and 3/4 of revenue is variable costs. No more than 1/3.
# For salmon maybe 1/10. 

# Prop. fixed to variable costs could be a model parameter that varies by fishery.
# Start with equal sized fleets. 

# permit price should increase to soak up extra profit. we won't model unidirectional
# change, something that is variable but not directionally so. so then we won't 
# let permit price vary. 

# at the start of the season you have fleet, they get signal of recruitment,
# some of the less profitable ones realize they won't make it, don't enter.
# for now say they know recruitment perfectly, and assume the same   number of vessels that 
# fished last year.
# Something that I should pay attention to: one fishery is more capital intensive

