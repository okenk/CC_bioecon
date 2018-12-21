require(dplyr)
require(mvtnorm)

set.seed(8902809)

spp <- c('crab', 'salmon')
fleets <- c('crab', 'salmon', 'both')

wks_per_yr <- 52
nyrs <- 50
npops <- length(spp)
nfleets <- length(fleets)
ships_per_fleet <- c(1, 1, 1)
fleet_permits <- matrix(c(1, 0, 0, 1, 1, 1), nrow=2, byrow=FALSE, dimnames = list(spp = spp, fleet = c('crab', 'salmon', 'both')))

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

price <- c(20, 20)
cost <- c(1, 1)

names(catchability) <- names(price) <- names(cost) <- spp

Catch <- array(0, dim = c(npops, nyrs, wks_per_yr, nfleets), 
               dimnames = list(spp = spp, yr = NULL, wk = NULL, fleet = fleets))
N <- array(0, dim = c(npops, nyrs, wks_per_yr), dimnames = list(spp = spp, yr = NULL, wk = NULL))
N[,,1] <- rmvnorm(nyrs, mean=c(0,log(.1))) %>% t() %>% exp() # assume average Recruitment = c(1, .1)
wt_at_rec <- rlnorm(npops*nyrs) %>% 
  matrix(nrow = npops, ncol = nyrs, dimnames = list(spp = spp, yr = NULL))
# Right now: white noise, independence between stocks. 
# To do: correlation in time and between stocks.
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
