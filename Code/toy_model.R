require(dplyr)
require(mvtnorm)
require(optimx)

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

catchability <- c(.001, .00005); # these need to go down
# proportion of stock that will be caught by one fleet/ship during one week
# of fishing

price <- c(1, 1)
avg_rec <- c(1, 1)
avg_wt <- c(1,1)

weight_cv <- 0.2
recruit_cv <- 0.2

fixed_costs <- c(.0025, .0001)
cost_per_trip <- rep(NA, 2)
cost_cv <- sqrt(log(.15^2+1))

names(catchability) <- names(price) <- names(avg_rec) <- names(avg_wt) <- names(fixed_costs) <- names(cost_per_trip) <- spp

Catch <- array(0, dim = c(npops, nyrs, wks_per_yr, nfleets), 
               dimnames = list(spp = spp, yr = NULL, wk = NULL, fleet = fleets))
N <- array(0, dim = c(npops, nyrs, wks_per_yr), dimnames = list(spp = spp, yr = NULL, wk = NULL))
N[,,1] <- rmvnorm(nyrs, mean = log(avg_rec), sigma = recruit_cv * diag(2)) %>% 
  t %>% 
  exp
# recruit CV = 20%

wt_at_rec <- rmvnorm(nyrs, mean = log(avg_rec), sigma = weight_cv * diag(2)) %>% 
  t %>%
  exp
# Right now: white noise, independence between stocks. 
# To do: correlation in time and between stocks.


salmon_tac_rule <- 0.2
salmon_tac <- N['salmon',,1] * salmon_tac_rule
# TAC for troll fishery is 20% of recruitment? (arbitrary) 

# avg_rev <- numeric(2)
# names(avg_rev) <- spp
# avg_rev['crab'] <- avg_rec['crab'] * price['crab'] * avg_wt['crab']/ 
#   (ships_per_fleet['crab'] + ships_per_fleet['both'])
# avg_rev['salmon'] <-  salmon_tac_rule * price['salmon'] * avg_wt['salmon'] /
#   (ships_per_fleet['salmon'] + ships_per_fleet['both'])

# fixed_costs <- c(0.3, 0.1) * avg_rev
# crab 30% of revenue, salmon 10% of revenue

calc_var_cost <- function(log_avg_cost_per_trip, cost_cv, recruits, wt_at_rec, fishing_season, in_season_dpltn, 
                          fleet_size, fixed_costs, catchability, price, tac = NA) {
  # avg_cost_per_trip <- exp(log_cost_per_trip)
  quantiles <- seq(1/(fleet_size+1), fleet_size/(fleet_size+1), length.out = fleet_size)
  cost_per_trip <- qlnorm(quantiles, log_avg_cost_per_trip - cost_cv^2/2, cost_cv)
  N <- numeric(wks_per_yr)
  N[1] <- recruits
  variable_costs <- revenue <- numeric(fleet_size)
  Catch <- matrix(0, nrow = wks_per_yr, ncol = fleet_size)
  for(wk in 1:wks_per_yr) { 
    exp_profit <- N[wk] * wt_at_rec * price * catchability  - cost_per_trip
    did_fish <- exp_profit >= 0
    variable_costs <- variable_costs + did_fish * cost_per_trip
    Catch[wk,] <- did_fish * catchability * N[wk]
    revenue <- revenue + Catch[wk,] * wt_at_rec * price
    
    if(wk < wks_per_yr)
      N[wk+1] <- ifelse(in_season_dpltn,
                        N[wk] - sum(Catch[wk,]),
                        N[wk])
    if(!is.na(tac)) {
      if(sum(Catch) > tac * N[1])
        break
    }
  }
  print(sum(Catch))
  print(paste(mean(variable_costs), fixed_costs))
  profit <- sum(revenue - variable_costs) - fleet_size*fixed_costs 
  # print(paste('avg profit is', profit))
  return(profit)
}

xx <- uniroot(calc_var_cost, interval = c(-20, 0),
              cost_cv = cost_cv, recruits = 1, wt_at_rec = 1, price = 1,
              fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['crab'], catchability = catchability['crab'], tac = NA)
cost_per_trip['crab'] <- exp(xx$root)

xx <- uniroot(calc_var_cost, interval = c(-20, 0),
              cost_cv = cost_cv, recruits = 1, wt_at_rec = 1, price = 1,
              fishing_season = pop_seasons['salmon',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = .0001, catchability = catchability['salmon'], tac = salmon_tac_rule)
cost_per_trip['salmon'] <- exp(xx$root)



# simulate cost per trip for each ship in each fleet
cost_by_ship <- array(NA, dim = c(max(ships_per_fleet), nfleets, npops), dimnames = list(ships = NULL, fleet = fleets, spp = spp))
for(fleet in 1:nfleets) {
  fleet_permits_logical <- as.logical(fleet_permits[,fleet])
  cost_by_ship[1:ships_per_fleet[fleet], fleet, fleet_permits_logical] <- sapply(cost_per_trip[fleet_permits_logical], 
                                                                                 function(cost) rlnorm(ships_per_fleet[fleet], log(cost), cost_cv))
}

for(yr in 1:nyrs) {
  for(wk in 1:wks_per_yr) {
    exp_rev <- array(N[,yr,wk] * wt_at_rec[,yr] * catchability * price * pop_seasons[,wk], dim=c(2, 100, 3)) %>% 
      aperm(perm = c(2,3,1))
    # if out of legal season, expected revenue = 0, no one will fish.
    
    exp_profit <- exp_rev - cost_by_ship
    best_spp <- apply(exp_profit, c(1,2), function(ship) ifelse(max(ship, na.rm=TRUE) >= 0, which.max(ship), NA))
    # returns index of most profitable stock for each ship, or NA if no stock is profitable
    
    ships_per_stock <- apply(best_spp, 2, tabulate, nbins = npops)
    # counts up number of ships fishing each stock

    # Next task: keep track of costs and revenues
    
    Catch[,yr,wk,] <-  ships_per_stock * catchability * N[,yr,wk]
    if(wk < wks_per_yr) {
      N[,yr,wk+1] <- N[,yr,wk] - apply(Catch[,yr,wk,], 1, sum)
    }
  }
}

save.image(file = 'code/base_conditions.RData')
