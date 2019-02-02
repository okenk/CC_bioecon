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

catchability <- c(.0012, .00005); # these need to go down
# proportion of stock that will be caught by one fleet/ship during one week
# of fishing

price <- c(1, 1)
avg_rec <- c(1, 1)
avg_wt <- c(1,1)

weight_cv <- 0.2
recruit_cv <- 0.2

names(catchability) <- names(price) <- names(avg_rec) <- names(avg_wt) <- spp

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


salmon_tac_rule <- 0.3
salmon_tac <- N['salmon',,1] * salmon_tac_rule
# TAC for troll fishery is 30% of recruitment? (arbitrary) 

avg_rev <- numeric(2)
names(avg_rev) <- spp
avg_rev['crab'] <- avg_rec['crab'] * price['crab'] * avg_wt['crab']/ 
  (ships_per_fleet['crab'] + ships_per_fleet['both'])
avg_rev['salmon'] <-  salmon_tac_rule * price['salmon'] * avg_wt['salmon'] /
  (ships_per_fleet['salmon'] + ships_per_fleet['both'])

fixed_costs <- c(0.6, 0.1) * avg_rev
# crab 30% of revenue, salmon 10% of revenue
cost_per_trip <- NA

calc_var_cost <- function(cost_vec, recruits, wt_at_rec, fishing_season, in_season_dpltn, 
                          fleet_size, fixed_costs, catchability) {
  cost_per_trip <- exp(cost_vec[1])
  fixed_costs <- exp(cost_vec[2]) + cost_per_trip
  N <- numeric(wks_per_yr)
  N[1] <- recruits
  variable_costs <- revenue <- numeric(fleet_size)
  Catch <- matrix(0, nrow = wks_per_yr, ncol = fleet_size)
  scale_param <- (0.05 * cost_per_trip) / qlogis(0.95)
  for(wk in 1:wks_per_yr) { # put x price and catchability back in!
    exp_profit <- N[wk] * wt_at_rec * catchability  - cost_per_trip
    prob_fishing <- plogis(exp_profit, scale = scale_param)
    # if(wk==1) print(prob_fishing)
    nfishers <- round(prob_fishing * fleet_size, 0)
    did_fish <- c(rep(1, nfishers),
                  rep(0, fleet_size - nfishers))
    variable_costs <- variable_costs + did_fish * cost_per_trip
    Catch[wk,] <- did_fish * catchability * N[wk]
    revenue <- revenue + Catch[wk,] * wt_at_rec * price
    
    if(wk < wks_per_yr)
      N[wk+1] <- ifelse(in_season_dpltn,
                        N[wk] - sum(Catch[wk,]),
                        N[wk])
  }
  # print(sum(Catch))
  # print(paste(mean(variable_costs), fixed_costs))
  return((mean(revenue - variable_costs) - fixed_costs)^2 + 
           (.3 * mean(revenue) - fixed_costs)^2 -
           sum(Catch))
}

xx <- optimx(fn = calc_var_cost, par = c(-10, -5), upper= c(0,0), lower = c(-15, -10), method = "L-BFGS-B",
             recruits = 1, wt_at_rec = 1, 
             fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200, 
             fixed_costs = fixed_costs['crab'], catchability = .0008)

uniroot(calc_var_cost, interval = c(-20,0), recruits = 1, wt_at_rec = 1, 
        fishing_season = pop_seasons['salmon',], in_season_dpltn = FALSE, fleet_size = 200, 
        fixed_costs = fixed_costs['salmon'])

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

save.image(file = 'code/base_conditions.RData')
