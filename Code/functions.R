# This function finds the index of a vector containing the value that is closest to a target value.
# vec: the vector of all values you are taking an index from
# target: the target for which you want to know which element of vec is closest 
find_closest <- function(vec, target) {
  which.min(abs(vec - target))
}

# This function simulates two positive random time series that are correlated with one another and are also autocorrelated.
# corr: correlation between the two time series, on the log-scale 
# ar: autoregressive parameter for each time series, on the log-scale
# cv: SD of each time series on the log-scale (approximate CV on the actual scale)
# mn: mean of the time series, on the log-scale. No bias correction is done, so bias correction should be done prior
#     to running this function
sim_correlated_ar_ts <- function(corr, ar, cv, mn, nyrs, npops) {
  if(npops > 2) {
    stop('Can only generate synchronous autocorrelated time series for two populations')
  }
  burn.in <- 300
  q12 <- corr * (1 - ar[1] * ar[2]) * (cv[1] * cv[2])
  q.diag <- cv^2 * (1 - ar^2)
  eps <- rmvnorm(nyrs + burn.in, c(0,0), cbind(c(q.diag[1], q12), c(q12, q.diag[2]))) %>%
    as.data.frame()
  
  out.ts <- map2(eps, ar, function(.x, .y)
    as.vector(arima.sim(list(ar = .y), nyrs, innov = .x[burn.in+1:nyrs], start.innov = .x[1:burn.in]))) %>%
    map2(mn, ~ exp(.x + .y)) %>%
    bind_cols() %>%
    as.matrix()
  colnames(out.ts) <- names(mn)
  return(out.ts)
}

# This function calculates the profit made by a marginal (5th percentile) fisher in an average year and returns profits. 
# The function is built to be used in a root-finding method to calculate what variable costs will lead to a fishing 
# fleet at equilibrium (no one leaving or entering the fishery). There are some print statement to help with tuning.
#
# It currently handles only single fishery fleets, no multi-fishery participation.
# One idea: instead of calculating in the average year, which will lead to average profits > 0 because profits have a 
# positive skew (profits have a negative limit in bad years because people stop fishing, but no positive limit in bumper 
# years), calculate average profit across distribution of years. 
calc_var_cost <- function(log_avg_cost_per_trip, cost_cv, recruits, wt_at_rec, fishing_season, in_season_dpltn, 
                          fleet_size, fixed_costs, catchability, price, tac = NA) {
  # avg_cost_per_trip <- exp(log_cost_per_trip)
  quantiles <- seq(1/(fleet_size+1), fleet_size/(fleet_size+1), length.out = fleet_size)
  cost_per_trip <- qlnorm(quantiles, log_avg_cost_per_trip - cost_cv^2/2, cost_cv)
  N <- numeric(wks_per_yr)
  N[1] <- recruits
  variable_costs <- revenue <- numeric(fleet_size)
  Catch <- matrix(0, nrow = wks_per_yr, ncol = fleet_size)
  if(length(price) == 1) {
    price_vec <- rep(price, wks_per_yr+1)
  } else {
    price_vec <- numeric(wks_per_yr+1)
    price_vec[1] <- 1#price[1] - price[2] * recruits * wt_at_rec * catchability * fleet_size
  }
  
  for(wk in 1:wks_per_yr) { 
    exp_profit <- N[wk] * wt_at_rec * price_vec[wk] * catchability * fishing_season[wk]  - cost_per_trip
    did_fish <- exp_profit >= 0
    variable_costs <- variable_costs + did_fish * cost_per_trip
    Catch[wk,] <- did_fish * catchability * N[wk]
    if(length(price) > 1) {
      if(sum(Catch[wk,]) > 0.1) {
        price_vec[wk+1] <- 1
      } else{
        price_vec[wk+1] <- price[1] - price[2] * sum(Catch[wk,])
      }
    }
    revenue <- revenue + Catch[wk,] * wt_at_rec * price_vec[wk+1]
    
    if(wk < wks_per_yr) {
      N[wk+1] <- ifelse(in_season_dpltn,
                        N[wk] - sum(Catch[wk,]),
                        N[wk])
    }
    if(!is.na(tac)) {
      if(sum(Catch) > tac * N[1]) {
        print('TAC reached!')
        break
      }
    }
  }
  print(sum(Catch))
  # print(price_vec)
  print(paste(mean(variable_costs), fixed_costs))
  # Want entry/exit at equilibrium, new fisher should expect 0 net profits. 
  # New fisher would have higher than average costs though, so set profit = 0 for 95th percentile variable cost fisher 
  marginal_fisher <- round(.95 * fleet_size)
  profit <- revenue[marginal_fisher] - variable_costs[marginal_fisher] - fixed_costs
  # profit <- sum(revenue - variable_costs) - fixed_costs*fleet_size 
  # print(paste('avg profit is', profit))
  return(profit)
}

calc_var_cost_groundfish <- function(log_avg_cost_per_trip, cost_cv, N1, bio_init, fishing_season, in_season_dpltn, 
                          fleet_size, fixed_costs, catchability, price, tac = NA, groundfish) {
  # avg_cost_per_trip <- exp(log_cost_per_trip)
  quantiles <- seq(1/(fleet_size+1), fleet_size/(fleet_size+1), length.out = fleet_size)
  cost_per_trip <- qlnorm(quantiles, log_avg_cost_per_trip - cost_cv^2/2, cost_cv)
  recruits <- rep(bio_init/(groundfish$BH_a + groundfish$BH_b * bio_init), 4)
  bio_old <- 0
  ii <- 0
  
  while((abs(bio_old - bio_init) > .00001) & ii < 1000) {
    N <- biomass <- numeric(wks_per_yr)
    N[1] <- N1
    biomass[1] <- bio_init
    variable_costs <- revenue <- numeric(fleet_size)
    yield <- matrix(0, nrow = wks_per_yr, ncol = fleet_size)
    
    for(wk in 1:wks_per_yr) { 
      exp_profit <- biomass[1] * price * catchability * fishing_season[wk]  - cost_per_trip
      did_fish <- exp_profit >= 0
      variable_costs <- variable_costs + did_fish * cost_per_trip
      yield[wk,] <- did_fish * catchability * biomass[wk]
      revenue <- revenue + yield[wk,] * price
      
      groundfish_total_survival <- exp(-groundfish$M_wk) * 
        (1 - catchability * sum(did_fish))
      if(wk < wks_per_yr) {
        N[wk+1] <- N[wk] * groundfish_total_survival
        biomass[wk+1] <- groundfish_total_survival * 
          (groundfish$alpha * N[wk] + groundfish$rho * biomass[wk])
      }
      
    }
    bio_temp <- bio_init
    N1 <- N[wk] * groundfish_total_survival + recruits[1]
    bio_init <- groundfish_total_survival * 
      (groundfish$alpha * N[wk] + groundfish$rho * biomass[wk]) + recruits[1]
    recruits[1:3] <- recruits[2:4]
    recruits[4] <- exp(-groundfish$M_wk*wks_per_yr*4) * groundfish$BH_a * bio_init/(groundfish$BH_b + bio_init)
    bio_old <- bio_temp
    ii <- ii + 1
  }
  print(paste('yield is', sum(yield)))
  print(paste('biomass is', bio_init))
  # print(price_vec)
  # print(paste(mean(variable_costs), fixed_costs))
  # Want entry/exit at equilibrium, new fisher should expect 0 net profits. 
  # New fisher would have higher than average costs though, so set profit = 0 for 95th percentile variable cost fisher 
  marginal_fisher <- round(.95 * fleet_size)
  profit <- revenue[marginal_fisher] - variable_costs[marginal_fisher] - fixed_costs
  # profit <- sum(revenue - variable_costs) - fixed_costs*fleet_size 
  # print(paste('avg profit is', profit))
  if(ii == 500) {
    profit <- 100
    print('broke out of loop')
  }
  return(profit)
}

# This function sets up the objects to be filled in the simulations. Returns a list of abundance (includes simulated 
# pre-season recruitment, remaining weeks to be filled), catch (empty), profit (empty), weights (simulated), 
# salmon TACs (based on the simulated recruitment), ship-specific variable costs (simulated, constant across years), 
# number of ships from the multi-fishery fleet fishing for crab (empty)
set_up_objects <- function(sim_pars) {
  list2env(sim_pars, sys.frame(sys.nframe()))
  profits <- array(-t(fleet_permits) %*% fixed_costs, dim = c(nfleets, nships, nyrs),  
                   dimnames = list(fleet = fleets, ship = NULL, yr = NULL)) %>%
    aperm(c(2,3,1))
  
  revenue <- array(0, dim = c(nships, nyrs, nfleets), dimnames = list(ship = NULL, yr = NULL, fleet = fleets))
  
  Catch <- array(0, dim = c(npops, nyrs, wks_per_yr, nfleets), 
                 dimnames = list(spp = spp.names, yr = NULL, wk = NULL, fleet = fleets))
  
  groundfish_bio <- matrix(0, nrow = nyrs, ncol = wks_per_yr, dimnames = list(yr=NULL, wk=NULL))
  groundfish_bio[1,1] <- groundfish$b_init
  
  N <- array(0, dim = c(npops, nyrs, wks_per_yr), dimnames = list(spp = spp.names, yr = NULL, wk = NULL))
 
  # N[,,1] <- sim_correlated_ar_ts(corr = recruit_corr, ar = recruit_ar, cv = recruit_cv, 
  #                                mn = log(avg_rec) - recruit_cv^2/2, nyrs = nyrs, npops = npops) %>% t()
  sigma_mat <- matrix(recruit_corr * recruit_cv^2, nrow = npops, ncol = npops)
  diag(sigma_mat) <- recruit_cv^2
  N[,,1] <- rec_devs <- rmvnorm(n = nyrs, sigma = sigma_mat, mean = log(avg_rec) - recruit_cv^2/2) %>%
    t %>% exp
  dimnames(rec_devs) <- list(spp = spp.names, yr = NULL)
  N['groundfish',1,1] <- groundfish$N_init
  groundfish_rec <- groundfish$rec_init * rec_devs['groundfish', 1:4]
  
  wt_at_rec <- rmvnorm(nyrs, mean = log(avg_wt) - weight_cv^2/2, sigma = weight_cv * diag(npops)) %>% 
    t %>%exp
  dimnames(wt_at_rec) <- list(spp = spp.names, yr = NULL)
  
  salmon_tac <- N['salmon',,1] * salmon_tac_rule

  # simulate cost per trip for each ship in each fleet
  cost_by_ship <- array(0, dim = c(nships, nfleets, npops), 
                        dimnames = list(ships = NULL, fleet = fleets, spp = spp.names))
  sigma_mat <- matrix(cost_corr*cost_cv^2, nrow = npops, ncol = npops)
  diag(sigma_mat) <- cost_cv^2
  
  for(fleet in 1:nfleets) {
    fleet_permits_TF <- as.logical(fleet_permits[,fleet])
    cost_by_ship[1:ships_per_fleet[fleet], fleet,] <- exp(rmvnorm(ships_per_fleet[fleet],
                                                                  mean = log(cost_per_trip) - cost_cv^2/2,
                                                                  sigma = sigma_mat)) %*%
      diag(fleet_permits_TF)
  }
  cost_by_ship[cost_by_ship==0] <- NA
  
  effort <- array(0, dim = c(npops, nfleets, wks_per_yr, nyrs), 
                        dimnames = list(spp = spp.names, fleet = fleets, wk = NULL, yr = NULL))
  out.list <- list(profits = profits, Catch = Catch, N = N, wt_at_rec = wt_at_rec, rec_devs = rec_devs,
                   salmon_tac = salmon_tac, cost_by_ship = cost_by_ship, effort = effort,
                   revenue = revenue, groundfish_bio = groundfish_bio, groundfish_rec = groundfish_rec)
  return(out.list)
}


# This function simulates the fleets and populations for a bunch of years with weekly time steps. Returns catch, profits, 
# recruitment, number of ships in the multi-fishery fleet fishing for crab each week of each year
run_sim <- function(sim_pars, seed = NA) {
  if(!is.na(seed)) set.seed(seed)
  setup.ls <- set_up_objects(sim_pars)
  list2env(setup.ls, sys.frame(sys.nframe()))
  list2env(sim_pars, sys.frame(sys.nframe()))
  
  this_week_profit <- this_week_revenue <- matrix(0, nrow = nships, ncol = nfleets)
  for(yr in 1:nyrs) {
    price_mat <- matrix(price, nrow = wks_per_yr+1, ncol = npops, byrow = TRUE,
                        dimnames = list(wk=NULL, spp=spp.names))
    exp_wk1_catch <- sum(ships_per_fleet[grep('crab', fleets)]) * N['crab',yr,1] * catchability['crab'] * wt_at_rec['crab',yr]
    price_mat[1,'crab'] <- ifelse(exp_wk1_catch > crab_price_cutoff, 
                                  1, crab_price_pars[1] - crab_price_pars[2] * exp_wk1_catch)
    temp_catchability <- catchability

    for(wk in 1:wks_per_yr){
      if(sum(pop_seasons[,wk]) > 0) {
        exp_rev <- array(N[,yr,wk] * wt_at_rec[,yr] * temp_catchability * price_mat[wk,] * pop_seasons[,wk], 
                         dim=c(npops, nships, nfleets), 
                         dimnames = list(spp = spp.names, ships = NULL, fleet = fleets)) %>% 
          aperm(perm = c(2,3,1))
        
        exp_rev[,,'groundfish'] <- groundfish_bio[yr,wk] * temp_catchability['groundfish'] * 
          price_mat[wk,'groundfish'] * pop_seasons['groundfish',wk]
        # If out of legal season, expected revenue = 0. 
        # exp_rev does *not* care whether a fleet has a permit though! (That's in exp_profit)
        
        exp_profit <- exp_rev - cost_by_ship
        
        best_spp <- apply(exp_profit, 2, getBestSpp)
        # returns index of most profitable stock for each ship, or NA if no stock is profitable.
        
        if(any(!is.na(best_spp))) {
          ships_per_stock <- apply(best_spp, 2, tabulate, nbins = npops)
        } else {
          ships_per_stock <- matrix(0, nrow = npops, ncol = nfleets)
        }
        # counts up number of ships fishing each stock
        
        effort[,,wk,yr] <- ships_per_stock
        Catch[,yr,wk,] <- ships_per_stock * temp_catchability * N[,yr,wk]
        
        # Adjust crab stuff because it has a demand function. Baseline price is hard-coded in right now...
        price_mat[wk+1,'crab'] <- ifelse(sum(Catch['crab',yr,wk,grep('crab', fleets)]) > crab_price_cutoff, 1,
                                         crab_price_pars[1] - crab_price_pars[2] * 
                                           sum(Catch['crab',yr,wk,grep('crab', fleets)]) * wt_at_rec['crab',yr])

        exp_rev[,,'crab'] <- N['crab',yr,wk] * wt_at_rec['crab',yr] * temp_catchability['crab'] *
          price_mat[wk+1,'crab'] * pop_seasons['crab',wk]
        exp_profit[,,'crab'] <- exp_rev[,,'crab'] - cost_by_ship[,,'crab']
        
        for(fleet in 1:nfleets) {
          this_week_profit[,fleet] <- exp_profit[cbind(1:nships, fleet, best_spp[,fleet])]
          this_week_revenue[,fleet] <- exp_rev[cbind(1:nships, fleet, best_spp[,fleet])]
        }
        this_week_revenue[is.na(this_week_profit)] <- 0
        this_week_profit[is.na(this_week_profit)] <- 0
        
        profits[,yr,] <- profits[,yr,] + this_week_profit
        revenue[,yr,] <- revenue[,yr,] + this_week_revenue
        # actual profits are for the stock with max expected profit, if max is > 0. 
        
      } else {
        Catch[,yr,wk,] <- 0
      }
      
      if(wk < wks_per_yr) {
        N[,yr,wk+1] <- N[,yr,wk] - apply(Catch[,yr,wk,], 1, sum)
        # N['groundfish',yr,wk+1] <- N['groundfish',yr,wk] * groundfish_total_survival
        groundfish_bio[yr,wk+1] <- (1 - temp_catchability['groundfish'] * sum(effort['groundfish',,wk,yr])) * groundfish_bio[yr,wk]
      }
      
      if(sum(Catch['salmon',yr,,]) >= salmon_tac[yr]) {
        # if you hit the salmon TAC, set catchability to 0
        temp_catchability['salmon'] <- 0
        print(paste('salmon TAC engaged in week', wk, 'of year', yr))
      }
    }
    
    if(yr < nyrs) {
      groundfish_total_survival <- exp(-groundfish$M) * (1 - sum(Catch['groundfish',yr,,]) / N['groundfish',yr,1])
        
      N['groundfish',yr+1,1] <- N['groundfish',1] * groundfish_total_survival + groundfish_rec[1]
      groundfish_bio[yr+1,1] <- groundfish_total_survival * 
        (groundfish$alpha * N['groundfish',yr,1] + groundfish$rho * groundfish_bio[yr,wks_per_yr]) +
        groundfish_rec[1] ## assumes weight at recruitment = 1
      
      groundfish_rec[1:3] <- groundfish_rec[2:4]
      if(yr+4 < nyrs) {
        ssb <- groundfish_bio[yr,1] * (1 - sum(Catch['groundfish',yr,,]) / N['groundfish',yr,1])
        groundfish_rec[4] <- rec_devs['groundfish', yr+4] * exp(-groundfish$M_wk*wks_per_yr*4) * 
          groundfish$BH_a * ssb / (groundfish$BH_b + ssb)
      }
    }
  }
  
  out.list <- list(Catch = Catch, profits = profits, effort = effort, rec_devs = rec_devs, 
                   revenue = revenue, groundfish_bio = groundfish_bio)
  return(out.list)
}


