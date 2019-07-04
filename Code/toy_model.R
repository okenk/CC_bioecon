library(dplyr)
library(mvtnorm)
library(purrr)
source('Code/functions.R')
Rcpp::sourceCpp("Code/getBestSpp.cpp")

# Model parameters --------------------------------------------------------

spp.names <- c('crab', 'salmon', 'groundfish')
fleets <- c('crab', 'salmon', 'groundfish', 'crab-salmon', 'crab-groundfish', 'crab-salmon-groundfish')

wks_per_yr <- 52
nyrs <- 50
npops <- length(spp.names)
nfleets <- length(fleets)
ships_per_fleet <- rep(1, nfleets) * 67
nships <- max(ships_per_fleet)
# fleet_permits <- cbind(c(1,0,0), # crab fleet 
#                        c(0,1,0), # salmon fleet
#                        c(1,1,0)) # crab-salmon fleet

fleet_permits <- cbind(c(1,0,0), # crab fleet 
                       c(0,1,0), # salmon fleet
                       c(0,0,1), # groundfish fleet
                       c(1,1,0), # crab-salmon fleet
                       c(1,0,1), # crab-groundfish fleet
                       c(1,1,1)) # crab-salmon-groundfish fleet
dimnames(fleet_permits) <- list(spp = spp.names, fleet = fleets)
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

catchability <- c(.0005, .00005, 6.5*10^(-6));
# proportion of stock that will be caught by one fleet/ship during one week
# of fishing

price <- c(1, 1, 2)
avg_rec <- c(1, 1, 1)
avg_wt <- c(1, 1, 1)

weight_cv <- 0 # sqrt(log(0.2^2+1))
recruit_cv <- rep(sqrt(log(0.5^2+1)), npops)
recruit_ar <- c(.3,.3, 3)
recruit_corr <- 0

fixed_costs <- c(.0025, .0001, .00002)
cost_per_trip <- rep(NA, npops)
cost_cv <- sqrt(log(.15^2+1))
cost_corr <- 0.7

names(catchability) <- names(price) <- names(avg_rec) <- names(avg_wt) <- names(fixed_costs) <- names(cost_per_trip) <- 
  names(recruit_cv) <- names(recruit_ar) <- spp.names

salmon_tac_rule <- 0.3

crab_price_cutoff <- 0.1
crab_price_pars <- c(2,10)

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
                                sum(ships_per_fleet[c('crab', 'crab-salmon')]))

alpha_price <- 1 + beta_price * catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] * 
  sum(ships_per_fleet[c('crab', 'crab-salmon')])

# log function. Price increases too steeply at low catches.
beta_price <- price_increase/(log(catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                    (ships_per_fleet['crab'] + ships_per_fleet['crab-salmon'])) -
                                log(ref_catch * catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                      (ships_per_fleet['crab'] + ships_per_fleet['crab-salmon'])))
alpha_price <- 1 + beta_price * log(catchability['crab'] * avg_rec['crab'] * avg_wt['crab'] *
                                      (ships_per_fleet['crab'] + ships_per_fleet['crab-salmon']))

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

xx <- run_sim(sim_pars = sim_pars)

# Groundfish model setup --------------------------------------------------

## Step 1: Convert VBGF * weight-length relationship to the age-weight relationship for a D-D model 
##         (based on Ford-Walford plot). Is there a rigorous way to do this? Probably makes little difference...
a.max <- 50
ages <- map(1:a.max, ~ .x + 0:(wks_per_yr-1) / wks_per_yr) %>% flatten_dbl 
l1 <- 26
l2 <- 60
t1 <- .5
t2 <- 30
vb.k <- .37
lengths <- l1 + (l2-l1) * (1-exp(-vb.k*(ages-t1)))/(1-exp(-vb.k*(t2-t1)))
weights <- 3*10^(-6)*lengths^3.27
# recruitment to fishery at age 4
# fix weight @ recruitment to 1
age.4.ind <- which(ages==4)
weights <- weights/weights[age.4.ind]
mod <- lm(weights[age.4.ind:length(ages)] ~ weights[age.4.ind:length(ages)-1])
growth.al <- coef(mod)[1]
growth.rho <- coef(mod)[2]
w.r <- 1/2
w.r.minus.1 <- (w.r - growth.al) / growth.rho
weights.new <- numeric(length(ages) - 3*wks_per_yr) # start weights.new at age 4.0
weights.new[1] <- 1
for(ii in 2:length(weights.new)) {
  weights.new[ii] <- growth.al + growth.rho * weights.new[ii-1]
}
weights.new.yr <- weights.new[0:46 * wks_per_yr + 1]

## Step 2: Calculate S-R relationship parameters based on stock assessment assumed steepness of 0.6
M <- 0.07
M_wk <- M/wks_per_yr
# calculate age 1+ age structure (but assume S-R relationship is for recruitment at age 0)
age.struc <- map_dbl(1:a.max, function(.x) exp(-M * .x)) 
age.struc[a.max] <- age.struc[a.max] / (1 - exp(-M))
# Age 4 recruitment to fishery/spawning stock (assume same age)
sbpr0 <- sum(age.struc[4:a.max] * weights.new.yr)
steepness <- 0.6
R0 <- 1
a <- sbpr0 * (1 - (steepness - 0.2)/(0.8 * steepness))
b <- (steepness - 0.2) / (0.8 * steepness * R0)
groundfish <- list(M_wk = M_wk, BH_a = a, BH_b = b, alpha = growth.al, rho = growth.rho)

# I tried doing this based on equilibrium biomasses from Hilborn & Walters, but the answers didn't seem right.
# So I did it the brute force way instead. The answer was different. This seems more likely to be right.

nyrs <- 300 # checked, this is sufficient to reach equilibrium @ B0 (which is slowest to converge)
harvest.seq <- seq(from = 0, to = .15/wks_per_yr, length.out = 500)
yield.seq <- bio.seq <- N.seq <- rec.seq <- numeric(length(harvest.seq))

for(ii in 1:length(harvest.seq)) {
  harvest <- harvest.seq[ii]
  groundfish_bio <- groundfish_N <- matrix(0, nrow = nyrs, ncol = wks_per_yr)
  groundfish_bio[1,] <- groundfish_N[1,] <- 1
  groundfish_rec <- rep(1,4)

  for(yr in 2:nyrs) {
    yield <- 0
    # move this to the end. start loop at 1.
    groundfish_total_survival <- exp(-groundfish$M_wk) * (1-harvest)
    groundfish_N[yr,1] <- groundfish_N[yr-1,wks_per_yr] * groundfish_total_survival + groundfish_rec[1]
    groundfish_bio[yr,1] <- groundfish_total_survival * 
      (groundfish$alpha * groundfish_N[yr-1,wks_per_yr] + groundfish$rho * groundfish_bio[yr-1,wks_per_yr]) +
      groundfish_rec[1] ## assumes weight at recruitment = 1
    
    groundfish_rec[1:3] <- groundfish_rec[2:4]
    if(yr+4 < nyrs) {
      groundfish_rec[4] <- exp(-groundfish$M_wk*wks_per_yr*4) * 
        groundfish$BH_a * groundfish_bio[yr,1] / (groundfish$BH_b + groundfish_bio[yr,1])
    }
    
    for(wk in 1:(wks_per_yr-1)) {
      groundfish_total_survival <- exp(-groundfish$M_wk) * (1-harvest)
      groundfish_N[yr,wk+1] <- groundfish_N[yr,wk] * groundfish_total_survival
      groundfish_bio[yr,wk+1] <- groundfish_total_survival * 
        (groundfish$alpha * groundfish_N[yr,wk] + groundfish$rho * groundfish_bio[yr,wk])
      yield <- yield + harvest * groundfish_bio[yr,wk]
    }
    yield <- yield + harvest * groundfish_bio[yr,wks_per_yr]
  }
  yield.seq[ii] <- yield
  bio.seq[ii] <- groundfish_bio[nyrs,1]
  N.seq[ii] <- groundfish_N[nyrs,1]
  rec.seq[ii] <- groundfish_rec[1]
}

bio.seq[which.min(abs(bio.seq - 0.4 * bio.seq[1]))]
0.4 * bio.seq[1]
plot(harvest.seq, yield.seq, pch = '.')
plot(bio.seq, yield.seq, pch = '.')

R0 <- 1/bio.seq[find_closest(bio.seq, 0.4 * bio.seq[1])] # set R0 so B40 = 3
a <- 4 * steepness * R0 / (5 * steepness - 1)
b <- sbpr0 * (1 - steepness) / (5 * steepness - 1)
groundfish$BH_a <- a
groundfish$BH_b <- b
# Set initial conditions based on equilibrium results here.

xx <- uniroot(calc_var_cost_groundfish, interval = c(-20,0), cost_cv = cost_cv,
                         fishing_season = pop_seasons['groundfish',], bio_init = 1,
                         N1 = N.seq[find_closest(bio.seq, 1)], fleet_size = 200, in_season_dpltn = TRUE,
                         fixed_costs = .00002, catchability = 6.5*10^(-6), price = 2, tac = NA, groundfish = groundfish)

# calc_var_cost_groundfish(log_avg_cost_per_trip = -11.9, cost_cv = cost_cv, 
#               fishing_season = pop_seasons['groundfish',], bio_init = 1, 
#               N1 = N.seq[find_closest(bio.seq, 1)], fleet_size = 200, in_season_dpltn = TRUE, 
#               fixed_costs = .00002, catchability = 6.5*10^(-6), price = 1, tac = NA, groundfish = groundfish)
# 
# xx <- uniroot(calc_var_cost_groundfish, interval = c(-20,0), cost_cv = cost_cv, 
#               fishing_season = pop_seasons['groundfish',], 
#               bio_init = bio.seq[find_closest(yield.seq[1:295], 0.05718231)],
#               N1 = N.seq[find_closest(yield.seq[1:295], 0.05718231)], fleet_size = 200, in_season_dpltn = TRUE, 
#               fixed_costs = .00002, catchability = 6.5*10^(-6), price = 1, tac = NA, groundfish = groundfish)

groundfish$b_init <- 0.984438786700563
groundfish$N_init <- N.seq[find_closest(bio.seq, 0.984438786700563)]
groundfish$rec_init <- rec.seq[find_closest(bio.seq, 0.984438786700563)]
sim_pars$groundfish <- groundfish

sim_pars$cost_per_trip['groundfish'] <- exp(xx$root)


# I looked at the S-R relationship. The weekly growth/mortality but annual recruitment made actual R0 less than the
# parameter R0. However, steepness was still 0.6, so I am proceeding.

# Dan ?: I have avg rec for salmon, crabs = 1. What is equivalent for groundfish? B_MSY = 1? Initial condition?

# Beverton-Holt S-R relationship: R = a*SSB/(b+SSB)
# aim for SSB_MSY = 1 instead of R0 = 1? Or something else?

## Groundfish to do:
## 1. Change population dynamics to annual for cost setting and simulation functions. Can then use analytic equilibrium conditions.
## 2.  

## Good coding practices:
## a) All of this could be written to be conditional on having a stock called "groundfish"
## b) Right now, age at recruitment to fishery (4) is hard-coded in. Make this flexible? (Unclear age 4 is best choice?)
