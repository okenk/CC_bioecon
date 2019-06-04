# baseline ----------------------------------------------------------------

sim_pars$crab_price_pars <- c(1,0)
sim_pars$cost_cv <- 0
sim_pars$cost_corr <- 0
sim_pars$avg_rec[2] <- 1

xx <- uniroot(calc_var_cost, interval = c(-20,0),
              cost_cv = cost_cv, recruits = sim_pars$avg_rec['crab'], wt_at_rec = 1, price = c(1,0), #price['crab'], c(alpha_price, beta_price),
              fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['crab'], catchability = catchability['crab'], tac = NA)
sim_pars$cost_per_trip['crab'] <- exp(xx$root)

xx <- uniroot(calc_var_cost, interval = c(-20, 0),
              cost_cv = cost_cv, recruits = sim_pars$avg_rec['salmon'], wt_at_rec = 1, price = price['salmon'],
              fishing_season = pop_seasons['salmon',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['salmon'], catchability = catchability['salmon'], tac = salmon_tac_rule)
sim_pars$cost_per_trip['salmon'] <- exp(xx$root)

constant.cost.price <- run_sim(sim_pars, 230985)
Catch.df <-  melt(constant.cost.price$Catch, value.name = 'catch') %>%
  as_tibble() %>%
  left_join(melt(constant.cost.price$recruitment, value.name = 'recruitment'))

png('Figures/think_tank/crabs_caught1.png', res = 500, height = 5, width = 7, units = 'in')
Catch.df %>%
  group_by(spp, yr) %>%
  summarize(catch = sum(catch),
            rec = first(recruitment),
            prop.caught = catch/rec) %>%
  # summarize(mean(prop.caught)) 
  filter(spp == 'crab') %>%
  ggplot() +
  geom_line(aes(x=yr,y=prop.caught), lwd=2) +
  geom_hline(aes(yintercept = mean(prop.caught)), lty = 2) +
  ylim(0,1) +
  xlab('Year') + ylab('Proportion of crab recruits caught') +
  theme_classic(base_size = 16) +
  NULL
dev.off()

png('Figures/think_tank/effort1.png', res = 500, height = 5, width = 7, units = 'in')
melt(constant.cost.price$effort, value.name = 'n_ships') %>% 
  as_tibble() %>%
  filter(fleet == 'both') %>%
  filter(yr %in% c(7,14,25,26,32,35,41)) %>%
  ggplot() +
  geom_line(aes(x = wk, y = n_ships, group = paste(spp,yr), col = factor(yr)), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  xlab('Week of year') +
  ylab ('Effort (number of ships)') +
  guides(col = guide_legend(title = 'Year')) +
  theme_classic(base_size = 14) +
  NULL
dev.off()

# heterogenous fleet ------------------------------------------------------

sim_pars$cost_cv <- sqrt(log(.15^2+1))
sim_pars$cost_corr <- 0.7

xx <- uniroot(calc_var_cost, interval = c(-20,0),
              cost_cv = cost_cv, recruits = sim_pars$avg_rec['crab'], wt_at_rec = 1, price = c(1,0), #price['crab'], c(alpha_price, beta_price),
              fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['crab'], catchability = catchability['crab'], tac = NA)
sim_pars$cost_per_trip['crab'] <- exp(xx$root)

xx <- uniroot(calc_var_cost, interval = c(-20, 0),
              cost_cv = cost_cv, recruits = sim_pars$avg_rec['salmon'], wt_at_rec = 1, price = price['salmon'],
              fishing_season = pop_seasons['salmon',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['salmon'], catchability = catchability['salmon'], tac = salmon_tac_rule)
sim_pars$cost_per_trip['salmon'] <- exp(xx$root)

constant.price <- run_sim(sim_pars, 230985)

Catch.df <-  melt(constant.price$Catch, value.name = 'catch') %>%
  as_tibble() %>%
  left_join(melt(constant.price$recruitment, value.name = 'recruitment'))

png('Figures/think_tank/effort2.png', res = 500, height = 5, width = 7, units = 'in')
melt(constant.price$effort, value.name = 'n_ships') %>% 
  as_tibble() %>%
  filter(fleet == 'both') %>%
  filter(yr %in% c(7,14,25,26,32,35,41)) %>%
  ggplot() +
  geom_line(aes(x = wk, y = n_ships, group = paste(spp,yr), col = factor(yr)), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  xlab('Week of year') +
  ylab ('Effort (number of ships)') +
  guides(col = guide_legend(title = 'Year')) +
  theme_classic(base_size = 14) +
  NULL
dev.off()


# price hockey stick ------------------------------------------------------

sim_pars$crab_price_pars <- c(2,10)
xx <- uniroot(calc_var_cost, interval = c(-20,0),
              cost_cv = cost_cv, recruits = sim_pars$avg_rec['crab'], wt_at_rec = 1, price = c(2,10), #price['crab'], c(alpha_price, beta_price),
              fishing_season = pop_seasons['crab',], in_season_dpltn = TRUE, fleet_size = 200, 
              fixed_costs = fixed_costs['crab'], catchability = catchability['crab'], tac = NA)
sim_pars$cost_per_trip['crab'] <- exp(xx$root)

variable.cost.price <- run_sim(sim_pars, 230985)

Catch.df <-  melt(variable.cost.price$Catch, value.name = 'catch') %>%
  as_tibble() %>%
  left_join(melt(variable.cost.price$recruitment, value.name = 'recruitment'))

png('Figures/think_tank/effort3.png', res = 500, height = 4, width = 7, units = 'in')
melt(variable.cost.price$effort, value.name = 'n_ships') %>% 
  as_tibble() %>%
  filter(fleet == 'both') %>%
  filter(yr %in% c(7,14,25,26,32,35,41)) %>%
  ggplot() +
  geom_line(aes(x = wk, y = n_ships, group = paste(spp,yr), col = factor(yr)), alpha = 0.5) +
  facet_wrap(~spp, nrow = 2) +
  xlab('Week of year') +
  ylab ('Effort (number of ships)') +
  guides(col = guide_legend(title = 'Year')) +
  theme_classic(base_size = 14) +
  NULL
dev.off()


# scenario function -------------------------------------------------------

make_think_tank_plots(synchrony, 'sync', 'rec_corr', 0)
make_think_tank_plots(access, 'access', 'access', 'even access')
make_think_tank_plots(timing, 'timing', 'timing', 'normal')
