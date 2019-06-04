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

png('Figures/think_tank/effort3.png', res = 500, height = 5, width = 7, units = 'in')
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

make_think_tank_plots <- function(res.list, sim.file.name, sim.id) {
  Catch.df <- map_dfr(res.list, function(corr.val)
    map_dfr(corr.val, function(sim.res) {
      melt(sim.res$Catch, value.name = 'catch') %>%
        as_tibble() %>%
        left_join(melt(sim.res$recruitment, value.name = 'recruitment'))
    }, 
    .id = 'sim_number'),
    .id = sim.id)
  
  effort.df <- map_dfr(res.list, function(corr.val)
    map_dfr(corr.val, function(sim.res) {
      melt(sim.res$effort, value.name = 'n_ships') %>%
        as_tibble() %>%
        left_join(melt(sim.res$recruitment, value.name = 'recruitment'))
    }, 
    .id = 'sim_number'),
    .id = sim.id)
  
  cpue <- left_join(Catch.df, effort.df)
  yrs <- filter(cpue, sim_number=='1', rec_corr == 0) %>%
    group_by(yr) %>%
    summarize(rec = first(recruitment)) %>%
    arrange(rec) %>%
    slice(c(3, 15, 27, 39, 50)) %>%
    with(yr)
  
  plt <- filter(cpue, n_ships > 0, sim_number == '1') %>%
    ggplot(aes(x=wk, group=yr)) +
    facet_grid(spp + fleet ~ rec_corr, scales = 'free_y') +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Week of year') +
    guides(col = guide_legend(title = 'Year')) +
    NULL
  
  # png(paste0('Figures/think_tank/', sim.file.name, '_catch.png'), height = 5, width = 7, units = 'in',
  #     res = 500)
  xx <- plt +
    geom_line(aes(y=catch), alpha = .25, lwd=.1) +
    geom_line(data = filter(cpue, sim_number == '1', n_ships > 0, 
                            yr %in% yrs),
              aes(x = wk, y = catch, group = yr, col = factor(yr))) +
    ylab('Catch (weight)') 
  ggsave(filename = 'poop.png', plot = xx)
  # 
  # png(paste0('Figures/think_tank/', sim.file.name, '_effort.png'), height = 5, width = 7, units = 'in',
  #     res = 500)
  # plt +
  #   geom_line(aes(y=n_ships), alpha = .25, lwd=.1) +
  #   geom_line(data = filter(cpue, sim_number == '1', n_ships > 0,
  #                           yr %in% yrs),
  #             aes(x = wk, y = n_ships, group = yr, col = factor(yr))) +
  #   ylab('Effort (number of ships)') %>%
  #   print()
  # dev.off()
  # 
  # png(paste0('Figures/think_tank/', sim.file.name, '_cpue.png'), height = 5, width = 7, units = 'in',
  #     res = 500)
  # plt +
  #   geom_line(aes(y=catch/n_ships), alpha = .25, lwd=.1) +
  #   geom_line(data = filter(cpue, sim_number == '1', n_ships > 0,
  #                           yr %in% yrs),
  #             aes(x = wk, y = catch/n_ships, group = yr, col = factor(yr))) +
  #   ylab('CPUE') %>%
  #   print()
  # dev.off()

  # profit.df <- map_dfr(res.list, function(corr.val)
  #   map_dfr(corr.val, function(sim.res) {
  #     melt(sim.res$profits, value.name = 'profit') %>%
  #       as_tibble()
  #   },
  #   .id = 'sim_number'),
  #   .id = sim.id)
  # 
  # revenue.df <- map_dfr(res.list, function(corr.val)
  #   map_dfr(corr.val, function(sim.res) {
  #     melt(sim.res$revenue, value.name = 'revenue') %>%
  #       as_tibble()
  #   },
  #   .id = 'sim_number'),
  #   .id = sim.id)
  # 
  # income <- left_join(profit.df, revenue.df)
  # income.summary <- group_by(income, rec_corr, sim_number, fleet, ship) %>%
  #   summarize(profit.sd = sd(profit),
  #             profit.mn = mean(profit),
  #             revenue.cv = sd(revenue)/mean(revenue),
  #             revenue.mn = mean(revenue))
  # 
  # png(paste0('Figures/think_tank/', sim.file.name, '_rev.png'), height = 3, width = 7, units = 'in',
  #     res = 500)
  # ggplot(income.summary) +
  #   geom_density(aes(x = revenue.cv, col = rec_corr, fill = rec_corr),
  #                alpha = .25) +
  #   facet_wrap(~fleet) +
  #   theme_bw(base_size = 14) +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_rect(fill="white")) +
  #   xlab('Revenue CV') +
  #   NULL %>%
  #   print()
  # dev.off()
  # 
  # png(paste0('Figures/think_tank/', sim.file.name, '_profit.png'), height = 3, width = 7, units = 'in',
  #     res = 500)
  # ggplot(income.summary) +
  #   geom_density(aes(x = profit.sd, col = rec_corr, fill = rec_corr),
  #                alpha = .25) +
  #   facet_wrap(~fleet) +
  #   theme_bw(base_size = 14) +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_rect(fill="white")) +
  #   xlab('Profit SD') +
  #   NULL %>%
  #   print()
  # dev.off()
}

make_think_tank_plots(synchrony, 'sync', 'rec_corr')

