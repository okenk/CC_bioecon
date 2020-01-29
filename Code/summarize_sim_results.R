summarize_sim_results <- function(res.list, sim.id) {
  # Calculate avg and sd of profit for each vessel in each fleet in each simulation
  print('starting profit df')
  profit.df <- map_dfr(res.list, function(sim.par)
    future_map_dfr(sim.par, function(sim.res) {
      apply(sim.res$profits, c(1,3), function(profit.sim)
        c(profit.mn = mean(profit.sim), profit.sd = sd(profit.sim))) %>%
        melt() %>%
        spread(key = Var1, value = value) # %>%
        # as_tibble()
    },
    .id = 'sim_number'),
    .id = sim.id)

  # Calculate avg, sd, cv of revenue for each vessel in each fleet in each simulation
  print('starting revenue df')
  revenue.df <- map_dfr(res.list, function(sim.par)
    future_map_dfr(sim.par, function(sim.res) {
      apply(sim.res$revenue, c(1,3), function(rev.sim) 
        c(revenue.mn = mean(rev.sim), revenue.sd = sd(rev.sim))) %>%
        melt() %>% 
        pivot_wider(names_from = Var1, values_from = value) %>%
        # as_tibble() %>%
        mutate(revenue.cv = revenue.sd / revenue.mn)
    },
    .id = 'sim_number'),
    .id = sim.id)
  
  income.summary <- left_join(profit.df, revenue.df) %>%
    filter(revenue.mn > 0)
  
  gini.index <- group_by(income.summary, get(sim.id), sim_number) %>%
    summarize(gini = DescTools::Gini(revenue.mn)) %>%
    rename(!!sim.id := `get(sim.id)`)
  
  # Calculate avg, sd profit summed across vessels for each fleet in each simulation
  total.profit <- map_dfr(res.list, function(sim.par)
    future_map_dfr(sim.par, function(sim.res) {
      tot <- apply(sim.res$profits, 2, sum)
      data.frame(`Mean profit` = mean(tot), `Profit SD` = sd(tot))
    },
    .id = 'sim_number'),
    .id = sim.id) %>%
    gather(key = 'metric', value = 'value', -(1:2))
  
  # Calculate avg, sd, cv revenue summed across vessels for each fleet in each sim
  total.revenue <- map_dfr(res.list, function(sim.par)
    future_map_dfr(sim.par, function(sim.res) {
      tot <- apply(sim.res$revenue, 2, sum)
      data.frame(`Mean revenue` = mean(tot), `Revenue SD` = sd(tot))
    },
    .id = 'sim_number'),
    .id = sim.id) %>%
    mutate(`Revenue CV` = `Revenue SD` / `Mean revenue`) %>%
    gather(key = 'metric', value = 'value', -(1:2))
  
  total.summary <- bind_rows(total.profit, total.revenue)
  
  # Calculate avg, sd, cv revenue summed across vessels for each SPECIES in each sim
  revenue.df.spp <- map_dfr(res.list, function(sim.par)
    map_dfr(sim.par, function(sim.res) {
      apply(sim.res$revenue_spp, 1, function(rev.sim) 
        c(revenue.mn = mean(rev.sim), revenue.sd = sd(rev.sim))) %>%
        t() %>%
        as_tibble(rownames = 'spp') %>%
        mutate(revenue.cv = revenue.sd / revenue.mn)
    },
    .id = 'sim_number'),
    .id = sim.id)
  
  out.list <- list(income.summary = income.summary, gini.index = gini.index, total.summary = total.summary, revenue.df.spp = revenue.df.spp)
  return(out.list)
}