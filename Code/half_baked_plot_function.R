make_half_baked_plots <- function(res.list, sim.file.name, sim.id, base.value) {
  Catch.df <- map_dfr(res.list, function(corr.val)
    map_dfr(corr.val, function(sim.res) {
      melt(sim.res$Catch, value.name = 'catch') %>%
        as_tibble() %>%
        left_join(melt(sim.res$rec_devs, value.name = 'rec_devs'))
    }, 
    .id = 'sim_number'),
    .id = sim.id)
  
  effort.df <- map_dfr(res.list, function(corr.val)
    map_dfr(corr.val, function(sim.res) {
      melt(sim.res$effort, value.name = 'n_ships') %>%
        as_tibble() %>%
        left_join(melt(sim.res$rec_devs, value.name = 'rec_devs'))
    }, 
    .id = 'sim_number'),
    .id = sim.id)
  
  cpue <- left_join(Catch.df, effort.df)
  yrs <- filter(cpue, sim_number=='1', get(sim.id) == base.value) %>%
    group_by(yr) %>%
    summarize(rec = first(rec_devs)) %>%
    arrange(rec) %>%
    slice(c(3, 15, 27, 39, 50)) %>%
    with(yr)
  
  plt <- filter(cpue, n_ships > 0, sim_number == '1') %>%
    ggplot(aes(x=wk, group=yr)) +
    facet_grid(get(sim.id) ~ spp + fleet, scales = 'free_y') +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Week of year') +
    guides(col = guide_legend(title = 'Year')) +
    NULL
  
  to.save <- plt +
    geom_line(aes(y=catch), alpha = .25, lwd=.1) +
    geom_line(data = filter(cpue, sim_number == '1', n_ships > 0, 
                            yr %in% yrs),
              aes(x = wk, y = catch, group = yr, col = factor(yr))) +
    ylab('Catch (weight)') 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_catch.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- plt +
    geom_line(aes(y=n_ships), alpha = .25, lwd=.1) +
    geom_line(data = filter(cpue, sim_number == '1', n_ships > 0,
                            yr %in% yrs),
              aes(x = wk, y = n_ships, group = yr, col = factor(yr))) +
    ylab('Effort (number of ships)') 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_effort.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- plt +
    geom_line(aes(y=catch/n_ships), alpha = .25, lwd=.1) +
    geom_line(data = filter(cpue, sim_number == '1', n_ships > 0,
                            yr %in% yrs),
              aes(x = wk, y = catch/n_ships, group = yr, col = factor(yr))) +
    ylab('CPUE') 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_cpue.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  profit.df <- map_dfr(res.list, function(corr.val)
    map_dfr(corr.val, function(sim.res) {
      melt(sim.res$profits, value.name = 'profit') %>%
        as_tibble()
    },
    .id = 'sim_number'),
    .id = sim.id)
  
  revenue.df <- map_dfr(res.list, function(corr.val)
    map_dfr(corr.val, function(sim.res) {
      melt(sim.res$revenue, value.name = 'revenue') %>%
        as_tibble()
    },
    .id = 'sim_number'),
    .id = sim.id)

  income <- left_join(profit.df, revenue.df)
  income.summary <- group_by(income, get(sim.id), sim_number, fleet, ship) %>%
    summarize(profit.sd = sd(profit),
              profit.mn = mean(profit),
              revenue.cv = sd(revenue)/mean(revenue),
              revenue.mn = mean(revenue)) %>%
    rename(!!sim.id := `get(sim.id)`) %>%
    filter(revenue.mn > 0) # filters out ships with 0 revenue over a simulation, assume these did not have permits
  # (problem is that revenue and profit should be ragged arrays, but have dimensions based on max # of ships
  # in a fleet)
  
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    facet_wrap(~fleet, nrow = 2) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Revenue CV') +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_rev.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = revenue.mn, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    facet_wrap(~fleet, nrow = 2, scales = 'free_x') +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Mean revenue') +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_avg_rev.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = profit.sd, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    facet_wrap(~fleet, scales = 'free') +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Profit SD') +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_profit.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = profit.mn, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    facet_wrap(~fleet, scales = 'free_x') +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Mean profit') +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_avg_prof.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)

    to.save <- ggplot(income.summary) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Revenue CV') +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/half_baked/', sim.file.name, '_rev_agg.png'), plot = to.save, height = 5, width = 7,
         units = 'in', dpi = 500)
  
}
