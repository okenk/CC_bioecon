make_half_baked_plots <- function(res.list, folder, sim.file.name, sim.id, base.value) {
  profit.df <- map_dfr(res.list, function(sim.par)
    map_dfr(sim.par, function(sim.res) {
      melt(sim.res$profits, value.name = 'profit') %>%
        as_tibble()
    },
    .id = 'sim_number'),
    .id = sim.id)
  
  revenue.df <- map_dfr(res.list, function(sim.par)
    map_dfr(sim.par, function(sim.res) {
      melt(sim.res$revenue, value.name = 'revenue') %>%
        as_tibble()
    },
    .id = 'sim_number'),
    .id = sim.id)
  
  income <- left_join(profit.df, revenue.df)
  income.summary <- group_by(income, get(sim.id), sim_number, fleet, ship) %>%
    summarize(profit.sd = sd(profit),
              profit.mn = mean(profit),
              revenue.sd = sd(revenue),
              revenue.mn = mean(revenue)) %>%
    mutate(revenue.cv = revenue.sd / revenue.mn) %>%
    rename(!!sim.id := `get(sim.id)`) %>%
    filter(revenue.mn > 0) # filters out ships with 0 revenue over a simulation, assume these did not have permits
  # (problem is that revenue and profit should be ragged arrays, but have dimensions based on max # of ships
  # in a fleet)
  
  # Individual Revenue CV
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    facet_wrap(~fleet, nrow = 2) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Revenue CV') +
    xlim(0,2) +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_rev.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  # Individual Revenue SD
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = revenue.sd, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    facet_wrap(~fleet, nrow = 2, scales = 'free_x') +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Revenue SD') +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_rev_sd.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  # Individual Revenue Mean
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
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_avg_rev.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  # Individual Profit SD
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
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_profit.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  # Individual Profit Mean
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
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_avg_prof.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)

  # Individual revenue CV aggregated over all fleets
  to.save <- ggplot(income.summary) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)),
                 alpha = .25) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Revenue CV') +
    xlim(0,2) +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_rev_agg.png'), plot = to.save, height = 5, width = 7,
         units = 'in', dpi = 500)
  
  # Gini index of mean revenue
  gini.index <- group_by(income, get(sim.id), sim_number, fleet, ship) %>%
    summarize(profit.sd = sd(profit),
              profit.mn = mean(profit),
              revenue.sd = sd(revenue),
              revenue.mn = mean(revenue)) %>%
    mutate(revenue.cv = revenue.sd / revenue.mn) %>%
    filter(revenue.mn > 0) %>%  # filters out ships with 0 revenue over a simulation, assume these did not have permits
    rename(!!sim.id := `get(sim.id)`) %>%
    ungroup() %>%
    group_by(get(sim.id), sim_number) %>%
    summarize(gini = DescTools::Gini(revenue.mn)) %>%
    rename(!!sim.id := `get(sim.id)`)
  
  to.save <- ggplot(gini.index) +
    geom_density(aes(x = gini, fill = get(sim.id)), alpha = .5) +
    guides(fill = guide_legend(title = sim.id)) 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_gini.png'), plot = to.save, height = 5, width = 7,
         units = 'in', dpi = 500)
  
  # Statistics for total revenue summed over all individuals
  total.rev <- group_by(income, get(sim.id), sim_number, yr) %>%
  summarize(revenue.total = sum(revenue),
            profit.total = sum(profit)) %>%
    summarize(profit.sd = sd(profit.total),
              profit.mn = mean(profit.total),
              revenue.sd = sd(revenue.total),
              revenue.mn = mean(revenue.total)) %>%
    mutate(revenue.cv = revenue.sd / revenue.mn) %>%
    rename(!!sim.id := `get(sim.id)`) %>%
    gather(key = 'metric', value = 'value', -(1:2))
  
  to.save <- ggplot(total.rev) +
    geom_density(aes(x = value, fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~metric, scales = 'free') +
    guides(fill = guide_legend(title = sim.id)) 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_whole_fleet.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  # Revenue by species
  revenue.df.spp <- map_dfr(res.list, function(sim.par)
    map_dfr(sim.par, function(sim.res) {
      melt(sim.res$revenue_spp, value.name = 'revenue') %>%
        as_tibble()
    },
    .id = 'sim_number'),
    .id = sim.id) %>%
    group_by(get(sim.id), spp, sim_number) %>%
    summarize(revenue.sd = sd(revenue),
              revenue.mn = mean(revenue)) %>%
    mutate(revenue.cv = revenue.sd / revenue.mn) %>%
    rename(!!sim.id := `get(sim.id)`)
  
  p.mn <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.mn, fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~spp, nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id)) 
  p.sd <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.sd, fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~spp, nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id)) 
  p.cv <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.cv, fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~spp, nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id)) 
  to.save <- gridExtra::arrangeGrob(p.mn, p.sd, p.cv, nrow = 3)
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_by_spp.png'), plot = to.save, height = 5, width = 7,
         units = 'in', dpi = 500)
}
