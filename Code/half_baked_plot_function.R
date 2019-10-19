make_half_baked_plots <- function(tibble.list, folder, sim.file.name, sim.id) {
  list2env(tibble.list, sys.frame(sys.nframe()))
  
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
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
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
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
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
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
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
  to.save <- ggplot(gini.index) +
    geom_density(aes(x = gini, col = get(sim.id), fill = get(sim.id)), alpha = .25) +
    guides(fill = guide_legend(title = sim.id)) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    xlab('Gini Index') +
    xlim(0,1) +
    guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
    NULL 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_gini.png'), plot = to.save, height = 5, width = 7,
         units = 'in', dpi = 500)
  
  # Statistics for total revenue summed over all individuals
  to.save <- ggplot(total.summary) +
    geom_density(aes(x = value, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~metric, scales = 'free') +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    NULL 
  
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_whole_fleet.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  # Revenue by species
  # revenue.df.spp <- map_dfr(res.list, function(sim.par)
  #   map_dfr(sim.par, function(sim.res) {
  #     apply(sim.res$revenue_spp, 1, function(rev.sim) 
  #       c(revenue.mn = mean(rev.sim), revenue.sd = sd(rev.sim))) %>%
  #       t() %>%
  #       as_tibble(rownames = 'spp') %>%
  #       mutate(revenue.cv = revenue.sd / revenue.mn)
  #   },
  #   .id = 'sim_number'),
  #   .id = sim.id)

  p.mn <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.mn, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~spp, nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          legend.key = element_rect(fill = "white")) + 
    scale_color_discrete(guide = guide_legend(override.aes = list(col = "white", fill = 'white'))) +
    NULL 
  p.sd <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.sd, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~spp, nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white")) +
    NULL 
  p.cv <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~spp, nrow = 1, scales = "free") +
    guides(fill = FALSE, col = FALSE) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill="white"),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          legend.key = element_rect(fill = "white")) + 
    scale_color_discrete(guide = guide_legend(override.aes = list(col = "white", fill = 'white'))) +
    NULL 
  to.save <- gridExtra::arrangeGrob(p.mn, p.sd, p.cv, nrow = 3)
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_by_spp.png'), plot = to.save, height = 5, width = 7,
         units = 'in', dpi = 500)
}
