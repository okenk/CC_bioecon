library(tidyverse)

  
make_pices_plots <- function(tibble.list, folder, sim.file.name, sim.id) {
  list2env(tibble.list, sys.frame(sys.nframe()))
  theme_set(ggsidekick::theme_sleek(base_size = 14)) 
  
  total.summary <- spread(tibble.list$total.summary, key = metric, value = value)  
  p.mn <- ggplot(total.summary) +
    geom_density(aes(x = revenue.mn, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    xlab('Mean revenue') +
    scale_color_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    scale_fill_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    NULL 
  
  p.cv <-  ggplot(total.summary) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    xlab('Revenue CV') +
    scale_color_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    scale_fill_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    NULL 
  
  to.save <- gridExtra::arrangeGrob(p.mn, p.cv, nrow = 1)
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_mn_cv.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.mn, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~str_to_title(spp), nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    xlab('Mean revenue') +
    scale_color_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    scale_fill_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    NULL 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_rev_spp_mn.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
  
  to.save <- ggplot(revenue.df.spp) +
    geom_density(aes(x = revenue.cv, col = get(sim.id), fill = get(sim.id)), alpha = 0.25) +
    facet_wrap(~str_to_title(spp), nrow = 1, scales = "free") +
    guides(fill = guide_legend(title = sim.id), col = guide_legend(title = sim.id)) +
    xlab('Revenue CV') +
    scale_color_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    scale_fill_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)]) +
    NULL 
  ggsave(filename = paste0('Figures/', folder, '/', sim.file.name, '_rev_spp_cv.png'), plot = to.save, height = 4, width = 9,
         units = 'in', dpi = 500)
}

load('Data/10k_5_20/sync_df_10-8_1k.RData')
load('Data/10k_5_20/access_df_10-8_1k.RData')

make_pices_plots(sync_tibbles, 'Presentation', 'sync', 'synchrony')
make_pices_plots(access_tibbles, 'Presentation', 'access', 'access')

make_half_baked_plots(sync_tibbles, 'Presentation', 'sync', 'synchrony')
make_half_baked_plots(access_tibbles, 'Presentation', 'access', 'access')