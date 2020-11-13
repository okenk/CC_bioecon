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


# portfolio benefit -------------------------------------------------------
pal <- wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1,2)]
names(pal) <- c('-0.5', '0', '0.5')

base.plot <- sync_tibbles$income.summary %>%
  prep_individuals_to_plot(experiment = 'synchrony') %>%
  group_by(synchrony, fleet) %>%
  summarize_at('revenue.cv', .funs = list(v.low = ~ quantile(., 0.025), low = ~ quantile (., 0.25), 
                                          mid = median, high = ~ quantile(., 0.75),
                                          v.high = ~ quantile(., 0.975))) %>%
  pivot_longer(cols = v.low:v.high, names_to = 'quantile', values_to = 'value') %>%
  filter(fleet != 'Salmon', fleet != 'Groundfish') %>%
  pivot_wider(names_from = fleet, values_from = value) %>%
  mutate_at(vars(`Crab-Salmon`:`Crab-Salmon-\nGroundfish`), .funs = list(~ Crab/.)) %>%
  select(-Crab) %>%
  pivot_longer(cols = `Crab-Salmon`:`Crab-Salmon-\nGroundfish`, names_to = 'fleet',
               values_to = 'portfolio_benefit') %>%
  mutate(quantile = recode(quantile, v.low = .025, low = .25, mid = .5, high = .75, v.high = .975),
         fleet = fct_inorder(str_to_title(fleet))) %>%
  ggplot(aes(x = quantile, y = portfolio_benefit, col = synchrony)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ fleet, ncol = 3) +
  ylim(0.25, 1.75) +
  ggsidekick::theme_sleek(base_size = 14) +
  scale_color_manual(values = pal, name = 'Synchrony') +
  scale_x_continuous(name = 'Revenue CV quantile', labels = c('0', '0.25', '0.5', '0.75', '1.0')) +
  # xlab('Revenue CV quantile') +
  ylab('Portfolio benefit') +
  # labs(col = 'Synchrony') +
  NULL

base.plot %+%
  filter(base.plot$data, synchrony == '0') %>%
  ggsave(filename = 'Figures/Presentation/portfolio_benefits1.png', plot = ., height = 4, width = 9,
         units = 'in', dpi = 500)

ggsave('Figures/Presentation/portfolio_benefits2.png', base.plot, height = 4, width = 9,
       units = 'in', dpi = 500)



# Gini index --------------------------------------------------------------

to.save <- access_tibbles$gini.index %>%
  ungroup() %>%
  mutate(access = str_to_title(access)) %>% 
  mutate(access = str_replace(access, ' ', '\n')) %>%
  group_by(access) %>%
  summarize(mn = mean(gini),
            upper = quantile(gini, probs = .975),
            lower = quantile(gini, probs = 0.025)) %>%
  ggplot() +
  geom_col(aes(x = access, y = mn, fill = access)) +
  geom_linerange(aes(x = access, ymin = lower, ymax = upper), col = 'gray40') +
  ylim(0,1) +
  labs(x = '', y = 'Gini index', fill = 'Access') +
  scale_fill_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1,2)])
ggsave('Figures/Presentation/gini.png', plot = to.save, dpi = 500, height = 5, width = 7, units = 'in')
