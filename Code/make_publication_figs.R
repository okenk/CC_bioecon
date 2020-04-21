library(tidyverse)
library(stringr)
library(getBestSpp)
library(reshape2)
library(beyonce)
library(gridExtra)

source('Code/functions.R')
source('Code/toy_model.R')

load('Data/10k/access_df_10-8_1k.RData')
load('Data/10k/sync_df_10-8_1k.RData')
load('Data/10k/sync_access_df_10-8_1k.RData')

make_individual_plots <- function(filename, df, pars_to_plot, xaxis_labs, experiment, facet_var, png_width) {
  if(length(pars_to_plot) != length(xaxis_labs)) {
    print('Error: number of parameters to plot does not match number of axis labels!')
    break
  }
  
  n_scenarios <- nlevels(factor(df[[experiment]]))
  pal <- colorRampPalette(wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)])
    
  plt.ls <- map2(pars_to_plot, xaxis_labs, function(par_to_plot, xaxis_lab) {
    to.plot <- df %>%
      ggplot() +
      geom_density(aes(x = get(par_to_plot), col = get(experiment), fill = get(experiment)),
                   alpha = .25) +
      facet_wrap(~ get(facet_var), nrow = 1, scales = 'free_x') +
      ggsidekick::theme_sleek(base_size = 12) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      labs(x = xaxis_lab, col = str_to_title(experiment), fill = str_to_title(experiment)) +
      scale_color_manual(values = pal(n_scenarios)) +
      scale_fill_manual(values = pal(n_scenarios)) +
      theme(axis.text = element_text(size = 5),
            axis.title.y = element_blank()) +
      NULL
    if(par_to_plot != pars_to_plot[1]) {
      to.plot <- to.plot +
        theme(strip.text.x = element_blank())
    }
    if(par_to_plot == pars_to_plot[length(pars_to_plot)]) {
      to.plot <- to.plot +
        theme(legend.position = 'bottom')
    } else {
      to.plot <- to.plot +
        theme(legend.position = 'none')
    }
    return(to.plot)
  })
  
  # png(paste0('Figures/pub_figs/', filename, '.png'), res = 500, units = 'in', width = 7,
  #     height = 2*length(pars_to_plot))
  ggsave(filename = paste0('Figures/pub_figs/', filename, '.png'), 
         plot = grid.arrange(grobs = plt.ls, nrow = 2, 
                             left = textGrob("Density", rot = 90, gp=gpar(col = 'gray30'))), 
         height = 2*length(pars_to_plot), width = png_width, units = 'in', dpi = 500) 
  # dev.off()
}

prep_individuals_to_plot <- function(df, experiment) {
  df %>%
    group_by(get(experiment), fleet, sim_number) %>%
    summarize(profit.mn = mean(profit.mn),
              profit.sd = mean(profit.sd), 
              revenue.mn = mean(revenue.mn),
              revenue.sd = mean(revenue.sd),
              revenue.cv = mean(revenue.cv)) %>%
    ungroup() %>%
    rename(!!experiment := `get(experiment)`) %>%
    mutate(fleet = str_to_title(fleet),
           fleet = factor(fleet, levels = c('Crab', 'Salmon', 'Groundfish', 'Crab-Salmon', 'Crab-Groundfish',
                                            'Crab-Salmon-Groundfish')),
           fleet = recode(fleet, `Crab-Salmon-Groundfish` = 'Crab-Salmon-\nGroundfish'))
}


# Species ------------------------------------------------------
access_tibbles$revenue.df.spp %>%
  mutate(spp = str_to_title(spp),
         spp = factor(spp, levels = c('Crab', 'Salmon', 'Groundfish'))) %>%
  make_individual_plots(filename = 'access_spp', df = ., experiment = 'access',
                      pars_to_plot = c('revenue.mn', 'revenue.cv'),
                      xaxis_labs = c('Mean revenue', 'Revenue CV'), facet_var = 'spp', png_width = 5)

sync_tibbles$revenue.df.spp %>%
  mutate(spp = str_to_title(spp),
         spp = factor(spp, levels = c('Crab', 'Salmon', 'Groundfish'))) %>%
  make_individual_plots(filename = 'sync_spp', df = ., experiment = 'synchrony',
               pars_to_plot = c('revenue.mn', 'revenue.cv'),
               xaxis_labs = c('Mean revenue', 'Revenue CV'), facet_var = 'spp', png_width = 5)

sync_access_tibbles$revenue.df.spp %>%
  mutate(spp = str_to_title(spp),
         spp = factor(spp, levels = c('Crab', 'Salmon', 'Groundfish'))) %>%
  make_individual_plots(filename = 'sync_access_spp', df = ., experiment = 'sync_access',
                 pars_to_plot = c('revenue.mn', 'revenue.cv'),
                 xaxis_labs = c('Mean revenue', 'Revenue CV'), facet_var = 'spp', png_width = 5)

  

# Individuals ----------------------------------------------------------

access_tibbles$income.summary %>%
  prep_individuals_to_plot(experiment = 'access') %>%
  make_individual_plots(filename = 'access_individuals', df = ., experiment = 'access',
                        pars_to_plot = c('revenue.mn', 'revenue.cv'),
                        xaxis_labs = c('Mean revenue', 'Revenue CV'), facet_var = 'fleet', png_width = 7)

sync_tibbles$income.summary %>%
  prep_individuals_to_plot(experiment = 'synchrony') %>%
  make_individual_plots(filename = 'sync_individuals', df = ., experiment = 'synchrony',
                        pars_to_plot = c('revenue.mn', 'revenue.cv'),
                        xaxis_labs = c('Mean revenue', 'Revenue CV'), facet_var = 'fleet', png_width = 7)

sync_access_tibbles$income.summary %>%
  prep_individuals_to_plot(experiment = 'sync_access') %>%
  make_individual_plots(filename = 'sync_access_individuals', df = ., experiment = 'sync_access',
                        pars_to_plot = c('revenue.mn', 'revenue.cv'),
                        xaxis_labs = c('Mean revenue', 'Revenue CV'), facet_var = 'fleet', png_width = 7)


# access individuals aggregated -------------------------------------------

png('Figures/pub_figs/access_individuals_agg.png', width = 4, height = 4, res = 500, units = 'in')
ggplot(access_tibbles$income.summary) +
  stat_density(aes(x = revenue.cv, col = access), geom = 'line', position = 'identity') +
  scale_color_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)], 
                     name = 'Access',
                     labels = str_to_title) +
  ggsidekick::theme_sleek() +
  theme(legend.position = c(.75, .75)) +
  xlim(0,2) +
  xlab('Revenue CV') +
  NULL 
dev.off()


# broad summary table -----------------------------------------------------

to.print <- matrix('', nrow = 8, ncol = 3, dimnames = list(rep('', 8), c('Mean revenue', 'Revenue CV', 
                                                                         'Gini index')))
temp <- access_tibbles$total.summary %>%
  pivot_wider(names_from = metric) %>%
  group_by(access) %>%
  summarize(revenue.mn = mean(revenue.mn), revenue.cv = mean(revenue.cv))

to.print[2:4, 1:2] <- temp %>%
  select(-access) %>%
  as.matrix %>%
  round(2)

rownames(to.print)[1] <- 'Access'
rownames(to.print)[2:4] <- str_to_title(temp$access) %>%
  str_c('   ', .)

temp <- access_tibbles$gini.index %>%
  group_by(access) %>%
  summarize(gini = mean(gini))
to.print[2:4,3] <- round(temp$gini, 2)

temp <- sync_tibbles$total.summary %>%
  pivot_wider(names_from = metric) %>%
  group_by(synchrony) %>%
  summarize(revenue.mn = mean(revenue.mn), revenue.cv = mean(revenue.cv))

to.print[6:8, 1:2] <- temp %>%
  select(-synchrony) %>%
  as.matrix %>%
  round(2)

rownames(to.print)[5] <- 'Synchrony'
rownames(to.print)[6:8] <- str_c('   ', c('Asynchronous', 'Independent', 'Synchronous'))

temp <- sync_tibbles$gini.index %>%
  group_by(synchrony) %>%
  summarize(gini = mean(gini))
to.print[6:8,3] <- round(temp$gini, 2)

write.csv(to.print, file = 'Figures/pub_figs/results_summary.csv')

# effect of synchrony on portfolio effects --------------------------------

png('Figures/pub_figs/portfolio_benefits.png', width = 7, height = 4, units = 'in', res = 500)
sync_tibbles$income.summary %>%
  group_by(synchrony, fleet) %>%
  summarize_at('revenue.cv', .funs = list(v.low = ~ quantile(., 0.025), low = ~ quantile (., 0.25), 
                                     mid = median, high = ~ quantile(., 0.75),
                                     v.high = ~ quantile(., 0.975))) %>%
  mutate(fleet = recode(fleet, `crab-salmon-groundfish` = 'crab-salmon-\ngroundfish')) %>%
  pivot_longer(cols = v.low:v.high, names_to = 'quantile', values_to = 'value') %>%
  filter(fleet != 'salmon', fleet != 'groundfish') %>%
  pivot_wider(names_from = fleet, values_from = value) %>%
  mutate_at(vars(`crab-salmon`:`crab-salmon-\ngroundfish`), .funs = list(~ crab/.)) %>%
  select(-crab) %>%
  pivot_longer(cols = `crab-salmon`:`crab-salmon-\ngroundfish`, names_to = 'fleet',
               values_to = 'portfolio_benefit') %>%
  mutate(quantile = recode(quantile, v.low = .025, low = .25, mid = .5, high = .75, v.high = .975),
         fleet = str_to_title(fleet)) %>%
  ggplot(aes(x = quantile, y = portfolio_benefit, col = synchrony)) +
  geom_hline(yintercept = 1, lty = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ fleet, ncol = 3) +
  ylim(0.25, 1.75) +
  ggsidekick::theme_sleek(base_size = 12) +
  scale_color_manual(values = wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1,2)], name = 'Synchrony') +
  scale_x_continuous(name = 'Revenue CV quantile', labels = c('0', '0.25', '0.5', '0.75', '1.0')) +
  # xlab('Revenue CV quantile') +
  ylab('Portfolio benefit') +
  # labs(col = 'Synchrony') +
  NULL
dev.off()


# Example sim -------------------------------------------------------------

set.seed(83209)
one.sim <- run_sim(sim_pars)
catch.df <- apply(one.sim$Catch, 1:3, sum) %>%
  melt(value.name = 'Catch') %>%
  left_join(melt(one.sim$rec_devs, value.name = 'rec_devs')) %>%
  as_tibble

yrs <- catch.df %>%
  group_by(yr) %>%
  summarize(rec = first(rec_devs)) %>%
  arrange(rec) %>% 
  slice(c(3, 15, 27, 39, 50)) %>%
  with(yr)

to.save <- catch.df %>%
  ggplot() +
  geom_line(aes(x = wk, y = Catch, group = yr), alpha = 0.25, lwd = 0.1) +
  geom_line(data = filter(catch.df, yr %in% yrs),
            aes(x = wk, y = Catch, group = yr, col = factor(yr)), lwd = 1, show.legend = FALSE) + 
  facet_wrap(~str_to_title(spp), nrow = 1, scales = 'free_y') +
  ggsidekick::theme_sleek(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white")) +
  xlab('Week of year') +
  ylab('Catch (weight)') +
  scale_color_manual(values = beyonce_palette(127, n = 6)[2:6]) +
  NULL
ggsave(filename = 'Figures/pub_figs/catch_ex.png', plot = to.save, height = 4, width = 7, units = 'in', 
       dpi = 500)


# groundfish biomass check ------------------------------------------------

set.seed(83209)
hundred.sims <- map(1:100, run_sim, sim_pars = sim_pars)
names(hundred.sims) <- 1:100
to.save <- map_dfr(.x = hundred.sims, function(.x) .x$groundfish_bio[,1]) %>%
  mutate(year = 1:50) %>%
  pivot_longer(cols = -year, names_to = 'sim', values_to = 'biomass') %>%
  filter(year < 50) %>%
  ggplot() +
  geom_line(aes(x = year, y = biomass, group = sim)) +
  stat_smooth(aes(x = year, y = biomass)) +
  geom_hline(aes(yintercept = mean(biomass)), col = 'red') +
  ggsidekick::theme_sleek(base_size = 12) +
  NULL
ggsave('Figures/pub_figs/groundfish_check.png', height = 6, width = 7, units = 'in', dpi = 500)


# Old plot code -----------------------------------------------------------

# temp <- df %>%
#   group_by_at(c(experiment, 'fleet')) %>%
#   summarize_at(pars_to_plot, .funs = list(v.low = ~ quantile(., 0.025), low = ~ quantile (., 0.25), 
#                                           mid = median, high = ~ quantile(., 0.75),
#                                           v.high = ~ quantile(., 0.975))) %>%
#   mutate(fleet = recode(fleet, `crab-salmon-groundfish` = 'crab-salmon-\ngroundfish')) %>%
#   group_by(fleet)
# 
# png(paste0('Figures/pub_figs/', filename, '.png'), res = 500, units = 'in', width = 7,
#     height = 2*length(pars_to_plot) + ifelse(make.legend, 1, 0))
# mfrow.row <- ifelse(make.legend, length(pars_to_plot) + 1, pars_to_plot)
# par(mfrow = c(mfrow.row, 6), mar = c(.5, .5, .5, .5), oma = c(5, 5, 4, 1), col = 'gray30', 
#     col.axis = 'gray30')
# if(make.legend) {
#   for(ii in 1:length(unique(df$fleet))) {
#     plot(1,1, type = 'n', ann = FALSE, axes = FALSE)
#   }
#   legend('center', legend = unique(temp[[experiment]]), col = pal(n_scenarios), pch = 15, cex = .75, 
#          box.col = 'gray70')
# }
# 
# for(ii in 1:length(pars_to_plot)) {
#   temp %>%
#     group_map(.f = function(.x, .y) {
#       plot(1:n_scenarios, 1:n_scenarios, type = 'n', ylim = c(0, quantile(df[[pars_to_plot[ii]]], 0.999)), 
#            xlim = c(.5, n_scenarios + 0.5), axes = FALSE, ann = FALSE)
#       with(.x, segments(x0 = 1:n_scenarios, y0 = get(paste0(pars_to_plot[ii], '_v.low')), x1 = 1:n_scenarios, 
#                         y1 = get(paste0(pars_to_plot[ii], '_v.high')), col = 'gray70'))
#       with(.x, segments(x0 = 1:n_scenarios, y0 = get(paste0(pars_to_plot[ii], '_low')), x1 = 1:n_scenarios, 
#                         y1 = get(paste0(pars_to_plot[ii], '_high')), lwd = 3,
#                         col = pal(n_scenarios)))
#       with(.x, points(x = 1:n_scenarios, y = get(paste0(pars_to_plot[ii], '_mid')), pch = 16))
#       box(col = 'grey70')
#       if(ii == 1) mtext(str_to_title(.y$fleet), 3, adj = 0, cex = 0.75)
#       
#       if(.y$fleet == 'crab') {
#         axis(2, las = 2, col = 'gray70')
#         mtext(yaxis_labs[ii], side = 2, line = 3.5)
#         if(ii == length(pars_to_plot)) {
#           axis(1, col = 'gray70', at = 1:n_scenarios, labels = FALSE)
#           axis(1, at = 1:n_scenarios, labels = .x[[experiment]], tick = FALSE, las = 2)
#         }
#       }
#     })
# }
# dev.off()
}

# make_spp_plots <- function(filename, df, pars_to_plot, axis_labs, experiment) {
#   if(length(pars_to_plot) != length(yaxis_labs)) {
#     print('Error: number of parameters to plot does not match number of axis labels!')
#     break
#   }
#   
#   n_scenarios <- nlevels(factor(df[[experiment]]))
#   pal <- colorRampPalette(wesanderson::wes_palette('Zissou1', n = 5)[c(5,3,1)])
#   
#   plt.ls <- map2(pars_to_plot, xaxis_labs, function(par_to_plot, xaxis_lab) {
#     to.plot <- df %>%
#       ggplot() +
#       geom_density(aes(x = get(par_to_plot), col = get(experiment), fill = get(experiment)),
#                    alpha = .25) +
#       facet_wrap(~fleet, nrow = 1, scales = 'free_x') +
#       ggsidekick::theme_sleek(base_size = 12) +
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
#       labs(x = xaxis_lab, col = str_to_title(experiment), fill = str_to_title(experiment)) +
#       scale_color_manual(values = pal(n_scenarios)) +
#       scale_fill_manual(values = pal(n_scenarios)) +
#       theme(axis.text = element_text(size = 5),
#             axis.title.y = element_blank()) +
#       NULL
#     if(par_to_plot != pars_to_plot[1]) {
#       to.plot <- to.plot +
#         theme(strip.text.x = element_blank())
#     }
#     if(par_to_plot == pars_to_plot[length(pars_to_plot)]) {
#       to.plot <- to.plot +
#         theme(legend.position = 'bottom')
#     } else {
#       to.plot <- to.plot +
#         theme(legend.position = 'none')
#     }
#     return(to.plot)
#   })
#   
#   # png(paste0('Figures/pub_figs/', filename, '.png'), res = 500, units = 'in', width = 7,
#   #     height = 2*length(pars_to_plot))
#   ggsave(filename = paste0('Figures/pub_figs/', filename, '.png'), 
#          plot = grid.arrange(grobs = plt.ls, nrow = 2, 
#                              left = textGrob("Density", rot = 90, gp=gpar(col = 'gray30'))), 
#          height = 2*length(pars_to_plot), width = 7, units = 'in', dpi = 500) 
#   # dev.off()
#   
#   spp.summary <- df %>% 
#     group_by_at(c(experiment, 'spp')) %>%
#     summarize_at(pars_to_plot, list(v.low = ~quantile(., 0.025), low = ~quantile(., 0.25),
#                                                    mid = median, high = ~quantile(., .75), 
#                                                    v.high = ~quantile(., .975))) %>%
#     ungroup() %>%
#     group_by(spp) 
#   
#   png(paste0('Figures/pub_figs/', filename, '.png'), res = 500, units = 'in', width = 5, 
#       height = 2*length(pars_to_plot) + ifelse(make.legend, 1, 0))
#   mfrow.row <- ifelse(make.legend, length(pars_to_plot) + 1, pars_to_plot)
#   par(mfrow = c(mfrow.row, 3), mar = c(.5, 3, .5, .5), oma = c(5, 2.5, 4, 1), col = 'gray30',
#       col.axis = 'gray30')
#   if(make.legend) {
#     for(ii in 1:length(unique(df$spp))) {
#       plot(1,1, type = 'n', ann = FALSE, axes = FALSE)
#     }
#     legend('center', legend = unique(temp[[experiment]]), col = pal(n_scenarios), pch = 15, cex = .75, 
#            box.col = 'gray70')
#   }
#   
#   for(ii in 1:length(pars_to_plot)) {
#     group_map(spp.summary, .f = function(.x, .y) {
#       ymax <- ifelse(ii == 1, 1.05*max(.x[[paste0(pars_to_plot[1], '_v.high')]]),
#                      quantile(df[[pars_to_plot[2]]], 0.999))
#       plot(1:n_scenarios, 1:n_scenarios, type = 'n', axes = FALSE, ann = FALSE, 
#            xlim = c(0.5, n_scenarios + 0.5), ylim = c(0, ymax))
#       with(.x, segments(x0 = 1:n_scenarios, y0 = get(paste0(pars_to_plot[ii], '_v.low')), x1 = 1:n_scenarios, 
#                         y1 = get(paste0(pars_to_plot[ii], '_v.high')), col = 'gray70'))
#       with(.x, segments(x0 = 1:n_scenarios, y0 = get(paste0(pars_to_plot[ii], '_low')), x1 = 1:n_scenarios, 
#                         y1 = get(paste0(pars_to_plot[ii], '_high')), lwd = 3,
#                         col = pal(n_scenarios)))
#       with(.x, points(x = 1:n_scenarios, y = get(paste0(pars_to_plot[ii], '_mid')), pch = 16))
#       box(col = 'grey70')
#       if(ii == 1) {
#         mtext(str_to_title(.y$spp), 3, adj = 0, cex = 0.75)
#         axis(2, las = 2, col = 'gray70')
#       }
#       if(.y$spp == 'crab') {
#         mtext(yaxis_labs[ii], side = 2, line = 3.5)
#         if(ii == length(pars_to_plot)) {
#           axis(1, col = 'gray70', at = 1:n_scenarios, labels = FALSE)
#           axis(1,  at = 1:n_scenarios, labels = .x[[experiment]], tick = FALSE, las = 2)
#           axis(2, las = 2, col = 'gray70')
#         }
#       }
#     })
#   }
#   dev.off()
# }
