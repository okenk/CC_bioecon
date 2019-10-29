library(reshape2)
library(beyonce)
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
            aes(x = wk, y = Catch, group = yr, col = factor(yr)), lwd = 1) + 
  facet_wrap(~spp, nrow = 1, scales = 'free_y') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white")) +
  xlab('Week of year') +
  ylab('Catch (weight)') +
  guides(col = guide_legend(title = 'Year')) +
  scale_color_manual(values = beyonce_palette(127, n = 6)[2:6]) +
  NULL
ggsave(filename = 'Figures/catch_ex.png', plot = to.save, height = 4, width = 9, units = 'in', dpi = 500)

effort.df <- apply(one.sim$effort, c(1,3,4), sum) %>%
  melt(value.name = 'effort') %>%
  as_tibble

to.save <- effort.df %>%
  ggplot() +
  geom_line(aes(x = wk, y = effort, group = yr), alpha = 0.25, lwd = 0.1) +
  geom_line(data = filter(effort.df, yr %in% yrs),
            aes(x = wk, y = effort, group = yr, col = factor(yr)), lwd = 1) + 
  facet_wrap(~spp, nrow = 1) +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white")) +
  xlab('Week of year') +
  ylab('Effort (number of vessels)') +
  guides(col = guide_legend(title = 'Year')) +
  scale_color_manual(values = beyonce_palette(127, n = 6)[2:6]) +
  NULL
ggsave(filename = 'Figures/effort_ex.png', plot = to.save, height = 4, width = 9, units = 'in', dpi = 500)


profit.df <- map_dfr(synchrony.access, function(corr.val)
  map_dfr(corr.val, function(sim.res) {
    melt(sim.res$profits, value.name = 'profit') %>%
      as_tibble()
    },
  .id = 'sim_number'),
  .id = 'sync_access')

revenue.df <- map_dfr(synchrony.access, function(corr.val)
  map_dfr(corr.val, function(sim.res) {
    melt(sim.res$revenue, value.name = 'revenue') %>%
      as_tibble()
  },
  .id = 'sim_number'),
  .id = 'sync_access')


correlation.df <- map_dfr(synchrony.access, function(corr.val) {
  map_dbl(corr.val, function(sim.res) 
    cor(sim.res$rec_devs['crab',], sim.res$rec_devs['salmon',])
  ) %>% 
    enframe(name = 'sim_number', value = 'correlation')
}, .id = 'sync_access') %>%
  mutate(sim_number = as.character(sim_number)) %>%
  separate(col = sync_access, into = c('access', 'synchrony'), sep = "(?<=access) ")
  

income <- left_join(profit.df, revenue.df)
income.summary <- group_by(income, sync_access, sim_number, fleet, ship) %>%
  summarize(profit.sd = sd(profit),
            profit.mn = mean(profit),
            revenue.sd = sd(revenue),
            revenue.mn = mean(revenue)) %>%
  mutate(revenue.cv = revenue.sd / revenue.mn) %>%
  # rename(!!sim.id := `get(sim.id)`) %>%
  filter(revenue.mn > 0) %>% # filters out ships with 0 revenue over a simulation, assume these did not have permits
# (problem is that revenue and profit should be ragged arrays, but have dimensions based on max # of ships
# in a fleet)
  separate(col = sync_access, into = c('access', 'synchrony'), sep = "(?<=access) ") %>%
  left_join(correlation.df)

                     
ggplot(income.summary) +
  geom_density(aes(x = revenue.sd, col = access, fill = synchrony),
               alpha = .25) +
  facet_wrap(~fleet, nrow = 2, scale = 'free_x') +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white")) +
  xlab('Revenue SD') +
  scale_color_manual(values = LaCroixColoR::lacroix_palette(type = 'paired', n = 4)[1:2]) +
  scale_fill_manual(values = LaCroixColoR::lacroix_palette(type = 'paired', n = 8)[c(3,7)]) +
  # guides(col = guide_legend(title = sim.id), fill = guide_legend(title = sim.id)) +
  NULL 

income.summary %>%
  filter(fleet == 'crab-salmon' | fleet == 'crab') %>%
  group_by(fleet, sim_number, synchrony, access) %>%
  summarize(revenue.cv = mean(revenue.cv), 
            correlation = first(correlation)) %>%
  ggplot(aes(y = revenue.cv, x = correlation, col = access)) +
  geom_point() +
#  ylim(0,2) +
  # facet_wrap(~access, nrow = 2) +
  stat_smooth(method = 'lm')
# Effect of synchrony on revenue CV is stronger with easy access!

xx <- rnorm(50)
yy <- rlnorm(50, sd = .6)

png('sr-shotgun.png', width = 5, height = 5, res = 200, units = 'in')
plot(xx, yy, pch = 16, axes = FALSE, ann = FALSE, cex = 1.5)
box(bty = 'l')
mtext('Spawning biomass', 1, 2, cex = 3)
mtext('Recruitment', 2, 2, cex = 3)
dev.off()

xx <- abs(rnorm(50))
yy <- xx/(1+xx) * rlnorm(50, sd = .3)

png('sr-bh.png', width = 5, height = 5, res = 200, units = 'in')
plot(xx, yy, pch = 16, axes = FALSE, ann = FALSE, cex = 1.5)
box(bty = 'l')
mtext('Spawning biomass', 1, 2, cex = 3)
mtext('Recruitment', 2, 2, cex = 3)
dev.off()
