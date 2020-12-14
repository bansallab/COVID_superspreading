library(tidyverse)
library(patchwork)

#########

df <- read_csv('results/contact_trace/2_obs.csv', col_names = c('k', 'N', 'p_obs', 'k.est')) %>% 
  mutate(. = na_if(., -1))

ggplot(df)+
  geom_boxplot(aes(as.factor(N), k.est), outlier.shape = NA)+
  geom_hline(aes(yintercept = k))+
  facet_grid(p_obs~k)+
  ylim(0, 1.5)+
  labs(x = 'Sample size', y = 'Estimated k value | superspreading observed twice')+
  theme_minimal()
ggsave('images/figS6.jpeg')

########

df <- read_csv('results/contact_trace/large_obs.csv', col_names = c('k', 'N', 'p_obs', 'k.est')) %>% 
  mutate(. = na_if(., -1))
ggplot(df)+
  geom_boxplot(aes(as.factor(N), k.est), outlier.shape = NA)+
  geom_hline(aes(yintercept = k))+
  facet_grid(p_obs~k)+
  ylim(0, 1.5)+
  labs(x = 'Sample size', y = 'Estimated k value | superspreading event observed')+
  theme_minimal()
ggsave('images/figS5.jpeg')


df %>%
  group_by(N, p_obs) %>% 
  summarize(bias = round(mean(k - k.est), 2)) %>% 
  rename('Bias' = 'bias', 'Secondary case detection probability' = 'p_obs') %>% 
  kable() %>% 
  kable_styling() %>% 
  save_kable('images/tableS1.jpeg')


means <- read_csv('results/NB_fit.csv')


ggplot(means)+
  geom_boxplot(aes(x = as.factor(N), k_obs), outlier.shape = NA)+
  #geom_jitter(aes(x = as.factor(N), y = k_obs), alpha = .015, height = 0, width = .15)+
  ylim(0, 2)+
  #scale_color_distiller(palette = 'Reds', direction = 1)+
  geom_hline(aes(yintercept = k, fill = 'True population k'), size = .8, color = 'black', show.legend = T)+
  facet_wrap(~k_true_name)+
  labs(x = 'Sample size', y = 'MLE fit of k', color = 'Population NB mean', fill = '')+
  theme_minimal()
ggsave('images/figS4.jpeg')

df.results <- read_csv('results/df_sim.csv')

ggplot(df.results %>% 
         filter(t.var.ratio == 1 | t.var.ratio == 50.5 | t.var.ratio == 100, graph.var < 801),
       aes(x = graph.var.ratio, y = mu.py, color = as.factor(t.var.ratio)))+
  geom_point(size = 5)+
  theme_minimal(base_size = 25)+
  theme(legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = "white"))+
  scale_color_manual(values = blue_pal, labels = c('1', '50', '100'))+
  labs(x = 'Behavior heterogeneity', 
       y = 'R0 (mean number of transmissions)', 
       color = 'Transmissibility \n heterogeneity')+
  guides(shape = FALSE,
         colour = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank"))))
ggsave('images/figS7.jpeg', scale = 2.25)


df.results <- read_csv('results/processed_results.csv')

ggplot(df.results %>% filter(mu.py < 5, corr.var == 0), aes(x = mean.var.ratio, y = k.py, color = b.var))+
  geom_point()+
  theme_minimal()+
  #facet_wrap(~corr.var)+
  scale_color_distiller(palette = 'Reds', direction = 1)+
  #geom_hline(aes(yintercept = coef(fit.1)[1]), color = 'grey', linetype = 2, show.legend = T)+
  labs(x = 'Degree var/mean (tail heaviness)',
       y = 'Homogeneity in transmission (k)',
       fill = '',
       color = 'Beta distribution \n b parameter')+
  #theme(legend.position = 'None')+
  ylim(0, 3)

ggsave(filename = 'images/figS3.jpeg')



df.large <- read_csv('results/graph_results_large_mean.csv', col_names = c('mu', 'k')) %>% 
  mutate(graph.var = graph_var_list,
         mean.var.ratio = graph_var_list / 16)

p1 <- ggplot(df.large, aes(mean.var.ratio, k))+
  geom_point(size = 1.5)+
  #geom_smooth(se = F, span = .3)+
  geom_hline(aes(yintercept = 1), linetype = 2, color = 'black')+
  ylim(0, 5)+  
  labs(y = 'Homogeneity in transmission (k)', x = 'Degree var / mean', tag = 'A')+
  theme_minimal(base_size = 15)



df.small <- read_csv('results/graph_results_small_mean.csv', col_names = c('mu', 'k')) %>% 
  mutate(graph.var = seq(4, 400, 4),
         mean.var.ratio = seq(4, 400, 4) / 4)

p2 <- ggplot(df.small, aes(mean.var.ratio, k))+
  geom_point(size=1.5)+
  #geom_smooth(se = F, span = .3)+
  geom_hline(aes(yintercept = 1), linetype = 2, color = 'black')+
  labs(y = 'Homogeneity in transmission (k)', x = 'Degree var / mean', tag = 'B')+
  ylim(0, 5)+
  theme_minimal(base_size = 15)

ggsave('images/figS1.jpeg', p1 | p2)




