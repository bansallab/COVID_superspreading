library(tidyverse)

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


N_names <- c('25' = 'N = 25',
             '50' = 'N = 50',
             '75' = 'N = 75',
             '100' = 'N = 100',
             '200' = 'N = 200',
             'k' = 'Dispersion k coverage',
             'mu' = 'Mean (R0) coverage')  

p2 <- ggplot(d %>% 
               pivot_longer(cols = contains('coverage'),
                            names_to = c('CI_type', 'param'),
                            names_sep = "_coverage_",
                            values_to = 'coverage') %>% 
               mutate(CI_type = if_else(CI_type == 'boot', 'Bootstrap',
                                        if_else(CI_type == 'profile', 'Profile',
                                                'Wald'))) %>% 
               filter(param == 'k', N != 75, CI_type != 'Profile', N == 50 | N == 200),
             aes(x = k_true,
                 y = coverage,
                 color = CI_type))+
  geom_point(alpha = .5, size = 5)+
  facet_wrap(~N, labeller = as_labeller(N_names))+
  #ylim(.65, 1)+
  theme_minimal(base_size = 20)+
  scale_color_brewer(palette = 'Dark2')+
  #geom_smooth(se = F, span = 3)+
  geom_hline(aes(yintercept = .95), linetype = 2, show.legend = T)+
  labs(x = 'True k value', y = 'CI coverage for k', color = 'CI type', fill = '', tag = 'B')+
  theme(legend.position = c(0.85, 0.2),
        legend.background = element_rect(fill = "white"))+
  guides(shape = FALSE,
         colour = guide_legend(override.aes = list(linetype = c("blank", "blank"))))

pal = brewer.pal(n = 9, name = "Greens")[c(9, 7, 5)]


p1 <- means %>% 
  mutate(k.accurate.10 = abs(k - k_obs) < .1 * k,
         k.accurate.15 = abs(k - k_obs) < .15 * k,
         k.accurate.25 = abs(k - k_obs) < .25 * k,
         k.accurate.50 = abs(k - k_obs) < .5 * k) %>% 
  filter(N != 75) %>% 
  group_by(N) %>% 
  summarize(accurate.10 = mean(k.accurate.10),
            accurate.15 = mean(k.accurate.15),
            accurate.25 = mean(k.accurate.25),
            accurate.50 = mean(k.accurate.50)) %>% 
  pivot_longer(cols = accurate.10:accurate.50, names_to = 'names', values_to = 'values', names_prefix = 'accurate.') %>%
  filter(names == 50 | names == 25 | names == 15) %>% 
  ggplot(aes(x = as.factor(N), y = values, color = names))+
  geom_point(size = 5)+
  labs(x = 'Number of primary infections', 
       y = 'Proportion of sample means within x% of true mean',
       color = '% distance from \ntrue mean',
       tag = 'A')+
  scale_color_manual(values = pal)+
  theme_minimal(base_size = 20)+
  theme(legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = "white"))


ggsave('images/fig3.jpeg', p1 | p2, scale = 2)
