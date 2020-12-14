library(tidyverse)

d <- read_csv('results/df_sim_univariate.csv')

df.results <- read_csv('results/df_sim.csv')

p1 <- ggplot(d, aes(x = mean.var.ratio , y = k, color = type))+
  geom_point(size = 5)+ 
  theme_minimal()+
  ylim(0, 10)+
  xlim(0, 100)+
  labs(x = 'Heterogeneity in transmission factor', 
       y = 'Homogeneity in transmission (k)',
       color = 'Transmission \nfactor',
       tag = 'A')+
  theme_minimal(base_size = 25)+
  #geom_hline(aes(yintercept))+
  scale_color_brewer(palette = 'Accent')+
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "white",
                                         color = NULL))


p2 <- ggplot(df.results %>% 
               filter(t.var.ratio == 1 | t.var.ratio == 50.5 | t.var.ratio == 100, graph.var.ratio < 100, !is.na(mu.py)) %>% 
               mutate(R0.thresh = mu.py < 5),
             aes(x = graph.var.ratio,
                 y = k.py, 
                 color = as.factor(t.var.ratio), 
                 group = as.factor(t.var.ratio)))+
  geom_point(size = 5)+
  geom_hline(aes(yintercept = 0.370), linetype = 2, show.legend = T)+
  #geom_smooth(se = F, span = 1.5)
  ylim(0, 10)+
  theme_minimal(base_size = 25)+
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "white"))+
  scale_color_manual(values = blue_pal, labels = c('1', '50', '100'))+
  labs(x = 'Behavior heterogeneity', 
       y = 'Homogeneity in transmission (k)', 
       color = 'Transmissibility \n heterogeneity',
       tag = 'B',
       fill = '')+
  guides(shape = FALSE,
         colour = guide_legend(override.aes = list(linetype = c("blank", "blank", "blank"))))



ggsave('images/fig1.jpeg', p1 | p2, scale = 2)