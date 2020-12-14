library(tidyverse)

df.results <- read_csv('results/df_sim.csv')

ggplot(df.results %>% 
         filter(k.py < 4),
       aes(x = k.py, y = p.20))+
  geom_point(alpha=.75, color = 'black', size = 5)+
  ylim(.2, 1)+
  geom_hline(aes(yintercept = .8), linetype = 2)+
  scale_x_log10()+
  labs(x = 'Homogeneity in transmission (k)', 
       y = 'Proportion of transmission due to \n most contagious 20% of cases',
       fill = '')+
  theme_minimal(base_size = 30)
ggsave('images/fig2.jpeg', scale = 2.25)
