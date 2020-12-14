library(tidyverse)
library(readxl)
library(fitdistrplus)

## NOTE: bias estimates are from the NB fit data used for fig 3


k_empirical <- tibble('dataset' = NA,
                      'point_estimate' = NA,
                      'bias' = NA,
                      'upper' = NA,
                      'lower' = NA)

vo <- read_csv('results/empirical_data/vo/empirical_contact_data.csv') %>% 
  mutate(from = as.factor(from)) %>% 
  group_by(from) %>% 
  summarize(nu_hat = n())


fit <- fitdist(vo$nu_hat, method = 'mle', dist = 'nbinom')
CI <- bootdist(fit, bootmethod = 'nonparam')


k_empirical <- k_empirical %>%
  add_row(dataset = "Vo' \n (n = 31)",
          point_estimate = fit$estimate[[1]],
          bias = fit$estimate[[1]] + .47,
          upper = CI$CI[[5]],
          lower = CI$CI[[3]])

jakarta <- read_xlsx('results/empirical_data/jakarta/jakarta.xlsx', col_names = T) %>% 
  uncount( weights = `Frequency`)


fit <- fitdist(jakarta$`secondary cases`, method = 'mle', dist = 'nbinom')
CI <- bootdist(fit, bootmethod = 'nonparam')

k_empirical <- k_empirical %>%
  add_row(dataset = 'Jakarta \n (n = 1,199)',
          point_estimate = fit$estimate[[1]],
          bias = fit$estimate[[1]] + .1,
          upper = CI$CI[[5]],
          lower = CI$CI[[3]])

batam <- read_xlsx('results/empirical_data/jakarta/Batam.xlsx', col_names = T)%>% 
  uncount( weights = `frequency`)



fit <- fitdist(batam$`secondary cases`, method = 'mle', dist = 'nbinom')
CI <- bootdist(fit, bootmethod = 'nonparam')

k_empirical <- k_empirical %>%
  add_row(dataset = 'Batam \n (n = 89)',
          point_estimate = fit$estimate[[1]],
          bias = fit$estimate[[1]] + .4,
          upper = CI$CI[[5]],
          lower = CI$CI[[3]])



transmission_pairs <- read_csv('results/empirical_data/hong_kong/transmission_pairs.csv')
infectee <- transmission_pairs %>%
  dplyr::select(infector.case, infectee.case) %>%
  gather() %>%
  filter(key == 'infectee.case')

offspring <- transmission_pairs %>%
  dplyr::select(infector.case) %>%
  group_by(infector.case) %>%
  count() %>%
  arrange(desc(n))


infector <-  transmission_pairs %>%
  dplyr::select(infector.case, infectee.case) %>%
  gather() %>%
  filter(key == 'infector.case')

duplicate <- infector %>%
  left_join(., infectee, by = 'value') %>%
  filter(key.y != 'NA') %>%
  dplyr::select(value) %>%
  distinct()

nterminal_infectees <- infectee %>% 
  dplyr::select(value) %>%
  filter(!value %in% duplicate$value) %>%
  transmute(case.no = as.numeric(value)) %>%
  nrow() + 46 #46 Sporadic Local cases without links additional transmission

#create vector of complete offspring distribution with terminal cases having zero secondary cases
complete_offspringd <- enframe(c(offspring$n, rep(0,nterminal_infectees)))


#create vector of complete offspring distribution with terminal cases having zero secondary cases
complete_offspringd <- enframe(c(offspring$n, rep(0,nterminal_infectees)))

#fit negative binomial distribution to the final offspring distribution
fit <- complete_offspringd %>%
  pull(value) %>%
  fitdist(., distr = 'nbinom')

#bootstrap analysis
CI <- bootdist(fit, bootmethod = 'nonparam')


k_empirical <- k_empirical %>%
  add_row(dataset = 'Hong Kong \n (n = 290)',
          point_estimate = fit$estimate[[1]],
          bias = point_estimate + .3,
          upper = CI$CI[[5]],
          lower = CI$CI[[3]])


load('results/empirical_data/india/traceDatSaved.Rdata')

traceDat = traceDatSaved

dat <- traceDat %>% 
  as_tibble() %>% 
  dplyr::select(cPos, id, state) %>% 
  filter(!is.na(id), !is.na(cPos)) %>% 
  mutate(id = as.factor(id)) %>% 
  group_by(id) %>% 
  mutate(n_pos = sum(cPos)) %>% 
  dplyr::select(id, n_pos, state) %>% 
  unique()


fit = fitdist(dat %>% 
                filter(state == 'tn') %>% 
                pull(n_pos),
              distr='nbinom')
CI <- bootdist(fit, silent = F, parallel = 'multicore', ncpus = 4, bootmethod = 'nonparam')

k_empirical <- k_empirical %>%
  add_row(dataset = 'Tamil Nadu \n (n = 1,261)',
          point_estimate = fit$estimate[[1]],
          bias = fit$estimate[[1]] + .1,
          upper = CI$CI[[5]],
          lower = CI$CI[[3]])


fit = fitdist(dat %>% 
                filter(state == 'ap') %>% 
                pull(n_pos),
              distr='nbinom')
CI <- bootdist(fit, silent = F, parallel = 'multicore', ncpus = 4, bootmethod = 'nonparam')

k_empirical <- k_empirical %>%
  add_row(dataset = 'Andhra Pradesh \n (n = 83,614)',
          point_estimate = fit$estimate[[1]],
          bias = fit$estimate[[1]] + 0,
          upper = CI$CI[[5]],
          lower = CI$CI[[3]])




k_empirical <- k_empirical %>% 
  filter(!is.na(lower))

k_empirical[1, 4] = 10


k_empirical.long <- k_empirical %>% 
  pivot_longer(cols = c(point_estimate, bias), names_to = 'estimate_type', values_to = 'value') %>% 
  mutate(estimate_type = if_else(estimate_type == 'bias', 'Bias corrected', 'Raw estimate'))

write_csv(k_empirical.long, 'results/empirical_data/k_empirical.csv')

read_csv('k_empirical.csv')

ggplot(k_empirical.long, aes(x = as.factor(dataset)))+
  geom_point(aes(y = value, color = estimate_type), size = 10, alpha = 0.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, alpha =.5, size = 1.5)+
  scale_color_manual(values = c('red','black'))+
  theme_minimal(base_size = 23)+ 
  labs(x = '', y = 'Estimated k statistic ± 95% CI', color = 'Point estimate type')+
  scale_y_continuous(
    trans = "log10",
    n.breaks = 9)
ggsave('images/datasets.jpeg', scale = 2)
ggsave('~/Downloads/fig5.jpeg', scale = 2)






 