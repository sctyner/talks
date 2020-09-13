library(tidyverse)


birth_data <- read_csv("~/../Downloads/tabula-arbuthnot.csv")

birth_data %>%
  separate(Christened., into = c("m", "f"), sep = " ") %>% 
  separate(Christened._1, into = c("m2", "f2"), sep = " ") -> birth_data

birth_data %>% 
  slice(2:20, 23:44) -> birth_data

birth_data1 <- birth_data %>% select(1:3) %>% set_names(c("year", "males", "females"))
birth_data2 <- birth_data %>% select(4:6) %>% set_names(c("year", "males", "females"))

birth_data <- bind_rows(birth_data1, birth_data2)

birth_data %>% 
  mutate(year = ifelse(str_length(year) == 2, paste0("16", year), year)) %>% 
  mutate_all(parse_number) %>% 
  pivot_longer(-year, names_to = "sex", values_to = "n_births") %>% 
  group_by(year) %>% 
  mutate(total_births = sum(n_births), 
         percent = n_births / total_births) -> birth_data_clean

male_births <- filter(birth_data_clean, sex == "males")

write_csv(male_births, "arbuthnot_data.csv")

birth_data_clean %>% group_by(sex) %>% summarize(mean = mean(percent))

ggplot(data = birth_data_clean) + 
  geom_line(aes(x = year, y = percent, color = sex))


sim_births <- crossing(year = birth_data_clean$year, sim = 1:1000)

# let's say males = 1 , females = 0 for simplicity 

sim_births %>% 
  left_join(birth_data_clean %>% select(year, total_births) %>% distinct()) %>% 
  mutate(simulated = map(total_births, rbinom, size = 1, prob = .5)) -> sim_births

# modern rate in UK is .512
# simulations for that 
crossing(year = birth_data_clean$year, sim = 1:1000) %>% 
  left_join(birth_data_clean %>% select(year, total_births) %>% distinct()) %>% 
  mutate(simulated = map(total_births, rbinom, size = 1, prob = .512)) -> sim_births2

sim_births %>% 
  mutate(n_males = map_int(simulated, sum)) -> sim_births

sim_births %>% 
  mutate(prop_male = n_males/total_births) -> sim_births

sim_births2 %>% 
  mutate(n_males = map_int(simulated, sum)) -> sim_births2

sim_births2 %>% 
  mutate(prop_male = n_males/total_births) -> sim_births2

sim_births %>%
  group_by(year) %>% 
  summarize(q95 = quantile(prop_male, .95), 
            q97.5 = quantile(prop_male, .975), 
            q99 = quantile(prop_male, .99)) -> sim_births_summ

sim_births2 %>%
  group_by(year) %>% 
  summarize(q95 = quantile(prop_male, .95), 
            q97.5 = quantile(prop_male, .975), 
            q99 = quantile(prop_male, .99)) -> sim_births2_summ

ggplot() + 
  geom_boxplot(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  #geom_violin(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  geom_line(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red") + 
  geom_point(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red") +
  geom_hline(yintercept = .5, linetype = "dashed", size = .5, color = "red", alpha = .75)

library(ggridges)
ggplot() + 
  geom_density_ridges(data = sim_births, aes(x = prop_male, y = year, group = year), rel_min_height = .01, scale = .9) + 
  geom_point(data = birth_data_clean %>% filter(sex == "males"), aes(y = year, x = percent), color = "red") + 
  coord_flip() + 
  scale_y_continuous(name = "YEAR", breaks = seq(1629, 1710, 1)) + 
  theme(panel.grid.minor.x = element_blank())

ggplot() + 
  geom_point(data = sim_births, aes(x = year, y = prop_male, group = year), size = .5, alpha = .1) + 
  #geom_boxplot(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  #geom_violin(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  geom_smooth(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red") + 
  geom_point(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red") +
  geom_hline(yintercept = .5, linetype = "dashed", size = .5, color = "red", alpha = .75) + 
  theme_bw()

ggplot() + 
  geom_point(data = sim_births, aes(x = year, y = prop_male, group = year), size = .5, alpha = .1) + 
  # add the 95th percentile
  #geom_line(data = sim_births_summ, aes(x = year, y = q95), color = "blue") + 
  #geom_line(data = sim_births_summ, aes(x = year, y = q97.5), color = "blue") + 
  geom_line(data = sim_births_summ, aes(x = year, y = q99), color = "blue",  alpha = .5) + 
  #geom_boxplot(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  #geom_violin(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  #geom_smooth(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red", se = FALSE, method = "lm") + 
  geom_point(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red") +
  geom_hline(yintercept = .5, linetype = "dashed", size = .5, color = "blue", alpha = .75) + 
  scale_x_continuous(name = "Year", breaks = seq(1629, 1710, by = 2)) + 
  # modern average: 1962-2017
  # geom_hline(yintercept = .512, linetype = "dashed", size = .5, color = "blue", alpha = .75) + 
  theme_classic() 


ggplot() + 
  geom_point(data = sim_births2, aes(x = year, y = prop_male, group = year), size = .5, alpha = .1) + 
  geom_line(data = sim_births2_summ, aes(x = year, y = q99), color = "blue",  alpha = .5) + 
  #geom_boxplot(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  #geom_violin(data = sim_births, aes(x = year, y = prop_male, group = year)) + 
  #geom_smooth(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red", se = FALSE, method = "lm") + 
  geom_point(data = birth_data_clean %>% filter(sex == "males"), aes(x = year, y = percent), color = "red") +
  #geom_hline(yintercept = .5, linetype = "dashed", size = .5, color = "blue", alpha = .75) + 
  # modern average: 1962-2017
  geom_hline(yintercept = .512, linetype = "dashed", size = .5, color = "blue", alpha = .75) + 
  theme_bw()


# arbuthnot

pr_even_births <- function(N){
  if ( (N %% 2) == 1){
    stop("N must be even")
  }
  num <- choose(N, N/2)
  denom <- 2^N
  num/denom
}

ns <- (1:500)*2

prs <- lapply(ns, pr_even_births)
prs <- unlist(prs)

qplot(x = ns, y = prs, geom = "line") 


# modern numbers 
# https://ourworldindata.org/gender-ratio#how-does-the-sex-ratio-at-birth-vary-across-the-world

# paper about the human sex ratio during pregnancy
# https://pdfs.semanticscholar.org/3986/f62b66ee8450859909e122fb2974c1c9fe6b.pdf

modern <- read_csv("~/../Downloads/sex-ratio-at-birth.csv")

names(modern)[4] <- "n_males_per_100_f"


modern %>% filter(Entity == "United Kingdom") %>% pull(3)
  mutate(perc_male = n_males_per_100_f / (n_males_per_100_f + 100)) %>% 
  ggplot() + 
  geom_point(aes(x = Year, y = perc_male))
  
  # hypothesis test 
  
p <- ggplot(birth_data_clean %>% filter(sex == "males"), aes(sample = percent))
p + stat_qq() + stat_qq_line()

ggplot(birth_data_clean %>% filter(sex == "males"), aes(x = percent)) + 
  # geom_histogram(binwidth = .002)
  geom_density()

birth_data_clean %>% group_by(sex) %>% summarize(mean = mean(percent), sd = sd(percent)) %>% pull(sd)

# proportion test for all years, use bonferroni corrections 

b_alpha <- .05/82

birth_data_clean %>% 
  select(year:total_births) %>% 
  pivot_wider(id_cols = c(year, total_births), names_from = sex, values_from = n_births) %>% # n = 82 
  mutate(prop.test = map2(males, total_births, prop.test, alternative = "greater")) %>% 
  mutate(pval = map_dbl(prop.test, ~.$p.value), 
         reject_hyp = pval < .05, 
         reject_hyp_bon = pval < b_alpha) -> freq_hyp_tests


test1 <- prop.test(x = 5128, n = 9901, alternative = "greater") 

birth_data_clean %>% 
  filter(sex == "males") %>% 
  select(year, n_males = n_births, total_births) -> for_hyp_tests

all_test <- prop.test(x = for_hyp_tests$n_males, n = for_hyp_tests$total_births, p = rep(.5, 82), 
                      alternative = "greater")
all_test

x <- seq(.49, .55, length.out = 1000)

# bayesian
install.packages("gghighlight")
library(gghighlight)

birth_data_clean %>% select(year, sex, n_births) %>% 
  pivot_wider(names_from = sex, values_from = n_births) %>% 
  mutate(alpha = males +1 , beta = females + 1) %>% 
  mutate(sims = map2(alpha, beta, rbeta, n = 1000)) %>% 
  unnest(c(sims)) %>% 
  ggplot() + 
  geom_density(aes(x = sims, group = year)) +
  #ggh
  geom_vline(xintercept = c(.5, sampling_distribution$mean_samp_prop))


birth_data_clean %>% select(year, sex, n_births) %>% 
    pivot_wider(names_from = sex, values_from = n_births) %>% 
    mutate(alpha = males +1 , beta = females + 1) %>% 
    mutate(dens = map2(alpha, beta, dbeta, x = x)) %>% 
    unnest(c(dens)) %>% 
  mutate(x = x) -> bayes_data
ggplot() + 
  geom_ribbon(data = bayes_data %>% filter(x <=.5), aes(ymin = 0, ymax = dens, x = x, group = year), fill = "red", alpha = .2) + 
  geom_line(data = bayes_data, aes(x = x, y = dens), alpha = .2) + 
  geom_line(data = bayes_data, aes(x = x, y = dens, group = year), alpha = .2) + 
   # geom_area(data = bayes_data %>% filter(x <=.5) ,inherit.aes = F,  aes(x = x, y = dens, group = year, fill = year), alpha = .4) + 
  #geom_vline(xintercept = sampling_distribution$mean_samp_prop) + 
  theme_classic() + 
  theme(legend.position = "none")
  
bayes_data %>% 
  nest(data = c(x, dens)) %>% 
  mutate(pr.5 = map2_dbl(alpha, beta, pbeta, q = .5)) %>% 
  mutate(likrat = (1-pr.5)/pr.5) %>% 
  ggplot() + 
  geom_bar(aes(x = pr.5))+ 
  scale_x_binned()
  geom_vline(xintercept = c(.05, .05/82))

bayes_data %>% 
  nest(data = c(x, dens)) %>% 
  mutate(pr.5 = map2_dbl(alpha, beta, pbeta, q = .5), 
         pr.gr.5 = 1-pr.5) %>% 
  ungroup() %>% 
  summarize(total_area_less.5 = sum(pr.5), total_area_gr.5 = sum(pr.gr.5)) %>% 
  mutate(rat = total_area_gr.5 / total_area_less.5)




#---------- scratch ----------------------------

(.5167832 - .5) / (.00716/(sqrt(82)))
# 21.22603

s_hat <- .00716
p_hat <- .5167832


birth_data_clean %>% 
  filter(sex == "males") %>% 
  select(year, total_births, percent) -> for_hyp_tests

for_hyp_tests %>% 
  mutate(var = (.5^2)/total_births) %>% 
  ungroup() %>% 
  summarize(mean_samp_prop = mean(percent), var_samp_dist = sum(var)/(82^2)) -> sampling_distribution

x <- seq(.49, .52, length.out = 1000)
density_h0 <- dnorm(x, mean = .5, sd = sqrt(sampling_distribution$var_samp_dist))

ggplot(data = NULL) + 
  geom_line(aes(x = x, y= density_h0)) + 
  geom_vline(xintercept = sampling_distribution$mean_samp_prop)


all_sims <- tibble(h0_.5 = rnorm(1000, mean = .5, sd = sqrt(sampling_distribution$var_samp_dist)), 
                   h0_.5025 = rnorm(1000, mean = .5025, sd = sqrt(sampling_distribution$var_samp_dist)),
                   h0_.505 = rnorm(1000, mean = .505, sd = sqrt(sampling_distribution$var_samp_dist)),
                   h0_.5075 = rnorm(1000, mean = .5075, sd = sqrt(sampling_distribution$var_samp_dist)),
                   h0_.51 = rnorm(1000, mean = .51, sd = sqrt(sampling_distribution$var_samp_dist)), 
                   h0_.5125 = rnorm(1000, mean = .5125, sd = sqrt(sampling_distribution$var_samp_dist)),
                   h0_.515 = rnorm(1000, mean = .515, sd = sqrt(sampling_distribution$var_samp_dist)),
                   h0_.5175 = rnorm(1000, mean = .5175, sd = sqrt(sampling_distribution$var_samp_dist)),
                   h0_.52 =  rnorm(1000, mean = .52, sd = sqrt(sampling_distribution$var_samp_dist)))


all_sims %>% 
  pivot_longer(1:9, names_to = "hypothesis", values_to = "x") %>%
  separate(hypothesis, into = c("type", "pop_mean"), sep = "_") %>% 
  mutate(pop_mean = parse_number(pop_mean)) -> all_sims_long

ggplot(data =all_sims_long) + 
  geom_density_ridges(aes(x = x, y = pop_mean, group = pop_mean), rel_min_height =.001) + 
  geom_vline(xintercept = sampling_distribution$mean_samp_prop) + 
  labs(x = "Possible values for the mean proportion of males born in the years 1629-1710", 
       y = "Distribution of values under different hypothesized population means.") + 
  scale_y_continuous(breaks = seq(.5, .52, by = .0025))


hyps_sims <- tibble(mean = means, s) %>% 
  mutate(sims = map(mean, ~rnorm(500, mean = ., sd = s_hat)))
hyps_sims %>% unnest(c(sims)) %>% 
  ggplot() + 
  geom_density(aes(x = sims, color = as.factor(mean))) + 
  geom_vline(xintercept = p_hat) + 
  geom_density(data = )


alpha_draws %>% 
  bind_rows(beta_draws) %>% 
  pivot_wider(names_from = .variable, values_from = .value) %>% 
  ggplot(aes(x = alpha, y = beta)) + 
  geom_density_2d_filled(color = NA) + 
  coord_cartesian(xlim = c(3000, 10000), ylim=c(3000, 10000)) +
  theme_xaringan() + 
  theme(legend.position = "none") 


