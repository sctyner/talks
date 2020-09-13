# simulation 
library(tidyverse)
# data 
male_births <- read_csv("dat/arbuthnot_data.csv")

# simulations 
B <- 1000

sim_births <- male_births %>% 
  select(year, total_births, percent) %>% 
  mutate(simulated = map(total_births, rbinom, n = B, prob = 0.5))
  
sim_births %>% 
  mutate(sim_props = map2(simulated, total_births, ~ .x / .y), 
         p_stars = map2_dbl(sim_props, percent, ~sum(.x >= .y )/B)) -> sim_births

p_star <- mean(sim_births$p_stars)

# continuity correction because so many are 0.
p_hat <- prod(ifelse(sim_births$p_stars == 0, 0.0005, sim_births$p_stars))
