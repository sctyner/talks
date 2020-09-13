# fit bayesian hierarchical model 

library(runjags)
library(tidyverse)

male_births <- read_csv("dat/arbuthnot_data.csv")

# data objects for JAGS

n <- male_births$total_births
y <- male_births$n_births
N <- length(y)

# hyperparam start values 
mu_a <- 1
mu_b <- 1
log_n <- log(mean(n))

my_dat <- list(y = y, n = n,  N = N, # data 
               mua = mu_a, mub = mu_b, logn = log_n)

# JAGS model 
# https://bayesball.github.io/BOOK/bayesian-hierarchical-modeling.html#a-hierarchical-beta-binomial-model

model_string <- "
model {
## likelihood
for (i in 1:N){
  y[i] ~ dbin(p[i], n[i])
}

## priors
for (i in 1:N){
   p[i] ~ dbeta(alpha, beta)
}
## hyperpriors
alpha <- mu*eta
beta <- (1-mu)*eta
mu ~ dbeta(mua, mub)
eta <- exp(logeta)
logeta ~ dlogis(logn, 1)
}
"
# generate samples from posterior

post_samps <- run.jags(model = model_string, n.chains = 3, 
                       data = my_dat, monitor = c("p", "alpha", "beta"),
                       adapt = 1000, burnin = 5000, sample = 10000, silent.jags = TRUE)

library(tidybayes)

post_samps %>% 
  gather_draws(alpha) -> alpha_draws
post_samps %>% 
  gather_draws(beta) -> beta_draws
post_samps %>% 
  gather_draws(p[]) -> p_draws

p_draws %>% arrange( .chain,  .iteration, .draw) %>% 
  group_by(.chain, .iteration, .draw) %>% 
  mutate(p_year = 1:82) -> p_draws


value_summ <- add.summary(post_samps, silent.jags = TRUE)

value_summ_df <- as_tibble(value_summ$summaries)
value_summ_df$param <- row.names(value_summ$summaries)

value_summ_df %>% 
  select(param, everything()) %>% 
  filter(str_detect(param, "a")) -> params_ab


# 

# library(ggfortify)
# ggdistribution(dbeta, seq(0.49, .54, 0.0001), shape1 = 	5808.95, shape2 = 5442.88)

# alpha_draws %>% 
#   bind_rows(beta_draws) %>% 
#   ggplot(aes(x = .value, fill = .variable)) + 
#   geom_density(alpha = .5) 
