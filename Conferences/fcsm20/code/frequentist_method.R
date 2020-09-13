# frequentist method: glm 

library(tidyverse)

male_births <- read_csv("dat/arbuthnot_data.csv")
successes <- male_births$n_births
failures <- male_births$total_births - male_births$n_births

data_mat <- cbind(successes, failures)

library(lme4)
# logit(p_i) = \mu + alpha_i
# alph_i ~ Normal(0, sigma^2)
mod <- glmer(data_mat ~ 1+ (1|male_births$year), family = "binomial")
# summary(mod)
# predict(mod, type = "response")
mu <- fixef(mod)
p_mean <-( exp(mu) / (1 + exp(mu)) )
# p_mean


preds <- predict(mod, type = "response")
preds2 <- predict(mod, type = "response")
qplot(preds, preds2) + geom_abline()

# plain ole glm
mod2 <- glm(data_mat ~ 1, family = "binomial")
mu2 <- coef(mod2)
p_mean2 <-( exp(mu2) / (1 + exp(mu2)) )
