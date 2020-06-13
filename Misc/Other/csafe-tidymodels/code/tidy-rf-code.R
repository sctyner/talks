# code for tidily fitting a rf to glass data

library(tidyverse)
library(tidymodels)

# get data 
glass_data <- read_rds("dat/glass_data.rds")
# head(glass_data)
# 
# # create the classes
# glass_data <- mutate(glass_data, match = as.numeric(pane_q == pane_k))
# head(glass_data)
# 
# #tidy names 
# names(glass_data) <- str_remove_all(names(glass_data), "[0-9]")
# 
# names(glass_data)
# 
# glass_data <- glass_data %>% select(-pane_q, -pane_k)
# 
# head(glass_data)
# write_rds(glass_data, "dat/glass_data.rds")


# for each element, the value is questioned mean log - known mean log. 
# ie first log each element rep val, then take the mean, then subtract: Qmean - Kmean
# get data 
glass_data <- read_rds("dat/glass_data.rds")

# first, split data into training/testing based on match 
set.seed(654321)
glass_data_split <- initial_split(data = glass_data, prop = 0.75, strata = "match")

glass_data_train <- training(glass_data_split)

glass_data_test <- testing(glass_data_split)

# model specification 
Mtry <- floor(sqrt(18)) # could also choose to tune according to this 
# type of model to fit
mod <- rand_forest(mode = "classification", mtry = Mtry)
mod

# what method to use to fit the model
spec_rf <- mod %>% 
  set_engine("randomForest")

spec_rf

# specify the formula we want to use
rf_formula <- match ~ . 

# what to do to the data before fitting the model 
rf_recipe <- recipe(rf_formula, data = glass_data_train) %>% 
  step_bin2factor(match) %>% # convert class to a factor for randomForest
  step_downsample(match) # downsample the data due to the large number of 0s in the data. 

rf_recipe

## set up cross-validation 
set.seed(2453)
cv_glass_data <- vfold_cv(
  data = glass_data_train,
  v = 5,
  strata = "match"
)
cv_glass_data 

object.size(cv_glass_data)

# do the downsampling ~~inside of resampling~~ 
cv_glass_data <-  cv_glass_data %>%
  mutate(recipes = map(splits, prepper, recipe = rf_recipe, retain = TRUE)) # retain = TRUE keeps processed version of the analysis set around so we don't have to recompute it. 

object.size(cv_glass_data)/1000000


## scratch work. not needed 
# just_juice <- juice(cv_glass_data$recipes[[1]])
# jucie_and_analysis <- bake(object = cv_glass_data$recipes[[1]], new_data=analysis(cv_glass_data$splits[[1]]))
# jucie_and_analysis %>% summary()

# fit rf to each cv split, making sure to apply the recipe to each cv split first! 
# (i.e. make sure we downsample before fitting the model)

fit_rf_to_splits <- function(split, rec = NULL, mspec, form) {
  if (!is.null(rec)) # if rec is specified (rec = recipe)
    mod_data <- juice(rec) # return the processed version of the training data 
  else
    mod_data <- analysis(split) # if no recipe, use raw cv analysis data 
  
  mod_fit <- fit(
    object = mspec, # which model are you fitting 
    formula = form, # what is its form?
    data = mod_data # what data are you fitting it to? 
  )
  return(mod_fit)
}

# fit rf to each CV split 
cv_glass_data_fits <- cv_glass_data %>% 
  mutate(model = map2(splits, recipes, fit_rf_to_splits, mspec = spec_rf, form = rf_formula))

# get predictions for each
compute_pred <- function(split, model, rec) {
  # Extract the assessment set
  assess <- bake(rec, new_data = assessment(split)) 
  # Compute predictions (a df is returned)
  predprob <- predict(model, new_data = assess, type = "prob") 
  predclas <- predict(model, new_data = assess) 
  bind_cols(assess, predprob, predclas)
}  

cv_glass_data_fits <- cv_glass_data_fits %>% 
  mutate(predictions = pmap(.l = list(splits, model, recipes), .f = compute_pred))

object.size(cv_glass_data_fits)/1e6


# assessing the fits 

cv_glass_data_fits <- cv_glass_data_fits %>% 
  mutate(roc_auc = map_dbl(predictions, ~roc_auc(.x, match, .pred_yes)$.estimate), 
         conf_mat = map(predictions, ~conf_mat(.x, match, .pred_class)), 
         specificity = map_dbl(predictions, ~spec(.x, match, .pred_class)$.estimate),
         sensitivity = map_dbl(predictions, ~sens(.x, match, .pred_class)$.estimate),  
         roc_data = map(predictions, ~roc_curve(.x, match, .pred_yes)))

# get optimum threshold for each ROC 
get_opt_th_rf <- function(rocdat){
  rocdat2 <- rocdat %>% mutate(s_plus_s = specificity + sensitivity) %>% filter(s_plus_s == max(s_plus_s)) %>% select(-s_plus_s)
  names(rocdat2) <- c("opt.th", "opt.spec", "opt.sens")
  return(rocdat2)
}

cv_glass_data_fits <- cv_glass_data_fits %>% 
  mutate(opt_thresh = map(roc_data, get_opt_th_rf)) 

cv_glass_data_fits %>% unnest(opt_thresh) 


chosen_one <- cv_glass_data_fits[3,]

chosen_one$roc_data[[1]] %>% autoplot()

chosen_one$roc_data[[1]] %>% mutate(diff = abs(diff(specificity - sensitivity)))
test <- chosen_one$roc_data[[1]] 
abs(test$specificity - test$sensitivity) %>% which.min()

chosen_one$roc_data[[1]][288,]

##########################################################################################################
############################### SCRATCH CODE ############################################################
##########################################################################################################

cv_glass_data_fits$roc_data[[1]] %>% mutate(s_plus_s = specificity + sensitivity) %>% filter(s_plus_s == max(s_plus_s))

# density of probability predictions by true 
cv_glass_data_fits$predictions[[1]] %>%  
  ggplot() + 
  geom_histogram(aes(x = .pred_yes, y = stat(density), color = match), fill = "grey80") + 
  geom_density(aes(x = .pred_yes, color= match)) + 
  scale_color_brewer(name = "Known Match", palette = "Dark2") + 
  scale_fill_brewer(name = "Known Match", palette = "Dark2")

# roc object 
roc_obj <- cv_glass_data_fits$predictions[[1]] %>%  
  roc_curve(match, .pred_yes) 

roc_obj %>% autoplot() 

cv_glass_data_fits$predictions[[1]] %>% roc_auc(match, .pred_yes)

?roc_auc

conf_mat(cv_glass_data_fits$predictions[[1]], match,.pred_class)

sens(cv_glass_data_fits$predictions[[1]], match, .pred_class)

spec(cv_glass_data_fits$predictions[[1]], match, .pred_class)

# yeppers. ok write a function to fit the model to each CV set

# fit_rf <- function(split, spec) {
#   fit(
#     object = spec,
#     formula = match ~ . ,
#     data = analysis(split) # <- pull out training set
#   )
# }

# fit rf to each cross-validation set 
# cv_splits_fits <- cv_splits %>% 
#   mutate(rf_model = map(splits, fit_rf, spec_rf))





# fit the model. but wait, do cross-validation with down-sampling first? 

fit_rf <- fit(
  spec_rf,
  match ~ .,
  data = rf_train_data
)

# confusion matrix with yardstick::conf_mat
# sensitivity & specificity with sens() and spec() from yardstick package. 

head(predict(fit_rf$fit, type = "prob"))

# have to decide on a class threshold after rf is fit. ROC curve
# draw ROC curve with yardstick::roc_curve 
# need columns call truth and estimate. 

