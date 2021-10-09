# check for tidyverse
if (!("tidyverse" %in% .packages())){
  library(tidyverse)
}

data("mtcars")

mtcars %>% 
  rownames_to_column("car") %>% 
  as_tibble() %>% 
  mutate(
    # convert cyl (# cylinders) to ordered factor
    cyl = fct_inseq(factor(cyl), ordered = TRUE),
    # repeat with gear, carb 
    gear = fct_inseq(factor(gear), ordered = TRUE), 
    carb = fct_inseq(factor(carb), ordered = TRUE), 
    # recode vs to match the values in the data dict.
    vs = fct_recode(factor(vs), "v-shaped" = "0", "straight" = "1"),
    # do the same to the am 
    am = fct_recode(factor(am), "automatic" = "0", "manual" = "1")
  ) -> mtcars
