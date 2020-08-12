#monty hall problem simulation
# http://www.lilianccheung.com/data-blog/the-monty-hall-problem-with-r 
doors <- 1:3


monty_hall <- function(doors = 1:3){
  # pick a door for the prize
  prizes <- c("Car", "Goat1", "Goat2")
  badprizes <- c("Goat1", "Goat2")
  doorsetup <- sample(prizes,3)
  #"contestant" picks the door
  pick <- sample(1:3, 1)
  
  # randomly choose whether the contestant switches doors 
  # 1 is switch, 0 is stay
  switch <- rbinom(1, 1, .5)
  
  # if switch, randomly pick another door
  if (switch == 1){
    if(doorsetup[pick]=="Car") {
      reveal <- sample(badprizes,1)           # If the player's original choice was Car, the host will reveal either Goat1 or Goat2
      revealpos <- which(doorsetup==reveal)   # Location of door revealed by host
    } else if (doorsetup[pick]=="Goat1") {
      revealpos <- which(doorsetup=="Goat2")
    } else if (doorsetup[pick]=="Goat2") {
      revealpos <- which(doorsetup=="Goat1") }
    final <- doorsetup[-c(pick, revealpos)]
  } else{
    final <- doorsetup[pick]
  }
  
  # did the person win the prize? 
  win <- final == "Car"
  
  return(c(switch = switch, win = win))
  
}

monty_hall()
N <- 100

library(tidyverse)

sims <- list()

for (i in 1:N){
  sims[[i]] <- monty_hall()
}

sims[1]

sim_dat <- tibble(sim = 1:N, mh = sims)

sim_dat %>% unnest_wider(mh) %>% 
  ggplot() + 
  geom_bar(aes(x = switch, fill = as.factor(win)), position = "fill")


wide_sims <- sim_dat %>% unnest_wider(mh) 

wide_sims %>% count(switch, win) %>% 
  group_by(switch) %>% 
  mutate(perc = n / sum(n))

library(gganimate)

wide_sims %>% 
  mutate(Switch = ifelse(switch == 1, "Yes", "No"),
         Win = ifelse(win == 1, "Win", "Lose")) %>% 
  group_by(switch, win) %>% 
  mutate(count = row_number()) %>% 
  ungroup() %>% group_by(switch) %>% 
  mutate(n_switch = row_number(), 
         perc = count / max(n_switch)) %>% 
  ggplot() + 
  geom_bar(aes(x = Win, weight = perc, fill = Win, group = seq_along(sim))) + 
  #geom_bar(aes(x = Win, fill = Win, group = seq_along(sim) )) +
  facet_grid(cols = vars(Switch), labeller = label_both) + 
  transition_reveal(seq_along(sim)) 


wide_sims %>% 
  mutate(Switch = ifelse(switch == 1, "Yes", "No"),
         Win = ifelse(win == 1, "Win", "Lose")) %>% 
  group_by(switch, win) %>% 
  mutate(count = row_number()) %>% 
  ungroup() %>% group_by(switch) %>% 
  mutate(n_switch = row_number(), 
         perc = count / max(n_switch)) %>% 
  group_by(switch, win) %>%
  mutate(perc2 = lag(perc, default = 0), 
         perc3 = 0) %>% 
  ggplot() + 
  geom_rect(aes(xmin = win - .5, xmax = win + .5, ymin = perc3, ymax = perc, fill = Win, group = seq_along(sim)), color = "white") + 
  facet_grid(cols = vars(Switch), labeller = label_both) + 
  theme(legend.position = "none") + 
  # option 1
  # transition_reveal(seq_along(sim)) +
  # option 2 
  #transition_reveal(seq_along(sim)) + 
  #enter_fly(y_loc = .7)
  # option 3 
  transition_states(seq_along(sim), transition_length = 100, state_length = 1) + 
  shadow_mark()


extrafont::loadfonts(device = "postscript")
  
p_to_anim <- wide_sims %>% 
  mutate(Switch = ifelse(switch == 1, "Yes", "No"),
         Win = ifelse(win == 1, "Win", "Lose")) %>% 
  group_by(switch, win) %>% 
  mutate(count = row_number()) %>% 
  ungroup() %>% group_by(switch) %>% 
  mutate(n_switch = row_number(), 
         perc = count / max(n_switch)) %>% 
  group_by(switch, win) %>%
  mutate(perc2 = lag(perc, default = 0), 
         perc3 = 0) %>% 
  mutate(sim_label = glue::glue("Trial {sim}: Switch: {Switch}, Outcome: {Win}")) %>% 
  ggplot() + 
  geom_rect(aes(xmin = win - .5, xmax = win + .5, ymin = perc2, ymax = perc, fill = Win, 
                group = seq_along(sim)), 
            color = "grey40") + 
  facet_grid(cols = vars(Switch), labeller = label_both) + 
  scale_x_continuous(breaks = c(0,1), labels = c("Lose", "Win")) + 
  scale_y_continuous(breaks = seq(0,.6, .1), labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values =  c("#F6C40C", "#4AA0BB")) +
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(family = "Peralta", size = 20), 
        panel.grid = element_blank(), 
        strip.background = element_rect(fill = NA), 
        plot.title =  element_text(hjust = .5))  + 
  labs(title = "Let's Make a Deal!") + 
  transition_states(sim, transition_length = 10, state_length = 5) + 
  shadow_mark(color = NA) 
  
  


animate(p_to_anim, fps = 40, end_pause = 20, duration = 20) 
