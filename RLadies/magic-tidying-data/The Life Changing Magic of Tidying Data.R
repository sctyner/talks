library(tidyverse)
## read in data
bb <- read_csv("https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/billboard.csv")
# alt.
# bb <- read.csv("https://raw.githubusercontent.com/tidyverse/tidyr/master/vignettes/billboard.csv", stringsAsFactors = FALSE)
head(bb)

# separate song info out
songs <- bb %>% select(artist, track, time) %>% unique
head(songs)
# alt. 
# songs <- unique(bb[,c("artist","track","time")])

songs <- songs %>% mutate(songID = row_number())
head(songs)
# alt.
# songs$songID <- 1:nrow(songs)

bb <- bb %>% left_join(songs) %>% select(year, songID, artist:date.entered, wk1:wk76)
head(bb)

# separate object types
songs <- bb %>% select(songID, artist, track, time, date.entered)
head(songs)

ranks <- bb %>% select(songID, date.entered, wk1:wk76)
# alt.
# ranks <- bb %>% select(songID, date.entered, -(c(songID, date.entered)))
head(ranks)

# tidy-ranks
ranks2 <- ranks %>% gather(week, rank, wk1:wk76) 
head(ranks2)

glimpse(ranks2)

# turn integer to character
ranks2 <- ranks2 %>% 
  mutate(week = parse_integer(str_replace(week, "wk", "")), 
         rank = parse_integer(rank))

head(ranks2)
tail(ranks2)

# look at nas
ranks2 %>% filter(is.na(rank)) %>% glimpse

# songs on the charts longest
ranks2 %>% filter(week == 76, !is.na(rank)) %>% select(songID)

# hm. I guess that was just a boo-boo. what's the max number of weeks?
ranks2 %>% filter(!is.na(rank)) %>% select(week) %>% max()

# what song was that? 
ranks2 %>% filter(!is.na(rank), week == 65) %>% select(songID)
songs %>% filter(songID == 63)

ranks3 <- ranks2 %>% filter(!is.na(rank))
dim(ranks3)
summary(ranks3)


# line chart
ggplot(data = ranks3, aes(x = week, y = rank, group = songID)) + 
  geom_line(alpha = .5) + 
  scale_y_reverse()

# line chart by date
ranks3 %>% mutate(date.actual = date.entered + (week-1)*7) %>% 
  ggplot(aes(x = date.actual, y = rank, group = songID)) +
  geom_line(alpha=.5) + 
  scale_x_date() + 
  scale_y_reverse()

# songs that reached number one
ranks3 %>% mutate(date.actual = date.entered + (week-1)*7) %>% 
  group_by(songID) %>% mutate(highest_rank = min(rank)) %>% 
  filter(highest_rank == 1) %>%
  left_join(songs) %>% 
  mutate(full = paste(track, artist, sep = " by\n")) %>% 
  ggplot(aes(x = date.actual, y = rank, group = songID)) +
  geom_line(aes(color = full)) +
  theme(legend.position = 'bottom') + 
  scale_x_date() + 
  scale_y_reverse() 

# who had the most weeks at #1?
ranks3 %>% group_by(songID) %>%
  filter(rank == 1) %>% 
  summarise(weeksat1 = n()) %>%
  left_join(songs) %>% 
  select(artist, track, weeksat1) %>% arrange(desc(weeksat1))


# tidyverse info
tidyverse::tidyverse_packages()

# french fry data
fries <- reshape2::french_fries
head(fries)

fries %>% gather(potato:painty)


# tuberculosis data example
tb <- read_csv("http://stat405.had.co.nz/data/tb.csv") %>%
  select(iso2, year, new_sp_m014:new_sp_m65, new_sp_f014:new_sp_f65) %>% 
  filter(year >=2000)
head(tb)


## ----solution------------------------------------------------------------
tb2 <- gather(tb, var, val, new_sp_m014:new_sp_f65) 
head(tb2)
tb2 <- tb2 %>% mutate(var = str_replace(var, "new_sp_", ""))
head(tb2)
tb2 <- tb2 %>% mutate(var = str_replace(var, "0", "00"))
head(tb2)
tb3 <- tb2 %>% 
  separate(var, into = c("sex", "low", "upp"), sep = c(1,3)) %>% 
  mutate(age = paste(parse_integer(low), parse_integer(upp), sep = "-"),
         no_cases = val) %>% 
  select(iso2, year, sex, age, no_cases) %>% 
  arrange(year, age)
head(tb3)
head(data.frame(tb3))