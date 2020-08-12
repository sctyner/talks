# recreating the xckd comic almost perfectly

xmas2 <- read_csv("slides/dat/xmas2.csv")
xmas2 %>% 
  group_by(decade) %>% 
  mutate(height = row_number()) -> xmas2 

library(forcats)

xmas2$decade <- factor(xmas2$decade, levels = paste0(seq(1920, 2010, by = 10), "s"), ordered = T)
xmas2$text_size <- stringr::str_length(xmas2$title)
xmas2$title2 <- purrr::map2_chr(xmas2$title, ceiling(sqrt(xmas2$text_size))*2, stringr::str_wrap)

test <- levels(xmas2$decade)

bbs <- gens %>% slice(2)


xmas2$text_size2 <- 1/log(xmas2$text_size)

readr::write_rds(xmas2, "slides/dat/xmasdata.rds")

xmas2 <- read_rds("slides/dat/xmasdata.rds")

ggplot(data = xmas2, aes(x = decade, y = height)) + 
  annotate("rect", xmin = 3, xmax = 5, ymin = 1, ymax = 11.1, fill = "grey70") + 
  annotate("text", x = 4, y = 10.5, label = "Baby Boom", family = "xkcd", size = 5, hjust = .3) + 
  geom_tile(aes(fill = as.factor(color_group)), color = "black", size = 1) + 
  scale_x_discrete(breaks = levels(xmas2$decade), labels = levels(xmas2$decade), drop = F) + 
  scale_fill_manual(values = c("#dc5555", "#74b37d")) + 
  geom_text(aes(label = title2, size =text_size2),family = "xkcd", lineheight = .7, 
            check_overlap = F) + 
  scale_size_continuous(range = c(2, 6)) + 
  theme(legend.position = "none", 
        text = element_text(family = "xkcd"), 
        panel.background = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_text(color = "black", size = 10),
        plot.tag.position = "bottom", plot.caption.position = "plot") + 
  labs(caption = "Source: ASCAP", y = "", x="",  title = "The 20 Most-Played Christmas Songs", 
       subtitle = "2019 Radio Airplay, by decade of copyright", 
       tag = "Every year, American culture embarks on a massive project to\n
       carefully recreate the Christmases of Baby Boomers' childhoods.")




ggplot(data = xmas2, aes(x = copy_year, fill = as.factor(age_group))) + 
  geom_histogram(binwidth = 10, center = 1935, color = "black") + 
  scale_fill_manual(values = c("darkred", "forestgreen")) + 
  scale_x_continuous(name = "Copyright year", breaks = seq(1930, 2010, by = 10)) + 
  scale_y_continuous(name = "Count", breaks = 1:12) + 
  theme_bw() + 
  ggtitle("ASCAP Top 25 Holiday Songs of 2019", subtitle = "Nielsen data from August 2, 2019 to November 21, 2019")

