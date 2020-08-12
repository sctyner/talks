# what is the difference between 
# guide_legend,
# guide_colourbar,
# guide_bins,
# guide_colorsteps 

df <- expand.grid(X1 = 1:10, X2 = 1:10)
df$value <- df$X1 * df$X2

p1 <- ggplot(df, aes(X1, X2)) + geom_tile(aes(fill = value))
p2 <- p1 + geom_point(aes(size = value) )


plegend <- p1 + scale_fill_continuous(guide = guide_legend()) + theme(legend.position = "top")
pcolorbar <- p1 + scale_fill_continuous(guide = guide_colourbar()) + theme(legend.position = "top") # (default)
pbins <- p1 + scale_fill_continuous(guide = guide_bins()) + theme(legend.position = "top") # why nas?
pcolorsteps <- p1 + scale_fill_continuous(guide = guide_colorsteps()) + theme(legend.position = "top")  # why nas? 

gridExtra::grid.arrange(plegend, pcolorbar, pbins, pcolorsteps)

# ok. the actual color on the plot never changes. only the legend changes. 
# for some reason, the last two have NAs appear in the legend even though there are none in the data 
# -> changed x1, x2 to include zero and it works 

# what is diff bw bins & colorsteps? 

# install.packages("cowplot")
library(cowplot)

lcolorsteps <- get_legend(pcolorsteps)
llegend <- get_legend(plegend)
lbins <- get_legend(pbins)
lcolorbar <- get_legend(pcolorbar)

plot_grid(lcolorbar, llegend, lbins, lcolorsteps, nrow = 2, 
          labels = c("colorbar", "legend", "bins", "colorsteps"))
