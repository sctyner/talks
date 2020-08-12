#resize large images 
install.packages("magick")

library(magick)

illus <- image_read("slides/img/gganimate_fireworks.PNG")
illus <- image_resize(illus, geometry_size_pixels(width = 1000, preserve_aspect = TRUE))
illus
image_write(illus, path = "slides/img/gganimate_fireworks.PNG")
                      
illus <- image_read("slides/img/ggplot2_exploratory.png")
illus <- image_resize(illus, geometry_size_pixels(width = 500, preserve_aspect = TRUE))
illus
image_write(illus, path = "slides/img/ggplot2_exploratory.png")

illus <- image_read("slides/img/ggplot2_masterpiece.png")
illus <- image_resize(illus, geometry_size_pixels(width = 500, preserve_aspect = TRUE))
illus
image_write(illus, path = "slides/img/ggplot2_masterpiece.png")

illus <- image_read("slides/img/patchwork_horst.png")
illus <- image_resize(illus, geometry_size_pixels(width = 500, preserve_aspect = TRUE))
illus <- illus %>% image_crop("450x300+20")
image_write(illus, path = "slides/img/patchwork_horst.png")
illus
