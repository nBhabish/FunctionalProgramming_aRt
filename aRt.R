#Ctrl shift R for labelling code chunk 
# Setup -------------------------------------------------------------------

library(tidyverse)
set.seed(1)


# Helper Function ---------------------------------------------------------


radians <- function(degree) {
  prop <- (degree / 360)
  rads <- prop *  2 * pi
  return(rads)
}


adjust_scale <- function(scale) {
  all_scales <- c(.8, .9, .95)
  new_scales <-
    scale * sample(x = all_scales,
                   size = length(scale),
                   replace = TRUE)
  return(new_scales)
}

adjust_angle <- function(angle){
  all_angles <- c(-10, -5, 0, 5, 15, 20, 25)
  new_angles <-
    angle + sample(x = all_angles,
                   size = length(angle),
                   replace = TRUE)
  return(new_angles)
  
}

adjust_x <- function(old_x, angle, scale){
  new_x <- old_x + scale * cos(radians(angle))
  return(new_x)
}

adjust_y <- function(old_y, angle, scale){
  new_y <- old_y + scale * sin(radians(angle))
  return(new_y)
}
# Workers --------------------------------------------------------------------


grow_sapling <- function() {
  sapling <- tibble(
    old_x = 0,
    old_y = 0,
    new_x = 0,
    new_y = 1,
    scale = 1,
    angle = 90
  )
  
  return(sapling)
}



grow_from <- function(tips) {
  new_growth <- tips %>%
    mutate(
      old_x = new_x,
      old_y = new_y,
      angle = adjust_angle(angle),
      scale = adjust_scale(scale),
      new_x = adjust_x(old_x, angle, scale),
      new_y = adjust_y(old_y, angle, scale)
    )
  
  return(new_growth)
}



draw_tree <- function(tree) {
  pic <- ggplot(data = tree,
                mapping = aes(
                  x = old_x,
                  y = old_y,
                  xend = new_x,
                  yend = new_y
                )) +
    geom_segment() +
    geom_point()+
    theme_void() +
    coord_equal()
  
  return(pic)
}


# do stuff ----------------------------------------------------------------

tips <- grow_sapling()
tree <-  tips

for(i in 1:3){
  
  tips <- grow_from(tips)
  tree <- bind_rows(tree, tips)
  
  
}

pic <- draw_tree(tree)
plot(pic)




