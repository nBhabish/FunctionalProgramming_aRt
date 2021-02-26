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