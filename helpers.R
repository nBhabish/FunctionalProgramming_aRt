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