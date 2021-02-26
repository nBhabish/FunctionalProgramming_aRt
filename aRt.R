#Ctrl shift R for labeling code chunk 
# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)
source(file = here("~/Desktop/Repositories/FunctionProgramming_aRt/helpers.R"))
source(file = here("~/Desktop/Repositories/FunctionProgramming_aRt/workers.R"))


# do stuff ----------------------------------------------------------------

tips <- grow_sapling()
tree <-  tips

for(i in 1:3){
  
  tips <- grow_from(tips)
  tree <- bind_rows(tree, tips)
  
  
}

pic <- draw_tree(tree)
plot(pic)




