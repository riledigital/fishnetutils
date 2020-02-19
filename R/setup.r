# Package Setup
install.packages('pacman')
devtools::install_github('rstudio/packrat')
library('pacman')

p_load(
  char = Packages,
  character.only = TRUE,
  install = TRUE,
  update = TRUE
)

# remotes::install_github("https://github.com/richierocks/yelp")

packrat::snapshot()

options(tigris_use_cache = FALSE)
