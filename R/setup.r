# Package Setup
install.packages('pacman')
devtools::install_github('rstudio/packrat')
library('pacman')

Packages <- c(
  'tidyverse',
  'dplyr',
  'tidyr',
  'readr',
  'devtools',
  'googleway',
  'rmarkdown',
  'sf',
  'purrr',
  'lwgeom',
  'tmap',
  'raster',
  'tigris',
  'remotes',
  'yelp',
  'spData')

p_load(
  char = Packages,
  character.only = TRUE,
  install = TRUE,
  update = TRUE
)

# remotes::install_github("https://github.com/richierocks/yelp")

packrat::snapshot()

# Setup directories
root <- rprojroot::find_rstudio_root_file()
setwd("~/LocalDev/AV_YelpData/AV_YelpData_R")


## we are going to set the data file path here
data_in <- file.path(root, 'data_in')
data_out <- file.path(root, 'data_out')

