library(tigris)
library(sf)

make_buffered_road <- function(state_name, county_name, crs_in) {
  ## Takes in specifications for roads in a county,
  ## returned the buffered areas
  shape <- roads(state = state_name,
                 county = county_name,
                 class = 'sf') %>%
    st_transform(., crs = crs_in) %>%
    st_buffer(1500) %>%
    st_combine() %>%
    st_union(x = . , by_feature = TRUE)
  return(shape)
}
options(tigris_use_cache = FALSE)

water_only <- area_water('CA', county= 'Mariposa', class = 'sf', refresh = TRUE) %>%
  st_transform(crs=3310) %>% st_simplify(dTolerance = 300)

plot(water_only)

pts_output <- create_fishnet_clip(
  make_buffered_road('CA', 'Mariposa', 3310), crs_to = 3310)

points_no_water <- st_difference(pts_output, water_only)


# README -----------------------------------------------------------------------
# This script takes in a .csv of study areas and outputs geojson files
# that describe a fishnet, water is clipped out

# PACKAGE SETUP
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
  'spData'
)

p_load(
  char = Packages,
  character.only = TRUE,
  install = TRUE,
  update = TRUE
)

# SOME VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326

tigris_cache_dir(path = data_cache)
Sys.getenv('TIGRIS_CACHE_DIR')

plot(county_montgomery %>% st_bbox())

fetch_county_geographies <- function(fips_state, fips_county) {
  out <- county_subdivisions(
    class = "sf",
    county = fips_county,
    cb = TRUE,
    state = fips_state
  )
  save_file(
    out,
    'county_subdivs' ,
    format = 'gpkg',
    county_name = fips_county,
    county_abbrev = fips_county
  )
  return(out)
}

create_county_boundaries <- function(statey, countyy) {
  ## This function gets a county and dissolves it...
  county <- fetch_county_geographies(fips_state = statey,
                                     fips_county = countyy)
  print(paste('The CRS of incoming is:', st_crs(county)))
  ## Dissolve!
  output <- county %>% st_union(., by = STATEFP)
  return(output)
}


create_fishnet_clip <- function(inputty, crs_to) {
  print(paste0('inputty CRS is: ', st_crs(inputty)))
  ## These need to be projected beforehand.
  input <- inputty %>% st_transform(x = ., crs = crs_to)
  fishnet <- input %>% st_bbox() %>%
    st_make_grid(x = .,
                 cellsize = 500,
                 what = 'centers')
  # plot(fishnetty)
  st_set_crs(value = st_crs(input), x = fishnet)
  pts_output <- st_intersection(fishnet, input)
  ## TODO Figrue out why the output is badly projected?
  return(pts_output)
}

fishnets_outputs <- c()

process_county <- function(x) {
  print(as.numeric(x['crs_utm']))
  county_bounds <-
    create_county_boundaries(statey = x['fips_state'], countyy = x['fips_county'])
  fishnet_clipped <-
    create_fishnet_clip(inputty = county_bounds, crs_to = as.numeric(x['crs_utm'])) %>%
    st_transform(x = ., crs = CRS_WGS84)
  ## Then do the export!
  # append(x = fishnets_outputs, values = fishnet_clipped)
  county_name <- as.character(x['Names'])
  county_abbrev <- as.character(x['name_abbrev'])
  save_file(fishnet_clipped,
            'fishnet',
            'geojson',
            county_name,
            county_abbrev)
  save_file(fishnet_clipped,
            'fishnet',
            'gpkg',
            county_name,
            county_abbrev)
  save_file(county_bounds,
            'bounds_dissolved',
            'geojson',
            county_name,
            county_abbrev)
  save_file(county_bounds,
            'bounds_dissolved',
            'gpkg',
            county_name,
            county_abbrev)
  save_file(county_bounds,
            'bounds_dissolved',
            'geojson',
            county_name,
            county_abbrev)
  save_file(county_bounds,
            'bounds_dissolved',
            'gpkg',
            county_name,
            county_abbrev)

  return(fishnet_clipped)
}


save_file <-
  function(to_save,
           geo_type,
           format,
           county_name,
           county_abbrev) {
    dir.create(file.path(data_out, geo_type))

    data_out <-
      file.path(data_out,
                geo_type,
                paste0(as.character(county_name), county_abbrev, '.' , format))

    # print('Produced file:')
    st_write(obj = to_save,
             dsn = data_out,
             update = TRUE)
  }

#jobs %>% walk(.x = ., .f = process_county)

# Folder Setup ----------------------------------------------------------

jobs <-
  read_csv(
    file = file.path(data_in, 'study_areas.csv'),
    col_types = list(
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_number(),
      col_character()
    )
  )

# Folder Setup ----------------------------------------------------------


# Run the Data Job ----------------------------------------------------------
#
# data_in
#
# # nrow(jobs)
# jobs %>% by(data = .,
#             FUN = process_county,
#             INDICES = 1:nrow(.))
