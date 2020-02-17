.onAttach <- function(libname, pkgname) {
  packageStartupMessage("fishnettools loaded")
}

# SOME GLOBAL VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326

# Folder Setup -----------------------------------------------------------------

# Setup folders
dir.create(file.path('data_out'))
data_cache <- file.path(data_out, 'cached')
dir.create(data_cache)

# business logic
# ------------------------------------------------------------------------------

# Fix tigris cache folder in case the downloads corrupt/fail
tigris::tigris_cache_dir(path = data_cache)
Sys.getenv('TIGRIS_CACHE_DIR')



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

data_in

# nrow(jobs)
jobs %>% by(data = .,
            FUN = process_county,
            INDICES = 1:nrow(.))
