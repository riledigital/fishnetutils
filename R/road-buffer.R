# options(tigris_use_cache = FALSE)

#' make_buffered_road
#'
#' @param state_name string, state abbreviation
#' @param county_name numeric 3 digit county code
#' @param crs_in numeric, EPSG of the local CRS for the county
#'
#' @return SF object of a buffered area around roads in the county
#' @export
#'
#' @examples
#'
make_buffered_road <- function(state_name, county_name, crs_in) {
  ## Takes in specifications for roads in a county,
  ## returned the buffered areas
  shape <- roads(state = state_name,
                 county = county_name,
                 class = 'sf') %>%
    st_transform(., crs = crs_in) %>%
    st_buffer(500) %>%
    st_combine() %>%
    st_union(x = . , by_feature = TRUE)
  return(shape)
}

water_only <-
  area_water('CA',
             county = 'Mariposa',
             class = 'sf',
             refresh = TRUE) %>%
  st_transform(crs = 3310) %>% st_simplify(dTolerance = 300)

# plot(water_only)
#
# pts_output <- create_fishnet_clip(make_buffered_road('CA', 'Mariposa', 3310), crs_to = 3310)
#
#
# fn_brookline <- create_fishnet_clip(make_buffered_road('MA', 021, 102686), crs_to = 102686)
# points_no_water <- st_difference(pts_output, water_only)


# README -----------------------------------------------------------------------
# This script takes in a .csv of study areas and outputs geojson files
# that describe a fishnet, water is clipped out


# SOME VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326

# tigris_cache_dir(path = data_cache)
# Sys.getenv('TIGRIS_CACHE_DIR')

# plot(county_montgomery %>% st_bbox())

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


#' create_fishnet_clip
#'
#' @param county_geometry sf/sfc, county area
#' @param crs_to numeric, desired output CRS
#'
#' @return
#' @export
#'
#' @examples
create_fishnet_clip <- function(county_geometry, crs_to) {
  print(paste0('county_geometry CRS is: ', st_crs(county_geometry)))
  input <- county_geometry %>%
    st_transform(x = ., crs = crs_to)
  # Cell size can get wonky
    fishnet <-
    input %>%
    st_bbox() %>%
    st_make_grid(x = .,
                 cellsize = 500*10,
                 what = 'centers')
  # return(fishnet)
  st_set_crs(x = fishnet, value = sf::st_crs(input))
  return(st_intersection(fishnet, input))
}

fn_brookline <- create_fishnet_clip(county_geometry = brookline, crs_to = 102686)

sf::write_sf(fn_brookline, dsn='brookline-fn23.gpkg')

# fishnets_outputs <- c()

process_county <- function(x) {
  print(as.numeric(x['crs_utm']))
  county_bounds <-
    create_county_boundaries(statey = x['fips_state'], countyy = x['fips_county'])
  fishnet_clipped <-
    create_fishnet_clip(county_geometry = county_bounds, crs_to = as.numeric(x['crs_utm'])) %>%
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

# jobs <-
#   read_csv(
#     file = file.path(data_in, 'study_areas.csv'),
#     col_types = list(
#       col_character(),
#       col_character(),
#       col_character(),
#       col_character(),
#       col_number(),
#       col_character()
#     )
#   )


# Run the Data Job ----------------------------------------------------------
#
# data_in
#
# # nrow(jobs)
# jobs %>% by(data = .,
#             FUN = process_county,
#             INDICES = 1:nrow(.))
