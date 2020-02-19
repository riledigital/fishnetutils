library(tigris)
library(sf)

#' Title
#'
#' @param state_name Character,
#' @param county_name Character,
#' @param crs_in Numeric, int,
#' @param buffer_width Int numeric, assumes unit of crs_in. Width of the buffer to build around the road
#'
#' @return SFC containing a multipolygon showing a road buffer.
#' @export
#'
#' @examples
make_buffered_road <-
  function(state_name,
           county_name,
           crs_in,
           buffer_width) {
    ## Takes in specifications for roads in a county,
    ## returned the buffered areas
    shape <- tigris::roads(state = state_name,
                           county = county_name,
                           class = 'sf') %>%
      sf::st_transform(., crs = crs_in) %>%
      sf::st_buffer(x = ., dist = buffer_width) %>%
      sf::st_combine(x = .) %>%
      sf::st_union(x = . , by_feature = TRUE)
    return(shape)
  }


# SOME VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326


## TODO: Func that saves files for convenience.
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
