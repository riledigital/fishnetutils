# SOME VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326

#' make_buffered_road
#'
#' @param state_name Character,
#' @param county_name Character,
#' @param crs_in Numeric, int, a planar CRS
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


## TODO: Func that saves files for convenience.
fetch_county_geographies <- function(fips_state, fips_county) {
  out <- tigris::county_subdivisions(
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


#' create_fishnet_clip
#'
#' @param inputty SF or SFC
#' @param crs_to Int or numeric. planar projected
#' @param cell_size Int or numeric
#'
#' @return SFC of fishnet points, not clipped
#' @export
#'
#' @examples
create_fishnet_clip <- function(inputty, crs_to, cell_size = 5280) {
  print("Warning we are using a lousy function...")
  print(paste0('inputty CRS is: ', sf::st_crs(inputty)))

  ## These need to be projected beforehand.
  input <- inputty %>%
    sf::st_transform(x = ., crs = crs_to)
  fishnet <- input %>%
    sf::st_bbox() %>%
    sf::st_make_grid(x = .,
                     cellsize = cell_size,
                     what = 'centers')
  print('fishnet CRS is....')
  print(sf::st_crs(fishnet))
  # plot(fishnetty)
  sf::st_set_crs(x = fishnet, value = sf::st_crs(input))

  pts_output <- sf::st_intersection(fishnet, input)
  ## TODO Figrue out why the output is badly projected?
  return(pts_output)
}
