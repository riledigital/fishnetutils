# Functions that download stuff

fetch_county_geographies <- function(fips_state, fips_county) {
  out <- tigris::county_subdivisions(
    class = "sf",
    county = fips_county,
    cb = TRUE,
    state = fips_state
  )
  save_file(out, 'county_subdivs',format = 'gpkg',
            county_name = fips_county, county_abbrev = fips_county)
  return(out)
}

#' create_county_boundaries
#'
#' @param statey String
#' @param countyy String
#'
#' @return SFC
#' @export
#'
#' @examples
#' create_county_boundaries(statey = 'MA', countyy = 'Norfolk')
create_county_boundaries <- function(statey, countyy) {
  ## This function gets a county and dissolves it...
  county <- tigris::county_subdivisions(
    class = "sf",
    county = countyy,
    cb = TRUE,
    state = statey
  )
  print(paste('The CRS of incoming is:', sf::st_crs(county)))
  ## Dissolve!
  output <-
    sf::st_union(x = county, by = 'STATEFP')
  return(output)
}

