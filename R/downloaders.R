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

create_county_boundaries <- function(statey, countyy) {
  ## This function gets a county and dissolves it...
  county <- fetch_county_geographies(fips_state = statey,
                                     fips_county = countyy)
  print(paste('The CRS of incoming is:', st_crs(county)))
  ## Dissolve!
  output <- county %>% st_union(., by = STATEFP)
  return(output)
}
