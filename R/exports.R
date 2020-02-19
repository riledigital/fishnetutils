# Functions for exporting


# # Setup directories
# root <- rprojroot::find_rstudio_root_file()
# setwd("~/LocalDev/AV_YelpData/AV_YelpData_R")


## we are going to set the data file path here
# data_in <- file.path(root, 'data_in')
data_out <- file.path('./data_out')
#

# fishnets_outputs <- c()

#' save_file
#'
#' @param to_save SF or SFC
#' @param geo_type Character or string
#' @param format Character or string
#' @param county_name Character or string
#' @param county_abbrev Character or string
#'
#' @return Empty, saves output file
#' @export
#'
#' @examples
save_file <-
  function(to_save,
           geo_type,
           format,
           county_name,
           county_abbrev) {
    data_out <- fs::dir_create(fs::path_wd('data_out'))
    # Create the new directory for each geography type
    fs::dir_create(data_out, geo_type)

    data_out <-
      fs::path(data_out,
               geo_type,
               paste0(as.character(county_name), county_abbrev, '.' , format))

    if (fs::dir_exists(fs::path_wd('data_out', geo_type)))  {
      update_existing <- TRUE
    } else {
      update_existing <- FALSE
    }

    # print('Produced file:')
    sf::st_write(obj = to_save,
                 dsn = data_out,
                 update = update_existing)
  }



#' save_all_outputs
#'
#' @param x df row
#'
#' @return None since requires walk
#' @export
#'
#' @examples
save_all_outputs <- function(x) {
  # TODO x is a row listing used for batch processing
  # print(x)
  print(as.numeric(x['crs_epsg']))

  county_bounds <-
    create_county_boundaries(statey = x['fips_state'],
                             countyy = x['fips_county'])

  # TODO Test if this is projected right
  buffer_road <- make_buffered_road(state_name = x['fips_state'],
                                        county_name = x['fips_county'],
                                        crs_in = as.numeric(x['crs_epsg']),
                                        buffer_width = 600)

  fishnet_clipped <-
    create_fishnet_clip(inputty = buffer_road,
                        crs_to = sf::st_crs(buffer_road)) %>%
    sf::st_transform(x = ., crs = CRS_WGS84)
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
#
# jobs <-
#   readr::read_csv(
#     file = file.path('study_areas.csv'),
#     col_types = list(
#       tidyverse::col_character(),
#       tidyverse::col_character(),
#       tidyverse::col_character(),
#       tidyverse::col_character(),
#       tidyverse::col_number(),
#       tidyverse::col_character()
#     )
#   )
