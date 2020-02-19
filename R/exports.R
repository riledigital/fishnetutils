# Functions for exporting


# Setup directories
root <- rprojroot::find_rstudio_root_file()
setwd("~/LocalDev/AV_YelpData/AV_YelpData_R")


## we are going to set the data file path here
data_in <- file.path(root, 'data_in')
data_out <- file.path(root, 'data_out')


fishnets_outputs <- c()



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


process_county <- function(x) {
  print(as.numeric(x['crs_utm']))
  county_bounds <-
    create_county_boundaries(statey = x['fips_state'], countyy = x['fips_county'])
  fishnet_clipped <-
    create_fishnet_clip(inputty = county_bounds,
                        crs_to = as.numeric(x['crs_utm'])) %>%
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
    sf::st_write(obj = to_save,
                 dsn = data_out,
                 update = TRUE)
  }
