# Functions for exporting



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
