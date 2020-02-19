

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("fishnettools loaded")
}

## OLD CODE TO MOVE...
# SOME GLOBAL VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326


# Fix tigris cache folder in case the downloads corrupt/fail
tigris::tigris_cache_dir(path = data_cache)
Sys.getenv('TIGRIS_CACHE_DIR')


#' create_fishnet_clip
#'
#' @param inputty sf, sfc
#' @param crs_to int, numeric
#'
#' @return SFC of a full land area fishnet
#' @export
#'
#' @examples
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

