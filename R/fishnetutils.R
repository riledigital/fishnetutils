.onAttach <- function(libname, pkgname) {
  packageStartupMessage("fishnettools loaded!")
}

## OLD CODE TO MOVE...
# SOME GLOBAL VARIABLES ---------------------------------------------------------------
CRS_WGS84 <- 4326

# Fix tigris cache folder in case the downloads corrupt/fail
# tigris::tigris_cache_dir(path = data_cache)
# Sys.getenv('TIGRIS_CACHE_DIR')



