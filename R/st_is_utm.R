#' Verificar si un objeto sf tiene proyección UTM
#'
#' @param sf Objeto de clase 'sf' o 'sfc'
#'
#' @return Logical. TRUE si el CRS es UTM, FALSE en caso contrario.
#' @export
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#' st_is_utm(nc)
#' }
st_is_utm <- function(sf) {
  if (!inherits(sf, c("sf", "sfc"))) stop("El objeto 'sf' debe ser de clase 'sf' o 'sfc'.")
  
  crs_info <- sf::st_crs(sf)
  if (is.na(crs_info)) {
    warning("El objeto no tiene un CRS definido.")
    return(FALSE)
  }

  if (crs_info$IsGeographic) {
    return(FALSE)
  }

  wkt <- crs_info$wkt
  if (!is.null(wkt) && !is.na(wkt) && grepl("UTM zone", wkt, ignore.case = TRUE)) {
    return(TRUE)
  }
  
  proj4 <- crs_info$proj4string
  if (!is.null(proj4) && !is.na(proj4) && grepl("\\+proj=utm", proj4, ignore.case = TRUE)) {
    return(TRUE)
  }
  
  return(FALSE)
}
