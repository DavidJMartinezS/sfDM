#' Posicion desde rio Elqui
#'
#' @param sf objeto sf 
#'
#' @returns
#'
#' @export
#' @examples
st_position_from_elqui <- function(punto) {
  if (sf::st_crs(punto) != sf::st_crs(rio)) {
    rio <- sf::st_transform(rio, sf::st_crs(punto))
  }
  rio_union <- sf::st_union(rio)

  linea_conector <- sf::st_nearest_points(punto, rio_union)
  
  coords <- sf::st_coordinates(linea_conector)
  
  coords %>% dplyr::group_by()

  y_punto <- coords[1, "Y"]
  y_rio   <- coords[2, "Y"]
  
  if (y_punto > y_rio) {
    return("Norte")
  } else {
    return("Sur")
  }
}