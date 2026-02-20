#' Posición relativa a una geometría de referencia
#'
#' Calcula la posición cardinal (Norte/Sur o Este/Oeste) de un objeto sf ('x')
#' con respecto al punto más cercano de otro objeto sf de referencia ('y').
#'
#' @param x Objeto `sf` o `sfc` (preferiblemente un punto) cuya posición se quiere determinar.
#' @param y Objeto `sf` o `sfc` que sirve como línea o polígono de referencia.
#' @param rel Cadena de caracteres. Define el eje de comparación: "NS" para
#'   Norte-Sur (compara coordenadas Y) o "EO" para Este-Oeste (compara coordenadas X).
#'
#' @return Una cadena de caracteres: "Norte", "Sur", "Este", "Oeste" o "Dentro" cuando se calcula la posición respecto de un polígono.
#' @export
#' @examples
#' \dontrun{
#'   library(dplyr)
#'   # Posición de un punto respecto del rio Elqui
#'   punto <- sf::st_sample(sf::st_buffer(rio_elqui, 100), 1)
#'   st_position(punto, rio_elqui)
#'   st_position(sf::st_buffer(punto, 500), rio_elqui)
#' 
#'   # posición de un punto respecto dela región de Valparaíso
#'   punto_valpo <- sf::st_sample(sf::st_buffer(valparaiso, 0), 1)
#'   st_position(punto_valpo, valparaiso)
#' 
#'   # posición de un punto respecto dela región de Valparaíso
#'   st_position(sf::st_buffer(punto_valpo, 20000), valparaiso)
#'   st_position(sf::st_buffer(punto_valpo, 20000), rio_elqui)
#' }
st_position <- function(x, y, rel = c("NS", "EO")) {
  if (!inherits(x, c("sf", "sfc"))) stop("El argumento 'x' debe ser un objeto de clase 'sf' o 'sfc'.")
  if (!inherits(y, c("sf", "sfc"))) stop("El argumento 'y' debe ser un objeto de clase 'sf' o 'sfc'.")
  rel <- match.arg(rel)

  if (sf::st_crs(x) != sf::st_crs(y)) {
    y <- sf::st_transform(y, sf::st_crs(x))
  }
  y_union <- sf::st_union(y)

  linea_conector <- sf::st_nearest_points(x, y_union)
  coords <- sf::st_coordinates(linea_conector)

  unname(switch(rel,
    "NS" = { ifelse(coords[1, "Y"] == coords[2, "Y"], "Dentro", ifelse(coords[1, "Y"] > coords[2, "Y"], "Norte", "Sur")) },
    "EO" = { ifelse(coords[1, "X"] == coords[2, "X"], "Dentro", ifelse(coords[1, "X"] > coords[2, "X"], "Este", "Oeste")) }
  ))
}
