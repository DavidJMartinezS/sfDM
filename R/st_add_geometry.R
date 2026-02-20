#' Agregar coordenadas X e Y a un objeto sf
#'
#' Extrae las coordenadas de la geometría (asumiendo geometrías de tipo POINT) y las agrega
#' como nuevas columnas al objeto sf.
#'
#' @param sf Objeto de clase `sf`. Debe contener geometrías de tipo POINT.
#' @param names Vector de caracteres de longitud 2. Nombres para las columnas de coordenadas (X, Y).
#'   Por defecto c("UTM_E", "UTM_N").
#'
#' @return El objeto `sf` original con dos columnas nuevas correspondientes a las coordenadas.
#' @export
#' @examples
#' \dontrun{
#'  # Crea los campos 'UTM_E' y 'UTM_N' por defecto
#'  sf_puntos %>% st_add_coords(sf_puntos)
#'  # Asignar nombre a los campos con las coordenadas
#'  sf_puntos %>% st_add_coords(sf_puntos, names = c("Coord_X", "Coord_Y"))
#' }
st_add_coords <- function(sf, names = c("UTM_E", "UTM_N")) {
  if (!inherits(sf, "sf")) stop("El argumento 'sf' debe ser un objeto de clase 'sf'.")
  if (!is.character(names) || length(names) != 2) stop("El argumento 'names' debe ser un vector de caracteres de longitud 2.")

  if (nrow(sf::st_coordinates(sf)) != nrow(sf)) {
    warning("El número de coordenadas extraídas no coincide con el número de filas del objeto sf. Se calcularán las coordenadas del centroide.")
  }

  coords <- sf::st_coordinates(sf::st_centroid(sf))

  sf %>% 
    dplyr::mutate(
      !!names[[1]] := coords[, 1],
      !!names[[2]] := coords[, 2]
    )
}

#' Agregar superficie en hectáreas
#'
#' Calcula el área de las geometrías y agrega una columna 'Sup_ha' con el valor en hectáreas.
#'
#' @param sf Objeto de clase `sf`.
#' @param digits Entero. Número de decimales para redondear. Por defecto 2.
#'
#' @return El objeto `sf` con la columna 'Sup_ha'.
#' @export
#' @examples
#' \dontrun{
#'  sf_poligonos <- st_add_sup_ha(sf_poligonos)
#'  sf_poligonos <- st_add_sup_ha(sf_poligonos, digit = 0)
#' }
st_add_sup_ha <- function(sf, digits = 2) {
  if (!inherits(sf, "sf")) stop("El argumento 'sf' debe ser un objeto de clase 'sf'.")
  if (!is.numeric(digits) || length(digits) != 1) stop("El argumento 'digits' debe ser un valor numérico único.")

  sf %>% 
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(digits)
    )
}

#' Agregar superficie en metros cuadrados
#'
#' Calcula el área de las geometrías y agrega una columna 'Sup_m2' con el valor en metros cuadrados.
#'
#' @param sf Objeto de clase `sf`.
#' @param digits Entero. Número de decimales para redondear. Por defecto 0.
#'
#' @return El objeto `sf` con la columna 'Sup_m2'.
#' @export
#' @examples
#' \dontrun{
#'  sf_poligonos <- st_add_sup_m2(sf_poligonos)
#'  sf_poligonos <- st_add_sup_m2(sf_poligonos, digits = 1)
#' }
st_add_sup_m2 <- function(sf, digits = 0) {
  if (!inherits(sf, "sf")) stop("El argumento 'sf' debe ser un objeto de clase 'sf'.")
  if (!is.numeric(digits) || length(digits) != 1) stop("El argumento 'digits' debe ser un valor numérico único.")

  sf %>% 
    dplyr::mutate(
      Sup_m2 = sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(digits)
    )
}