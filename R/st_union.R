#' Unir características de dos capas sf (Intersección + Diferencia)
#'
#' @description
#' Realiza una operación espacial que combina la intersección de `x` e `y`
#' con la diferencia de `x` menos `y`. El resultado cubre espacialmente la misma
#' área que `x`, pero con las geometrías divididas por los límites de `y` y
#' los atributos de `y` agregados en las zonas de intersección.
#'
#' @param x Objeto `sf` (capa base).
#' @param y Objeto `sf` (capa de superposición).
#'
#' @return Un objeto `sf` que contiene las geometrías resultantes.
#'
#' @export
#' @examples
#' \dontrun{
#'   # res <- st_union_features(poligonos_a, poligonos_b)
#' }
st_union_features <- function(x, y) {
  checkmate::assert_class(x, "sf")
  checkmate::assert_class(y, "sf")
  
  sf::st_agr(x) = "constant"
  sf::st_agr(y) = "constant"
  
  x %>% 
    sf::st_difference(sf::st_union(sf::st_geometry(y))) %>% 
    dplyr::bind_rows(sf::st_intersection(x, y))
}


#' Unión espacial completa sin superposiciones (Planarize)
#'
#' @description
#' Crea una cobertura completa de las geometrías de `x` e `y`, resolviendo superposiciones.
#' El resultado incluye:
#' 1. La intersección de `x` e `y`.
#' 2. La diferencia de `y` menos `x`.
#' 3. La diferencia de `x` menos `y`.
#'
#' Es útil para generar un mosaico de polígonos sin traslapes a partir de dos capas.
#'
#' @param x Objeto `sf` de polígonos.
#' @param y Objeto `sf` de polígonos.
#' @param sel_var_y Selección de variables de `y` a mantener (tidy-select).
#'
#' @return Un objeto `sf` de polígonos.
#'
#' @export
#' @examples
#' \dontrun{
#'   # mosaico <- st_union_no_overlaps(zona_a, zona_b)
#' }
st_union_no_overlaps <- function(x, y, sel_var_y = dplyr::everything()){
  checkmate::assert_class(x, "sf")
  checkmate::assert_class(y, "sf")
  
  sf::st_agr(x) = "constant"
  sf::st_agr(y) = "constant"
  
  x_union <- sf::st_union(sf::st_geometry(x))
  y_union <- sf::st_union(sf::st_geometry(y))
  
  x %>%
    sf::st_intersection(y %>% dplyr::select({{ sel_var_y }})) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    dplyr::bind_rows(
      y %>%
        sf::st_difference(x_union) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON")
    ) %>%
    dplyr::bind_rows(
      x %>%
        sf::st_difference(y_union) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON")
    ) %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON")
}