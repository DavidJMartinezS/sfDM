#' Agrupar geometrías por proximidad espacial
#'
#' Esta función agrupa las geometrías de un objeto `sf` que se encuentran dentro de una
#' distancia umbral especificada. Utiliza componentes conectados en un grafo de adyacencia.
#'
#' @param sf Objeto de clase `sf` o `sfc`.
#' @param distance Valor numérico. Distancia máxima en metros para considerar dos elementos como conectados.
#'
#' @return Un vector de enteros indicando el ID del grupo (membership) para cada geometría.
#' @export
#' @examples
#' \dontrun{
#'   # Agrupar geometrías que distan menos de 50 metros entre sí
#'   sf_obj$group <- group_by_distance(sf_obj, distance = 50)
#'   # Agrupar geometrias en base a una agrupación de atributos
#'   variables <- c("Grupo_1", "Grupo_2")
#'   sf_obj %>% 
#'    dplyr::group_by(!!dplyr::syms(variables)) %>% 
#'    dplyr::mutate(group = group_by_distance(geometry, distance = 50))
#' }
group_by_distance <- function(sf, distance){
  if (!inherits(sf, c("sf", "sfc"))) stop("El argumento 'sf' debe ser un objeto de clase 'sf' o 'sfc'.")
  if (!is.numeric(distance) || length(distance) != 1) stop("El argumento 'distance' debe ser un valor numérico único.")

  dist_matrix = sf::st_distance(sf, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(igraph::components(g)$membership)
}
