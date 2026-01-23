#' Title
#'
#' @param sf
#' @param distance
#'
#' @returns
#'
#' @export
#' @examples
group_by_distance <- function(sf, distance){
  dist_matrix = sf::st_distance(x, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(igraph::components(g)$membership)
}
