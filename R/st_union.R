#' Title
#'
#' @param x
#' @param y
#'
#' @returns
#'
#' @export
#' @examples
st_union_features <- function(x, y) {
  sf::st_agr(x) = "constant"
  sf::st_agr(y) = "constant"
  x %>% sf::st_difference(sf::st_union(y)) %>% dplyr::bind_rows(sf::st_intersection(x, y))
}


#' Title
#'
#' @param x
#' @param y
#' @param sel_var_y
#'
#' @returns
#'
#' @export
#' @examples
st_union_no_overlaps <- function(x, y, sel_var_y = dplyr::everything()){
  sf::st_agr(x) = "constant"
  sf::st_agr(y) = "constant"
  x %>%
    sf::st_intersection(y %>% dplyr::select(sel_var_y)) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    dplyr::bind_rows(
      y %>%
        sf::st_difference(sf::st_union(sf::st_combine(x))) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON")
    ) %>%
    dplyr::bind_rows(
      x %>%
        sf::st_difference(sf::st_union(sf::st_combine(y))) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON")
    ) %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON")
}