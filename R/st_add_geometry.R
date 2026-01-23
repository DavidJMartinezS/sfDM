#' Title
#'
#' @param sf
#' @param names
#'
#' @returns
#'
#' @export
#' @examples
st_add_coords <- function(sf, names = c("UTM_E", "UTM_N")) {
  sf %>% 
    dplyr::mutate(
      names[[1]] = sf::st_coordinates(geometry)[,1],
      names[[2]] = sf::st_coordinates(geometry)[,2]
    )
}

#' Agregar superficie en metros
#'
#' @param sf
#' @param digits
#'
#' @returns
#'
#' @export
#' @examples
st_add_sup_ha <- function(sf, digits = 2) {
  sf %>% 
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(digits)
    )
}

#' Agregar superficie en metros
#'
#' @param sf
#' @param digits
#'
#' @returns
#'
#' @export
#' @examples
st_add_sup_m2 <- function(sf, digits = 0) {
  sf %>% 
    dplyr::mutate(
      Sup_m2 = sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(digits)
    )
}