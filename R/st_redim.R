#' Redimensionar objeto stars
#'
#' @param x
#'
#' @returns
#'
#' @export
#' @examples
st_redim <- function(x) {
  dm <- stars::st_dimensions(x)
  x %>% 
    stars::st_set_dimensions("x", offset = dm$x$offset + (dm$x$delta * (dm$x$from - 1)), delta = dm$x$delta) %>% 
    stars::st_set_dimensions("y", offset = dm$y$offset + (dm$y$delta * (dm$y$from - 1)), delta = dm$y$delta) %>% 
    sf::st_set_crs(dm$y$refsys)
}