#' Redimensionar objeto stars
#'
#' @description
#' Actualiza las dimensiones espaciales ('x' e 'y') de un objeto `stars`.
#' Calcula el nuevo `offset` considerando el índice de inicio (`from`) y la resolución (`delta`),
#' lo cual es útil para corregir la extensión espacial después de realizar recortes o subsets
#' que no actualizan automáticamente estos metadatos.
#'
#' @param x Un objeto de clase `stars`.
#'
#' @return Un objeto de clase `stars` con las dimensiones actualizadas.
#'
#' @export
#' @examples
#' \dontrun{
#'   # x <- stars::read_stars("archivo.tif")
#'   # y <- sf::read_sf("poligono.shp") %>% sf::st_transform(sf::st_crs(x))
#'   # x_y <- x[y]
#'   # x_redim <- st_redim_stars(x_y)
#' }
st_redim_stars <- function(x) {
  checkmate::assert_class(x, "stars")
  dm <- stars::st_dimensions(x)
  x %>% 
    stars::st_set_dimensions("x", offset = dm$x$offset + (dm$x$delta * (dm$x$from - 1)), delta = dm$x$delta) %>% 
    stars::st_set_dimensions("y", offset = dm$y$offset + (dm$y$delta * (dm$y$from - 1)), delta = dm$y$delta) %>% 
    sf::st_set_crs(dm$y$refsys)
}