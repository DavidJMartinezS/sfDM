#' Buffer variable por fila
#'
#' Aplica un buffer a cada geometría del objeto `sf` utilizando una distancia variable
#' especificada en una columna del mismo objeto.
#'
#' @param sf Objeto de clase `sf`.
#' @param field Nombre o posición del campo que contiene la distancia del buffer en metros para cada fila.
#'
#' @return Un objeto `sf` con las geometrías resultantes del buffer. Las filas con valores `NA`
#'   en la columna de distancia son eliminadas.
#' @export
#' @examples
#' \dontrun{
#'   # Usando nombre de columna como texto
#'   sf_buffer <- st_buffer_byrow(sf_obj, "distancia_m")
#'   
#'   # Usando nombre de columna sin comillas
#'   sf_buffer <- st_buffer_byrow(sf_obj, distancia_m)
#'   
#'   # Usando posición de la columna
#'   sf_buffer <- st_buffer_byrow(sf_obj, 1)
#' }
st_buffer_byrow <- function(sf, field) {
  if (!inherits(sf, "sf")) stop("El argumento 'sf' debe ser un objeto de clase 'sf'.")
  
  # Seleccionar la columna usando tidy evaluation y validar existencia
  col_df <- tryCatch({
    sf %>% sf::st_drop_geometry() %>% dplyr::select({{ field }})
  }, error = function(e) stop("No se pudo seleccionar la columna especificada. Verifique el nombre."))
  
  if (ncol(col_df) != 1) stop("Debe seleccionar exactamente una columna para el buffer.")
  col_name <- names(col_df)[1]
  if (!is.numeric(col_df[[1]])) stop(paste("El campo", col_name, "debe ser de tipo numérico."))
  
  sf %>% 
    tidyr::drop_na(dplyr::all_of(col_name)) %>% 
    sf::st_buffer(dist = .[[col_name]])
}
