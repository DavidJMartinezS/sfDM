#' Buffer por elemeno
#'
#' @param sf ovjeto 'sf'
#' @param field Campo con la distancia de buffer en metros
#'
#' @returns 
#'
#' @export
#' @examples
st_buffer_byrow <- function(sf, field) {
  if (!inherits(sf, "sf")) stop("El objeto 'sf' debe ser de clase 'sf'.")
  arg_val <- tryCatch(field, error = function(e) NULL)
  if (is.list(arg_val) && all(sapply(arg_val, rlang::is_quosure))) {
    col_df <- sf %>% dplyr::select(!!!arg_val) %>% sf::st_drop_geometry()
  } else {
    col_df <- sf %>% dplyr::select({{ arg_val }}) %>% sf::st_drop_geometry()
  }
  if (ncol(col_df) != 1) stop("Debe seleccionar exactamente una columna para el buffer.")
  col_name <- names(col_df)[1]
  if (!is.numeric(col_df[[1]])) stop(paste("El campo", col_name, "debe ser de tipo numérico."))
  
  sf %>% 
    tidyr::drop_na(dplyr::all_of(col_name)) %>% 
    sf::st_buffer(dist = .[[col_name]])
}
