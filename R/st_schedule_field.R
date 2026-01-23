#' Programar puntos de terrenos
#'
#' @param sf
#' @param dias
#' @param cuadrillas
#' @param fecha_i
#' @param dias_sem
#'
#' @returns
#'
#' @export
#' @examples
st_schedule_field <- function(sf, dias, cuadrillas, fecha_i = NULL, dias_sem = 5) {
  if (!inherits(sf, "sf")) stop("El objeto 'sf' debe ser de clase 'sf'.")
  arg_val <- tryCatch(dias, error = function(e) NULL)
  if (is.list(arg_val) && all(sapply(arg_val, rlang::is_quosure))) {
    col_df <- sf %>% dplyr::select(!!!arg_val) %>% sf::st_drop_geometry()
  } else {
    col_df <- sf %>% dplyr::select({{ arg_val }}) %>% sf::st_drop_geometry()
  }
  if (ncol(col_df) != 1) stop("Debe indicar solo una columna con los dias necesarios usando una cuadrilla.")
  col_name <- names(col_df)[1]
  if (!col_name %in% names(sf)) stop(paste0("El campo '", col_name, "' no existe en el objeto sf."))
  if (!is.numeric(sf[[col_name]])) stop(paste0("El campo '", col_name, "' debe ser numérico."))
  
  sf <- sf %>% 
    mutate(
      Dia = ceiling(.data[[col_name]] / cuadrillas),
      Cuadrilla = (.data[[col_name]] - 1) %% cuadrillas + 1
    )
  
  if (!is.null(fecha_i)) {
    if (length(dias_sem) > 1 || !is.numeric(dias_sem) || dias_sem < 1 || dias_sem > 7) stop("dias_sem debe ser un número entre 3 y 7.")
    
    fecha_i <- lubridate::as_date(fecha_i)
    start_wday <- lubridate::wday(fecha_i, week_start = 1)
    allowed_wdays <- (start_wday - 1 + 0:(dias_sem - 1)) %% 7 + 1
    
    max_dia <- max(sf$Dia, na.rm = TRUE)
    dates_seq <- seq(from = fecha_i, by = "day", length.out = max_dia * 7 + 30)
    working_dates <- dates_seq[lubridate::wday(dates_seq, week_start = 1) %in% allowed_wdays]
    date_lookup <- tibble::tibble(Dia = 1:max_dia, Fecha = working_dates[1:max_dia])
    sf <- sf %>% dplyr::left_join(date_lookup, by = "Dia")
  }
  return(sf)
}
