#' Programar puntos de terrenos
#'
#' @description
#' Asigna días y cuadrillas a un conjunto de geometrías (puntos) basándose en un índice numérico
#' y la disponibilidad de cuadrillas. Opcionalmente calcula las fechas calendario estimadas.
#'
#' @param sf Objeto de clase `sf`.
#' @param dias Columna (tidy-select) numérica que actúa como índice o secuencia de trabajo.
#' @param cuadrillas Número entero positivo. Cantidad de cuadrillas disponibles.
#' @param fecha_i Fecha de inicio (objeto `Date` o cadena "YYYY-MM-DD"). Opcional.
#' @param dias_sem Dias de trabajo a la semana (1-7). Por defecto 5.
#'
#' @return El objeto `sf` original con columnas nuevas:
#'   \item{Dia}{Número de día de trabajo relativo (1, 2, ...).}
#'   \item{Cuadrilla}{Número de cuadrilla asignada.}
#'   \item{Fecha}{(Opcional) Fecha calendario estimada si se provee `fecha_i`.}
#'
#' @export
#' @examples
#' \dontrun{
#'   # puntos_terr <- sf::read_sf("puntos.shp")
#'   # Programacion_terr <- st_schedule_field(
#'   #   puntos_terr, 
#'   #   dias = ID_ORDEN, 
#'   #   cuadrillas = 2, 
#'   #   fecha_i = "2023-01-01", 
#'   #   dias_sem = 5
#'   # )
#' }
st_schedule_field <- function(sf, dias, cuadrillas, fecha_i = NULL, dias_sem = 5) {
  checkmate::assert_class(sf, "sf")
  checkmate::assert_count(cuadrillas, positive = TRUE)
  
  col_df <- tryCatch(
    sf %>% sf::st_drop_geometry() %>% dplyr::select({{ dias }}),
    error = function(e) stop("No se pudo seleccionar la columna 'dias'. Verifique el nombre.")
  )
  
  if (ncol(col_df) != 1) stop("Debe indicar exactamente una columna para el argumento 'dias'.")
  
  col_name <- names(col_df)[1]
  if (!is.numeric(col_df[[1]])) stop(paste0("El campo '", col_name, "' debe ser numérico."))
  
  sf <- sf %>% 
    dplyr::mutate(
      Dia = ceiling(.data[[col_name]] / cuadrillas),
      Cuadrilla = (.data[[col_name]] - 1) %% cuadrillas + 1
    )
  
  if (!is.null(fecha_i)) {
    fecha_i <- tryCatch(as.Date(fecha_i), error = function(e) stop("fecha_i debe ser una fecha válida."))
    checkmate::assert_date(fecha_i, len = 1, any.missing = FALSE)
    checkmate::assert_int(dias_sem, lower = 1, upper = 7)
    
    start_wday <- lubridate::wday(fecha_i, week_start = 1)
    allowed_wdays <- (start_wday - 1 + 0:(dias_sem - 1)) %% 7 + 1
    
    max_dia <- max(sf$Dia, na.rm = TRUE)
    if (is.infinite(max_dia)) max_dia <- 0
    
    dates_seq <- seq(from = fecha_i, by = "day", length.out = max_dia * 7 + 100)
    working_dates <- dates_seq[lubridate::wday(dates_seq, week_start = 1) %in% allowed_wdays]
    
    date_lookup <- tibble::tibble(Dia = seq_len(max_dia), Fecha = working_dates[seq_len(max_dia)])
    sf <- sf %>% dplyr::left_join(date_lookup, by = "Dia") %>% dplyr::relocate(geometry, .after = dplyr::last_col())
  }
  return(sf)
}
