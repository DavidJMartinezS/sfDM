#' @noRd
photo2html <- function(file) {
  file_path <- paste0("file:///", file)
  htmltools::tags$table(
    border = 1,
    htmltools::tags$tr(
      htmltools::tags$td(
        htmltools::tags$img(
          width = "800",
          src = file_path
        )
      )
    )
  ) %>%
    as.character() %>%
    stringi::stri_replace_all_regex("\\s+", " ") %>% 
    stringi::stri_trim_both()
}


#' Leer metadatos EXIF de fotos y convertir a objeto sf
#'
#' Lee los metadatos GPS de archivos de imagen y genera un objeto sf de puntos.
#' Crea una descripción HTML con la imagen incrustada, útil para popups en mapas interactivos.
#'
#' @param x Vector de caracteres con las rutas de los archivos de imagen.
#' @param ... Argumentos adicionales pasados a \code{exifr::read_exif}.
#' @param crs_out Código EPSG o objeto CRS para la transformación final. Por defecto 4326 (WGS 84).
#'
#' @return Un objeto \code{sf} con columnas 'Name' y 'Description'. Retorna \code{NULL} si falla la lectura o no hay coordenadas GPS.
#' @export
#' @examples
#' \dontrun{
#'   fotos <- list.files("ruta/a/fotos", pattern = ".jpg$", full.names = TRUE)
#'   sf_fotos <- readexif_try(fotos)
#' }
readexif_try <- function(x, ..., crs_out = 4326) {
  if (!is.character(x)) stop("El argumento 'x' debe ser un vector de caracteres (rutas de archivo).")
  
  df <- tryCatch(
    exifr::read_exif(x, ...),
    error = function(e) return(NULL)
  )
  if (is.null(df)) {
    return(NULL)
  } else {
    df %>%
      dplyr::select(dplyr::starts_with("GPS"), -dplyr::ends_with("Ref"), FileName, SourceFile) %>%
      {
        if ("GPSPosition" %in% names(.)) {
          dplyr::mutate(
            .,
            Lat = stringr::word(GPSPosition, 1),
            Lon = stringr::word(GPSPosition, 2)
          ) %>%
            tidyr::drop_na(Lat) %>%
            dplyr::mutate_at("SourceFile", tools::file_path_as_absolute) %>%
            dplyr::mutate_at(dplyr::vars(Lat, Lon), as.numeric) %>%
            dplyr::rename(Name = FileName) %>%
            dplyr::mutate(Description = purrr::map_chr(SourceFile, photo2html)) %>%
            sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = T) %>%
            {if(!is.null(crs_out)) sf::st_transform(., crs_out) else .} %>%
            dplyr::select(Name, Description)
        } else {
          NULL
        }
      }
  }
}
