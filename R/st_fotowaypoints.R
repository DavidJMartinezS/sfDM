
#' Title
#'
#' @param file
#'
#' @returns
#'
#' @export
#' @examples
photo2html <- function(file) {
  file_path <- paste0("file:///", file)
  tags$table(
    border = 1,
    tags$tr(
      tags$td(
        tags$img(
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


#' Title
#'
#' @param x
#'
#' @returns
#'
#' @export
#' @examples
readexif_try <- function(x) {
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
            .[],
            Lat = map_chr(GPSPosition, stringr::word, 1),
            Lon = map_chr(GPSPosition, stringr::word, 2)
          ) %>%
            tidyr::drop_na(Lat) %>%
            dplyr::mutate_at("SourceFile", tools::file_path_as_absolute) %>%
            dplyr::mutate_at(dplyr::vars(Lat, Lon), as.numeric) %>%
            dplyr::rename(Name = FileName) %>%
            dplyr::mutate(Description = purrr::map_chr(SourceFile, photo2html)) %>%
            sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = T) %>%
            sf::st_transform(32719) %>%
            dplyr::select(Name, Description)
        } else {
          NULL
        }
      }
  }
}