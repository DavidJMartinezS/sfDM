#' leer shapefile desde zip
#'
#' @param zip.file
#' @param pattern
#'
#' @returns
#'
#' @export
#' @examples
read_sf_zip <- function(zip.file, pattern) {
  tmp <- tempfile()
  contenido_zip <- utils::unzip(zipfile = zip.file, list = TRUE) %>% 
    dplyr::mutate(basename = basename(Name)) %>% 
    dplyr::filter(basename %>% stringi::stri_detect_regex(pattern, case_insensitive = T)) %>% 
    dplyr::arrange(desc(Date)) %>% 
    dplyr::distinct(basename, .keep_all = T)
  file.zip <- contenido_zip$Name %>% subset(tools::file_ext(basename(.)) %in% c("dbf", "prj", "shp", "shx"))
  if (length(file.zip) > 0 & any(file.zip %>% stringi::stri_detect_regex(".shp$"))) {
    utils::unzip(zipfile = zip.file, files = file.zip, exdir = tmp)
    ruta_shp <- file.path(tmp, file.zip) %>% normalizePath() %>% subset(tools::file_ext(basename(.)) == "shp") 
    shp <- ruta_shp %>% purrr::map(sf::read_sf) %>% purrr::set_names(tools::file_path_sans_ext(basename(ruta_shp)))
    return(shp)
  } else {
    warning("No se halló ningún shapefile. Intente con otro pattern")
  }
}

#' Copiar shapefile  
#'
#' @param zip.file
#' @param pattern
#' @param dirsave
#'
#' @returns
#'
#' @export
#' @examples
copy_sf_zip <- function(zip.file, pattern, dirsave) {
  tmp <- tempfile()
  contenido_zip <- utils::unzip(zipfile = zip.file, list = TRUE) %>% 
    dplyr::mutate(basename = basename(Name)) %>% 
    dplyr::filter(basename %>% stringi::stri_detect_regex(pattern, case_insensitive = T)) %>% 
    dplyr::arrange(desc(Date)) %>% 
    dplyr::distinct(basename, .keep_all = T)
  file.zip <- contenido_zip$Name %>% subset(tools::file_ext(basename(.)) %in% c("dbf", "prj", "shp", "shx"))
  if (length(file.zip) > 0 & any(file.zip %>% stringi::stri_detect_regex(".shp$"))) {
    utils::unzip(zipfile = zip.file, files = file.zip, exdir = tmp)
    ruta <- file.path(tmp, file.zip) %>% normalizePath()
    file.copy(from = ruta, to = dirsave)
  } else {
    warning("No se halló ningún shapefile. Intente con otro pattern")
  }
}
