#' Instalar 7-Zip
#'
#' @description
#' Descarga e instala automáticamente la versión de 7-Zip correspondiente a la arquitectura
#' del sistema (32 o 64 bits). Esta herramienta es necesaria para descomprimir archivos `.rar`
#' con las funciones `read_sf_ziprar` y `copy_sf_ziprar`.
#'
#' @return Invisible. El resultado de la función `installr::install.URL`.
#' @export
#' @examples
#' \dontrun{
#'   install.7z()
#' }
install.7z <- function() {
  url <- switch(
    as.character(.Machine$sizeof.pointer), 
    "8" = "https://www.7-zip.org/a/7z2600-x64.exe", 
    "4" = "https://www.7-zip.org/a/7z2600.exe"
  )
  installr::install.URL(url)
}


#' Leer o copiar shapefiles desde un archivo comprimido (.zip o .rar)
#'
#' @description
#' `read_sf_ziprar` lee uno o más shapefiles desde un archivo `.zip` o `.rar`
#' que coincidan con un patrón de búsqueda, devolviendo un objeto `sf` o una lista de ellos.
#'
#' `copy_sf_ziprar` copia los archivos componentes de un shapefile (p. ej. .shp, .shx, .dbf)
#' desde un archivo `.zip` o `.rar` a un directorio específico.
#'
#' Ambas funciones requieren que 7-Zip esté instalado y accesible en el PATH del
#' sistema para poder manejar archivos `.rar`.
#'
#' @param zip.file Ruta al archivo comprimido (`.zip` o `.rar`). Debe ser una única ruta.
#' @param pattern Patrón de expresión regular (regex) para buscar el nombre del shapefile
#'   dentro del archivo comprimido. La búsqueda no distingue mayúsculas de minúsculas.
#' @param dirsave Directorio de destino donde se copiarán los archivos (solo para `copy_sf_ziprar`).
#'
#' @returns
#' `read_sf_ziprar`: Si se encuentra un solo shapefile, devuelve un objeto `sf`.
#'   Si se encuentran múltiples shapefiles, devuelve una lista nombrada de objetos `sf`.
#'   Si no se encuentra ningún shapefile que coincida, devuelve `NULL` con una advertencia.
#'
#' `copy_sf_ziprar`: Retorna `NULL` de forma invisible. Emite una advertencia si no se encuentra
#'   el shapefile.
#'
#' @rdname st_ziprar
#'
#' @export
#' @examples
#' # Se necesita un archivo .zip de ejemplo para ejecutar.
#' # zip_path <- "ruta/a/su/archivo.zip"
#' # if (file.exists(zip_path)) {
#' #   # Leer un shapefile cuyo nombre contenga 'comunas'
#' #   comunas_sf <- read_sf_ziprar(zip_path, pattern = "comunas")
#' # }
#' #
#' # dest_dir <- tempdir()
#' # if (file.exists(zip_path) && dir.exists(dest_dir)) {
#' #   # Copiar los archivos del shapefile 'comunas' a un directorio
#' #   copy_sf_ziprar(zip_path, pattern = "comunas", dirsave = dest_dir)
#' # }
read_sf_ziprar <- function(zip.file, pattern) {
  checkmate::assert_file_exists(zip.file, access = "r", extension = c("zip", "rar"))
  checkmate::assert_string(pattern)

  ext <- tools::file_ext(zip.file)
  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  if (ext == "zip") {
    # Listar contenido y filtrar por patrón para encontrar los archivos componentes del shp
    contenido_zip <- utils::unzip(zipfile = zip.file, list = TRUE) %>%
      dplyr::mutate(basename = basename(Name)) %>%
      dplyr::filter(stringi::stri_detect_regex(basename, pattern, case_insensitive = TRUE)) %>%
      dplyr::arrange(desc(Date)) %>%
      dplyr::distinct(basename, .keep_all = TRUE)

    # Seleccionar solo los archivos esenciales del shapefile
    file.zip <- contenido_zip$Name %>%
      subset(tools::file_ext(basename(.)) %in% c("dbf", "prj", "shp", "shx", "cpg"))

    if (length(file.zip) > 0 && any(stringi::stri_detect_regex(file.zip, "\\.shp$"))) {
      utils::unzip(zipfile = zip.file, files = file.zip, exdir = td)
    }
  } else { 
    z7_path <- Sys.which("7z")
    if (z7_path == "") {
      z7_path_win <- "C:/Program Files/7-Zip/7z.exe"
      if (file.exists(z7_path_win)) {
        z7_path <- z7_path_win
      } else {
        stop("7-Zip no se encontró en el PATH del sistema ni en 'C:/Program Files/7-Zip/'. ",
             "Por favor, instale 7-Zip y agréguelo al PATH para leer archivos .rar.")
      }
    }
    # Extraer todo el contenido del rar
    cmd <- paste(shQuote(z7_path), "x", shQuote(zip.file), "-aot", paste0("-o", shQuote(td)))
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  # Buscar los archivos .shp extraídos que coincidan con el patrón
  ruta_shp <- list.files(td, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  ruta_shp <- ruta_shp[stringi::stri_detect_regex(basename(ruta_shp), pattern, case_insensitive = TRUE)]

  if (length(ruta_shp) == 0) {
    warning("No se halló ningún shapefile que coincida con el patrón. Intente con otro.")
    return(NULL)
  }

  # Leer los shapefiles
  shp <- purrr::map(ruta_shp, sf::read_sf) %>%
    purrr::set_names(tools::file_path_sans_ext(basename(ruta_shp)))

  if (length(shp) == 1) {
    return(shp[[1]])
  } else {
    return(shp)
  }
}

#' @rdname st_ziprar
#' @export
copy_sf_ziprar <- function(zip.file, pattern, dirsave) {
  # Verificaciones
  checkmate::assert_file_exists(zip.file, access = "r", extension = c("zip", "rar"))
  checkmate::assert_string(pattern)
  checkmate::assert_directory_exists(dirsave, access = "w")

  td <- tempfile()
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE))

  ext <- tools::file_ext(zip.file)

  if (ext == "zip") {
    all_files <- utils::unzip(zipfile = zip.file, list = TRUE)

    shp_file_in_zip <- all_files$Name[stringi::stri_detect_regex(all_files$Name, pattern, case_insensitive = TRUE) &
                                        stringi::stri_detect_regex(all_files$Name, "\\.shp$")]

    if (length(shp_file_in_zip) == 0) {
      warning("No se halló ningún shapefile (.shp) que coincida con el patrón.")
      return(invisible(NULL))
    }

    if (length(shp_file_in_zip) > 1) {
        warning(paste("Múltiples shapefiles coinciden con el patrón. Usando el primero:", shp_file_in_zip[1]))
        shp_file_in_zip <- shp_file_in_zip[1]
    }

    shp_basename <- tools::file_path_sans_ext(basename(shp_file_in_zip))

    files_to_extract <- all_files$Name[tools::file_path_sans_ext(basename(all_files$Name)) == shp_basename]

    if (length(files_to_extract) > 0) {
      utils::unzip(zipfile = zip.file, files = files_to_extract, exdir = td, junkpaths = TRUE)
      extracted_files <- list.files(td, full.names = TRUE)
      file.copy(from = extracted_files, to = dirsave)
    } else {
      warning("No se encontraron archivos componentes para el shapefile.")
      return(invisible(NULL))
    }
  } else {
    z7_path <- Sys.which("7z")
    if (z7_path == "") {
      z7_path_win <- "C:/Program Files/7-Zip/7z.exe"
      if (file.exists(z7_path_win)) {
        z7_path <- z7_path_win
      } else {
        stop("7-Zip no se encontró. Instale 7-Zip y agréguelo al PATH para usar archivos .rar.")
      }
    }

    cmd <- paste(shQuote(z7_path), "x", shQuote(zip.file), "-aot", paste0("-o", shQuote(td)))
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

    all_extracted_files <- list.files(td, recursive = TRUE, full.names = TRUE)

    shp_file_path <- all_extracted_files[stringi::stri_detect_regex(basename(all_extracted_files), pattern, case_insensitive = TRUE) &
                                           stringi::stri_detect_regex(basename(all_extracted_files), "\\.shp$")]

    if (length(shp_file_path) == 0) {
      warning("No se halló ningún shapefile (.shp) que coincida con el patrón en el archivo .rar.")
      return(invisible(NULL))
    }

    if (length(shp_file_path) > 1) {
        warning(paste("Múltiples shapefiles coinciden con el patrón. Usando el primero:", basename(shp_file_path[1])))
        shp_file_path <- shp_file_path[1]
    }

    shp_basename <- tools::file_path_sans_ext(basename(shp_file_path))

    component_files <- all_extracted_files[tools::file_path_sans_ext(basename(all_extracted_files)) == shp_basename]

    if (length(component_files) > 0) {
      file.copy(from = component_files, to = dirsave)
    } else {
      warning("No se encontraron archivos componentes para el shapefile.")
      return(invisible(NULL))
    }
  }
  return(invisible(NULL))
}
