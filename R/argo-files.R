#' Argo files.
#'
#' ARGO
#' @param type file type, i.e. prof, meta, traj, tech
#' @param dac data acqusition centre e.g.* "aoml", "bodc", "coriolis", "csio", "csiro", "incois", all returned if not specified
#' @export
argo_files <- function(type = c("prof", "meta", "traj", "tech", "Mprof"), dac = NULL) {
  type <- match.arg(type)
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "usgodae"))
  files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "nc$"))
  files <- dplyr::filter(files, stringr::str_detect(.data$file, "/argo/"))


  if (!is.null(dac)) {
    files <- dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, sprintf("/%s/", dac)))
  }
  typefilter <- switch(type,
                       prof = "profiles")

  files <- dplyr::filter(files, stringr::str_detect(.data$file, typefilter))


  if (nrow(files) < 1)
    stop("no files found")
  files <-   dplyr::transmute(files, fullname = file.path(.data$root, .data$file), .data$root)

  files
}

