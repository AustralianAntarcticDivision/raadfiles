#' Files containing relative lead frequencies for the Arctic and Antarctic
#'
#' Average Lead-Frequency for the polar oceans for winter months November-April 2002/03-2018/19 based on daily lead
#' composites as derived from MOD/MYD-29 IST 5 min granules.
#'
#' @param all return all files, or just the core grid files (*.nc)?
#' @references F. Reiser, S. Willmes, G. Heinemann (2020): A new algorithm for daily sea ice lead identification in the Arctic and
#'  Antarctic winter from thermal-infrared satellite imagery.
#' @export
#' @name leads
#' @examples
#' iceclim_south_leadsfiles()
#' iceclim_north_leadsfiles()
iceclim_south_leadsfiles <- function(all = FALSE) {
  files <- dplyr::filter(get_raad_filenames(all = TRUE), stringr::str_detect(.data$file, "store.pangaea.de"))

  files <- dplyr::filter(files, stringr::str_detect(.data$file, "ReiserF-etal_2020/Antarctic_Relleads"))
  if (!all) {
    files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*nc$"))
  }
  tibble::tibble(fullname = file.path(files$root, files$file))
}
#' @export
#' @name leads
iceclim_north_leadsfiles <- function(all = FALSE) {
  files <- dplyr::filter(get_raad_filenames(all = TRUE), stringr::str_detect(.data$file, "store.pangaea.de"))

  files <- dplyr::filter(files, stringr::str_detect(.data$file, "ReiserF-etal_2020/Arctic_Relleads"))
  if (!all) {
    files <- dplyr::filter(files, stringr::str_detect(.data$file, ".*nc$"))
  }
  tibble::tibble(fullname = file.path(files$root, files$file))
}
