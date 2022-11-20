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
#' \dontrun{
#'   iceclim_south_leadsfiles()
#'   iceclim_north_leadsfiles()
#' }
iceclim_south_leadsfiles <- function(all = FALSE) {
  pattern <- c("store.pangaea.de", "ReiserF-etal_2020/Antarctic_Relleads")
  if (!all) {
    pattern <- c(pattern, ".*nc$")
  }
  .find_files_generic(pattern)
}
#' @export
#' @name leads
iceclim_north_leadsfiles <- function(all = FALSE) {
  pattern <- c("store.pangaea.de", "ReiserF-etal_2020/Arctic_Relleads")
  if (!all) {
    pattern <- c(pattern, ".*nc$")
  }
  .find_files_generic(pattern)
}
