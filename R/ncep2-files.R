##.find_files_generic("ncep.reanalysis2.dailyavgs")


#' NCEP2 wind files
#'
#' NCEP2 six-hourly reanalysis2 gaussian grid
#'
#' @return tibble data frame of file names
#' @importFrom dplyr arrange distinct filter mutate
#' @importFrom rlang .data
#' @name ncep2_files
#' @export
#' @importFrom stringr str_extract str_replace
#' @examples
#' \dontrun{
#'   ncep2_uwnd_6hr_files()
#'   ncep2_vwnd_6hr_files()
#' }
ncep2_uwnd_6hr_files <- function() {
  pattern <- c("ncep.reanalysis2/gaussian_grid", "^.*uwnd.*gauss.*\\.nc$")
  files <- .find_files_generic(pattern)

  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::transmute(files, date = ISOdate(as.integer(stringr::str_extract(basename(fullname), "[0-9]{4}")), 1, 1, 0, 0, 0, tz = "UTC"),
                         fullname = .data$fullname, root = .data$root)
  dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date) %>%
    set_dt_utc()

}
#' @name ncep2_files
#' @export
ncep2_vwnd_6hr_files <- function() {
  pattern <- c("ncep.reanalysis2/gaussian_grid", "^.*vwnd.*gauss.*\\.nc$")
  files <- .find_files_generic(pattern)

  if (nrow(files) < 1)
    stop("no files found")
  files <- dplyr::transmute(files, date = ISOdate(as.integer(stringr::str_extract(basename(fullname), "[0-9]{4}")), 1, 1),
                         fullname = .data$fullname, root = .data$root)
  dplyr::arrange(dplyr::distinct(files, .data$date, .keep_all = TRUE), .data$date) %>%
    set_dt_utc()


}

#dplyr::filter(get_raad_filenames(), stringr::str_detect(.data$file, "ncep.reanalysis2")) %>% mutate(file = basename(dirname(file))) %>% distinct(file)
# A tibble: 5 x 1
#file
#<chr>
#1              gaussian_grid
#2                   pressure
#3 ncep.reanalysis2.dailyavgs
#4                    surface
#5   ncep.reanalysis2.derived



