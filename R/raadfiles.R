get_raw_raad_filenames <- function() {
  out <- getOption("raadfiles.filename.database" )
  base <- basename(out$root)
  out$root <- file.path(get_raad_datadir(), base)
  out
}
get_raad_datadir <- function() {
  getOption("raadfiles.default.data.directory")
}

