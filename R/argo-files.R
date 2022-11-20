#' Argo files.
#'
#' ARGO, by default we have 'profiles', alternatively 'meta' for type
#'
#' (No traj, tech, or Mprof at this time of writing 2022-11-21)
#' @param type file type, i.e. prof, meta, traj, tech
#' @param dac data acqusition centre e.g.* "aoml", "bodc", "coriolis", "csio", "csiro", "incois", all returned if not specified
#' @export
argo_files <- function(type = c("prof", "meta", "traj", "tech", "Mprof"), dac = NULL) {
  type <- match.arg(type)

  pattern <- c("usgodae", "nc$", "/argo/")

  if (!is.null(dac)) {
    pattern <- c(pattern, sprintf("/%s/", dac))
  }
  typefilter <- switch(type,
                       prof = "profiles",
                       traj = "traj",
                       meta = "_meta",
                       tech = "tech",
                       Mprof = "Mprof")


  pattern <- c(pattern, typefilter)
  files <- .find_files_generic(pattern)
files
}

