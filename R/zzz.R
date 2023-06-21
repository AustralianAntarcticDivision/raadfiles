raadfiles.env <- new.env(FALSE, parent=globalenv())

env0 <- new.env(FALSE, parent=globalenv())
assign("message2", NULL, envir = env0)
assign("message1", NULL, envir = env0)
.onAttach <- function(libname, pkgname) {
  mess1 <- get("message1", envir = env0)
  if (!is.null(mess1)) {
    packageStartupMessage(mess1)
  }
  mess2 <- get("message2", envir = env0)
  if (!is.null(mess2)) {
    packageStartupMessage(mess2)
  }


}
#' @importFrom tibble tibble
.onLoad <- function(libname, pkgname) {

  run_on_load <- getOption("raadfiles.file.cache.disable")
  file_refresh <- getOption("raadfiles.file.refresh.threshold")
  if (is.null(file_refresh)) {
    file_refresh <- 0.01
    options(raadfiles.file.refresh.threshold = file_refresh)  ## 0 for never, 1 for every time
  }
  if (isTRUE(run_on_load)) {
    assign("message1", "raadfiles in admin-mode, no file list loaded", envir = env0)
    #packageStartupMessage("raadfiles in admin-mode, no file list loaded")
    return(invisible())
  }
  ## this logic says "data roots list is >=1 and I've set the file list/s found to the in-mem cache
  raad_path_was_set <- set_raad_data_roots(use_known_candidates = TRUE,
                                           replace_existing = FALSE, verbose = FALSE)
  if (raad_path_was_set) {
    set_raad_filenames(clobber = FALSE)  ## clobber at start-up, why not
  } else {
    assign("message2", "No existing file cache found, see help('raadfiles-admin') for setting up", envir = env0)
    #packageStartupMessage("No existing file cache found, see help('raadfiles-admin') for setting up")
  }
  tm <- 24 * 3600

  altimetry_currents_polar_files <<- memoise::memoize(altimetry_currents_polar_files,  ~memoise::timeout(tm))
  altimetry_daily_files <<- memoise::memoize(altimetry_daily_files,  ~memoise::timeout(tm))
  amps_d1files <<- memoise::memoize(amps_d1files,  ~memoise::timeout(tm))
  amps_d2files <<- memoise::memoize(amps_d2files,  ~memoise::timeout(tm))
  amps_files <<- memoise::memoize(amps_files,  ~memoise::timeout(tm))
  amps_model_files <<- memoise::memoize(amps_model_files,  ~memoise::timeout(tm))
  amsr_daily_files <<- memoise::memoize(amsr_daily_files,  ~memoise::timeout(tm))
  amsr2_daily_files <<- memoise::memoize(amsr2_daily_files,  ~memoise::timeout(tm))
  amsr2_3k_daily_files <<- memoise::memoize(amsr2_3k_daily_files,  ~memoise::timeout(tm))
  amsre_daily_files <<- memoise::memoize(amsre_daily_files,  ~memoise::timeout(tm))

  argo_files <<- memoise::memoize(argo_files,  ~memoise::timeout(tm))
  cafe_monthly_files <<-   memoise::memoize(cafe_monthly_files,  ~memoise::timeout(tm))
  ccmp_6hourly_files <<- memoise::memoize(ccmp_6hourly_files, ~memoise::timeout(tm))

  cersat_daily_files <<- memoise::memoize(cersat_daily_files,  ~memoise::timeout(tm))
  cryosat2_files <<- memoise::memoize(cryosat2_files,  ~memoise::timeout(tm))
  etopo1_files <<- memoise::memoize(etopo1_files,  ~memoise::timeout(tm))
  etopo2_files <<- memoise::memoize(etopo2_files,  ~memoise::timeout(tm))
  fasticefiles <<- memoise::memoize(fasticefiles,  ~memoise::timeout(tm))

  fsle_files <<- memoise::memoize(fsle_files,  ~memoise::timeout(tm))
  gebco08_files <<- memoise::memoize(gebco08_files,  ~memoise::timeout(tm))
  gebco14_files <<- memoise::memoize(gebco14_files,  ~memoise::timeout(tm))
  gebco19_files <<- memoise::memoize(gebco19_files,  ~memoise::timeout(tm))
  gebco21_files <<- memoise::memoize(gebco21_files,  ~memoise::timeout(tm))
  geoid_files <<-   memoise::memoize(geoid_files,  ~memoise::timeout(tm))
  george_v_terre_adelie_1000m_files <<- memoise::memoize(george_v_terre_adelie_1000m_files,  ~memoise::timeout(tm))
  george_v_terre_adelie_100m_files <<- memoise::memoize(george_v_terre_adelie_100m_files,  ~memoise::timeout(tm))
  george_v_terre_adelie_250m_files <<- memoise::memoize(george_v_terre_adelie_250m_files,  ~memoise::timeout(tm))
  george_v_terre_adelie_500m_files <<- memoise::memoize(george_v_terre_adelie_500m_files,  ~memoise::timeout(tm))
  #get_raad_data_roots <<- memoise::memoize(get_raad_data_roots,  ~memoise::timeout(tm))
  #get_raad_filenames <<- memoise::memoize(get_raad_filenames,  ~memoise::timeout(tm))
  ghrsst_daily_files <<- memoise::memoize(ghrsst_daily_files,  ~memoise::timeout(tm))
  ibcso_background_files <<- memoise::memoize(ibcso_background_files,  ~memoise::timeout(tm))
  ibcso_bed_files <<- memoise::memoize(ibcso_bed_files,  ~memoise::timeout(tm))
  ibcso_digital_chart_files <<- memoise::memoize(ibcso_digital_chart_files,  ~memoise::timeout(tm))
  ibcso_files <<- memoise::memoize(ibcso_files,  ~memoise::timeout(tm))
  ibcso_sid_files <<- memoise::memoize(ibcso_sid_files,  ~memoise::timeout(tm))
  ibcso_rid_files <<- memoise::memoize(ibcso_rid_files,  ~memoise::timeout(tm))
  ibcso_tid_files <<- memoise::memoize(ibcso_tid_files,  ~memoise::timeout(tm))


  iceclim_south_leadsfiles <<- memoise::memoize(iceclim_south_leadsfiles,  ~memoise::timeout(tm))
  iceclim_north_leadsfiles <<- memoise::memoize(iceclim_north_leadsfiles,  ~memoise::timeout(tm))
  kerguelen_files <<- memoise::memoize(kerguelen_files,  ~memoise::timeout(tm))
  lakesuperior_files <<- memoise::memoize(lakesuperior_files,  ~memoise::timeout(tm))
  macquarie100m_57S_files <<- memoise::memoize(macquarie100m_57S_files,  ~memoise::timeout(tm))
  macquarie100m_58S_files <<- memoise::memoize(macquarie100m_58S_files,  ~memoise::timeout(tm))
  ncep2_uwnd_6hr_files <<- memoise::memoize(ncep2_uwnd_6hr_files,  ~memoise::timeout(tm))
  ncep2_vwnd_6hr_files <<- memoise::memoize(ncep2_vwnd_6hr_files,  ~memoise::timeout(tm))
  nsidc_daily_files <<- memoise::memoize(nsidc_daily_files,  ~memoise::timeout(tm))
  nsidc_daily_files_v2 <<- memoise::memoize(nsidc_daily_files_v2,  ~memoise::timeout(tm))
  nsidc_monthly_files_v2 <<- memoise::memoize(nsidc_monthly_files_v2,  ~memoise::timeout(tm))
  nsidc_monthly_files <<- memoise::memoize(nsidc_monthly_files,  ~memoise::timeout(tm))
  nsidc_north_daily_files <<- memoise::memoize(nsidc_north_daily_files,  ~memoise::timeout(tm))
  nsidc_north_monthly_files <<- memoise::memoize(nsidc_north_monthly_files,  ~memoise::timeout(tm))
  nsidc_south_daily_files <<- memoise::memoize(nsidc_south_daily_files,  ~memoise::timeout(tm))
  nsidc_south_monthly_files <<- memoise::memoize(nsidc_south_monthly_files,  ~memoise::timeout(tm))
  oisst_daily_files <<- memoise::memoize(oisst_daily_files,  ~memoise::timeout(tm))
  oisst_monthly_files <<- memoise::memoize(oisst_monthly_files,  ~memoise::timeout(tm))

  par_files <<- memoise::memoize(par_files,  ~memoise::timeout(tm))

  polarview_files <<- memoise::memoize(polarview_files,  ~memoise::timeout(tm))

  #raad_filedb_path <<- memoise::memoize(raad_filedb_path,  ~memoise::timeout(tm))
  ramp_files <<- memoise::memoize(ramp_files,  ~memoise::timeout(tm))
  rema_100m_files <<- memoise::memoize(rema_100m_files,  ~memoise::timeout(tm))
  rema_1km_files <<- memoise::memoize(rema_1km_files,  ~memoise::timeout(tm))
  rema_200m_files <<- memoise::memoize(rema_200m_files,  ~memoise::timeout(tm))
  rema_8m_files <<- memoise::memoize(rema_8m_files,  ~memoise::timeout(tm))
  rema_tile_files <<- memoise::memoize(rema_tile_files,  ~memoise::timeout(tm))
  run_build_raad_cache <<- memoise::memoize(run_build_raad_cache,  ~memoise::timeout(tm))
  seapodym_weekly_files <<- memoise::memoize(seapodym_weekly_files,  ~memoise::timeout(tm))
 # set_raad_data_roots <<- memoise::memoize(set_raad_data_roots,  ~memoise::timeout(tm))
 # set_raad_filenames <<- memoise::memoize(set_raad_filenames,  ~memoise::timeout(tm))
  smap_8day_files <<- memoise::memoize(smap_8day_files,  ~memoise::timeout(tm))
  smith_sandwell_files <<- memoise::memoize(smith_sandwell_files,  ~memoise::timeout(tm))
  smith_sandwell_lon180_files <<- memoise::memoize(smith_sandwell_lon180_files,  ~memoise::timeout(tm))
  smith_sandwell_unpolished_files <<- memoise::memoize(smith_sandwell_unpolished_files,  ~memoise::timeout(tm))
  smith_sandwell_unpolished_lon180_files <<- memoise::memoize(smith_sandwell_unpolished_lon180_files,  ~memoise::timeout(tm))
  srtm_files <<- memoise::memoize(srtm_files,  ~memoise::timeout(tm))
  thelist_files <<- memoise::memoize(thelist_files,  ~memoise::timeout(tm))
  woa09_daily_files <<- memoise::memoize(woa09_daily_files,  ~memoise::timeout(tm))
  woa09_files <<- memoise::memoize(woa09_files,  ~memoise::timeout(tm))
  woa13_files <<- memoise::memoize(woa13_files,  ~memoise::timeout(tm))
  invisible()
}




