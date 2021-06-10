# raadfiles dev

* Move to REMA 1.1 (not 8m yet). 

* New functions `amsre_daily_files()`, `amsr2_daily_files()`, `amsr2_3k_daily_files()` to round out
 AMSRE and AMSR2 support. The original function `amsr_daily_files()` provides a mix of AMSRE and AMSR2
 6.25km files to give a full temporal coverage (2002- current), but are different products. 

* Add leads files from Reiser et al, new functions `iceclim_north_leadsfiles()` `iceclim_south_leadsfiles()`. 

* Move to using 'circum_fast_ice' files. 

* Now `fasticefiles()` moved here from raadtools. 

* New function `polarview_files()` for Polarview images in JPEG or the tarball GeoTIFFs. 

* Updated path to OISST files https://github.com/AustralianAntarcticDivision/raadfiles/issues/21. 

* Add CERSAT 12.5 km ice files `cersat_daily_files()`. 

* New function `geoid_files()` for Earth Gravitation Model. 

* New function `sose_monthly_files()` for Southern Ocean State Estimate (SOSE). 

* Add MODIS "cafe" files in `cafe_monthly_files()`. 


# raadfiles 0.1.3

* Use new paths at hq. 


# raadfiles 0.1.2.9015

* Use memoization on all files functions. 

* Fix ncep2 files to use base time at midnight UTC (so we can speed up raadtools getting the time for each band). 

* New `altimetry_currents_polar_files()` for local derived product. 

* New `all` argument to `get_raad_filenames()`, for internal use. 

* Fix export of `gebco_14_files()`. 

# raadfiles 0.1.2

* New SEAPODYM function `seapodym_weekly_files()`, from the private collection onr
ly. 

* New GEBCO 2019 function `gebco19_files()`. 

* New function `srtm_files()`.  

* New function for SMAP salinity files. 

* New function for `fsle files()`. 

# 0.1.0

* 'raadfiles' now uses a new approach to file cache management, so easier to use from scratch, relevant topics are in `raadfiles-admin`. New
 functions include getters and setters for the file cache, as well as options to control whether the file cache is loaded, and how often 
 it is refreshed. 
 
* Internal function `run_this_function_to_build_raad_cache` is now deprecated, please use the newly exported `run_build_raad_cache`. 

* New `rema_8m_files`, `rema_100m_files`, `rema_200m_files`, and `rema_1km_files` for the Reference Elevation Model of Antarctica (REMA) topography. 

* New `amps_d2files` and `amps_d1files` (to replace raadtools version) functions for AMPs files. 

* Added RAMP topography in `ramp_files`. 

* each 'topography' related files function now uses an internal generic pattern form, and each separate data variant has its own function. In 
 contrast to other types, these files now have an argument `all` for exploratory use, to return the (non-exclusive) set of all files available with the 
 data set

* new functions for Macquarie Island region topography `macquarie100m_57S_files` and `macquarie100m_58S_files`, these are two UTM forms for 
 the alternative zones 57 or 58. 
 
* new functions for Smith and Sandwell topography `smith_sandwell_files`, `smith_sandwell_lon180_files` for the Pacific-view and Atlantic-view
  variants and their analogues `smith_sandwell_unpolished_files`, `smith_sandwell_lon180_unpolished_files`. All Smith and Sandwell data now
  defaults to version 18.1, older versions may be found with `smith_sandwell_files(all = TRUE)`. 
 
* new functions for AAD George V Terre Adelie topography at various resolutions `george_v_terre_adelie_1000m_files`, 
  `george_v_terre_adelie_500m_files`, `george_v_terre_adelie_250m_files`, and `george_v_terre_adelie_100m_files` 

* new functions for IBCSO data and images `ibcso_files`, `ibcso_background_files`, `ibcso_bed_files`, `ibcso_digital_chart_files`, `ibcso_sid_files`

* new functions  `etopo1_files` and `etopo2_files` for for NGDC NOAA global topographic data

* new function `lakesuperior_files` for NGDC NOAA Lake Superior bathymetry data

* new function `cryosat2_files` for Cryosat2 topography data

* new functions `gebco14_files` and `gebco08_files` for GEBCO topography data 

* new home for 'NOAA OI 1/4 Degree Daily SST AVHRR' now in ncei.noaa.gov

* new function `amsr_daily_files` for the AMSR 6.25km data 

* new global file cache not bound to a single root

* new `ncep2_uwnd_6hr_files` and `ncep2_vwnd_6hr_files`, note this is quite 
different from the old `raadtools::windfiles` - expectation is that downstream read will explicitly deal with `uwnd`, `vwnd` separately

* new `altimetry_daily_files`, `bom_tmax_daily_files`, `cmip5_files`,  `ghrsst_daily_files`, 
`ncep2_uwnd_6hr_files`, `ncep2_vwnd_6hr_files`, `nsidc_monthly_files`, `nsidc_north_monthly_files`, 
`nsidc_south_monthly_files`, `oisst_daily_files`, `oisst_monthly_files`, `thelist_files`

* new NSIDC monthly files, separates north, south, daily, monthly and provider
 at the function level, also supports fix https://github.com/AustralianAntarcticDivision/raadtools/issues/54

# raadfiles 0.0.1


* standard cache now separates "root" and "file" as separate columns

* general scheme for file cache that can incorporate other collections

* separated out from raadtools


