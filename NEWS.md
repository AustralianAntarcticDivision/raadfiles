# dev

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


