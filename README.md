<!-- README.md is generated from README.Rmd. Please edit that file -->
raadfiles
=========

The goal of raadfiles is to manage information about the files used by raadtools.

Why raadfiles?
--------------

``` r
library(raadtools)
#> Loading required package: raster
#> Loading required package: sp
system.time(rt_files <- sstfiles())
#>    user  system elapsed 
#>   8.032   0.228   8.310

library(raadfiles)
system.time(rf_files <- oisst_daily_files())
#>    user  system elapsed 
#>   1.100   0.124   1.224

range(rt_files$date)
#> [1] "1981-09-01 10:00:00 AEST" "2017-05-30 10:00:00 AEST"
range(rf_files$date)
#> [1] "1981-09-01 10:00:00 AEST" "2017-05-30 10:00:00 AEST"

length(rt_files$date)
#> [1] 13056
length(rf_files$date)
#> [1] 13056
```
