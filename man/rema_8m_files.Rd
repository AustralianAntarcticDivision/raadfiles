% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rema-files.R, R/rema-more-files.R
\name{rema_8m_files}
\alias{rema_8m_files}
\alias{rema_tile_files}
\alias{rema_100m_files}
\alias{rema_200m_files}
\alias{rema_1km_files}
\alias{rema_8m_tiles}
\alias{rema_200m_dem_files}
\alias{.rema_file_filter}
\alias{rema_200m_dem_geoid_files}
\alias{rema_200m_slope_files}
\alias{rema_200m_aspect_files}
\alias{rema_200m_rugosity_files}
\alias{rema_200m_rock_files}
\alias{rema_100m_dem_files}
\alias{rema_100m_dem_geoid_files}
\alias{rema_100m_slope_files}
\alias{rema_100m_aspect_files}
\alias{rema_100m_rugosity_files}
\alias{rema_100m_rock_files}
\alias{rema_8m_dem_files}
\alias{rema_8m_dem_geoid_files}
\alias{rema_8m_slope_files}
\alias{rema_8m_aspect_files}
\alias{rema_8m_rugosity_files}
\alias{rema_8m_rock_files}
\title{Files for The Reference Elevation Model of Antarctica (REMA)}
\usage{
rema_tile_files(all = FALSE, ...)

rema_100m_files(...)

rema_200m_files(filled = TRUE, ...)

rema_1km_files(filled = TRUE, ...)

rema_8m_files(...)

rema_8m_tiles()

rema_200m_dem_files()

.rema_file_filter(x)

rema_200m_dem_geoid_files()

rema_200m_slope_files()

rema_200m_aspect_files()

rema_200m_rugosity_files()

rema_200m_rock_files()

rema_100m_dem_files()

rema_100m_dem_geoid_files()

rema_100m_slope_files()

rema_100m_aspect_files()

rema_100m_rugosity_files()

rema_100m_rock_files()

rema_8m_dem_files()

rema_8m_dem_geoid_files()

rema_8m_slope_files()

rema_8m_aspect_files()

rema_8m_rugosity_files()

rema_8m_rock_files()
}
\arguments{
\item{all}{for \code{rema_tile_files}, return all or just \code{*.shp} files; for other functions, if `TRUE` include 'data_deprecated', expert-use only}

\item{...}{additional parameters, currently ignored}

\item{filled}{return 'filled' variant if available}

\item{x}{pattern to detect}
}
\value{
data frame of file names
}
\description{
Return files for various products from REMA Release 1
}
\details{
'rema_8m_files' returns the base level 8 GeoTIFF files, there are 1516 files at 8m resolution.
}
\examples{
\dontrun{
  rema_8m_files()
  rema_100m_files(filled = TRUE)
}
}
\references{
\url{https://www.pgc.umn.edu/tag/rema/}
}
