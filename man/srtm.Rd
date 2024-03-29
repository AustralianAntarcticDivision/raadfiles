% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/srtm-files.R
\name{srtm}
\alias{srtm}
\alias{srtm_files}
\title{SRTM files}
\usage{
srtm_files()
}
\value{
tibble data frame of file names, with columns `fullname`, `x`, `y` (tiles)

data frame with `fullname` file path, `x`, `y` tile column and row indices,  `root` the data file root path
}
\description{
SRTM 90m Digital Elevation Database v4.1
}
\details{
DOI: 0.1080/13658810601169899
}
\examples{
\dontrun{
  srtm_files()
}
}
\references{
[https://cgiarcsi.community/data/srtm-90m-digital-elevation-database-v4-1/]
}
