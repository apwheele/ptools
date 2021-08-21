# Documentation for datasets

#' NYPD Open Data on Shootings
#'
#' Shootings recorded from the New York City Police Department from 2006 to current.
#'
#' @format A data frame with currently over 20k rows and 21 fields, including date/time and address level geocoordinates for the event. Data from 2006 to currently. See the info on Socrata for the field name codebook.
#'
#' @source 
#'  - \url{https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8} for current
#'  - \url{https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8} for historical
"nyc_shoot"

#' NYC Boroughs
#'
#' Spatial file for New York City Borough outlines without water areas
#'
#' @format A SpatialPolygonsDataFrame object of the NYC Boroughs. This is projected (same coordinates as shootings). See the [Bytes of the Big Apple](https://www1.nyc.gov/site/planning/data-maps/open-data.page) for any details on the file.
#'
#' @source 
#'  - \url{https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nybb_21c.zip}
"nyc_bor"