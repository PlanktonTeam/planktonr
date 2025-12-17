## Documentation for package datasets (roxygen2)
## This file provides roxygen2 blocks for datasets so `devtools::document()` or
## `roxygen2::roxygenize()` will automatically generate the corresponding .Rd files

#' Coastal microbial station locations
#'
#' An sf object containing coastal microbial (NRS Coastal) station locations and basic
#' metadata used by plotting and mapping helpers in the package.
#'
#' @format An object of class \code{sf} (a data.frame with spatial geometry). The object
#' contains one row per station and columns including at least:
#' \describe{
#'   \item{StationName}{station name (character)}
#'   \item{StationCode}{station code (character)}
#'   \item{Longitude}{decimal longitude (numeric)}
#'   \item{Latitude}{decimal latitude (numeric)}
#'   \item{State}{state (factor)}
#'   \item{geometry}{point geometry (sfc_POINT)}
#' }
#'
#' @source Constructed from Coastal microbial station data via \code{pr_get_data(Survey = "Coastal", Type = "Micro")} in
#' \code{data-raw/DATASET.R}.
#' @docType data
#' @keywords datasets
"csDAT"

#' Marine bioregions (polygons and colours)
#'
#' An sf object containing marine bioregion polygons and associated colour codes used
#' for map rendering and region-based plotting in the package.
#'
#' @format An object of class \code{sf} (a data.frame with spatial geometry). The object
#' contains one row per bioregion and columns including at least:
#' \describe{
#'   \item{REGION}{bioregion name (character)}
#'   \item{Colour}{hex colour code associated with the region (character)}
#'   \item{geometry}{polygon geometry (sfc_MULTIPOLYGON or sfc_POLYGON)}
#' }
#'
#' @source Constructed from the marine regions shapefiles and supporting data in
#' \code{data-raw/} by the \code{data-raw/DATASET.R} script.
#' @docType data
#' @keywords datasets
"mbr"
