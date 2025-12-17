#' Assign Australian Marine Bioregions to sample locations
#'
#' Add bioregion classification to samples based on their geographic coordinates.
#' Uses Australian marine bioregion boundaries (IMCRA 4.0) from the Integrated
#' Marine and Coastal Regionalisation of Australia.
#'
#' @param dat A dataframe containing columns `Longitude` and `Latitude` with
#'   geographic coordinates in decimal degrees (WGS84)
#' @param near_dist_km Buffer distance (in kilometres) to use when assigning
#'   bioregions to samples that fall outside boundaries. Default is `0` (no
#'   buffer). Typical values:
#'   * `0` - Exact match only, samples outside boundaries are labelled "None"
#'   * `50` - Assigns samples within 50 km of a bioregion
#'   * `250` - More generous buffer, useful for CPR transects near boundaries
#'
#' @details
#' ## Bioregion Classification
#' The function assigns samples to one of the following Australian marine bioregions:
#' * **Temperate East**: Coastal waters off NSW and southern Queensland
#' * **South-east**: Bass Strait and waters off Tasmania
#' * **South-west**: Southern and western Australian waters
#' * **North-west**: Waters off northwest Western Australia
#' * **Coral Sea**: Offshore waters northeast of Queensland
#' * **None**: Samples that don't fall within any bioregion
#'
#' ## Assignment Method
#' The function uses spatial operations to:
#' 1. Convert coordinates to spatial features (sf objects)
#' 2. Match samples to bioregions using polygon intersection
#' 3. For samples outside boundaries, find the nearest bioregion within `near_dist_km`
#' 4. Prioritise Coral Sea assignments to handle boundary overlaps
#' 5. Add a colour column for plotting consistency
#'
#' ## Buffer Distance
#' The `near_dist_km` parameter is particularly useful for:
#' * CPR samples collected near bioregion boundaries
#' * Offshore samples that may fall just outside defined regions
#' * Creating continuous coverage for transect data
#'
#' Use larger buffers (e.g., 250 km) for offshore CPR data, smaller or no buffer
#' for coastal NRS data.
#'
#' ## Data Requirements
#' Input dataframe must include `Longitude` and `Latitude` columns. The function
#' handles coordinate transformations automatically.
#'
#' @return A dataframe with two additional columns:
#'   * `BioRegion`: Name of the marine bioregion
#'   * `Colour`: Hex colour code for that bioregion (for plotting)
#'
#' @seealso
#' * [pr_get_CPRData()] which calls this function internally
#' * [mbr] for the marine bioregion spatial data
#'
#' @export
#'
#' @examples
#' # Add bioregions with exact matching (no buffer)
#' dat <- pr_get_Raw("cpr_derived_indices_data") %>%
#'   pr_add_Bioregions()
#'
#' # Add bioregions with 250 km buffer for offshore samples
#' dat <- pr_get_Raw("cpr_derived_indices_data") %>%
#'   pr_add_Bioregions(near_dist_km = 250)
#'
#' # Check bioregion assignments
#' table(dat$BioRegion)
#'
#' @importFrom rlang .data
pr_add_Bioregions <- function(dat, near_dist_km = 0){

  # Input validation
  assertthat::assert_that(
    is.data.frame(dat),
    msg = "'dat' must be a data frame."
  )

  assertthat::assert_that(
    all(c("Longitude", "Latitude") %in% colnames(dat)),
    msg = "'dat' must contain 'Longitude' and 'Latitude' columns."
  )

  assertthat::assert_that(
    is.numeric(near_dist_km) && length(near_dist_km) == 1 && near_dist_km >= 0,
    msg = "'near_dist_km' must be a single non-negative numeric value (distance in km)."
  )

  Type <- pr_get_type(dat)
  Survey <- pr_get_survey(dat)

  # Ensure dat is of the correct class
  if (!("sf") %in% class(dat[])){
    dat <- dat %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84", remove = FALSE)
  }

  # First add Marine Bioregions
  dat <- dat %>%
    sf::st_join(mbr %>% dplyr::select(-"Colour"), join = sf::st_within) %>%
    dplyr::rename(BioRegion = "REGION") %>%
    dplyr::mutate(cellID = dplyr::row_number())

  # Then do the ones that are missing
  datna <- dat %>%
    dplyr::filter(is.na(.data$BioRegion)) %>%
    dplyr::select(-"BioRegion")

  dist <- datna %>%
    sf::st_distance(mbr)

  colnames(dist) <- mbr$REGION
  rownames(dist) <- datna$cellID

  dist <- dist %>%
    as.data.frame.table(responseName = "Dist") %>%
    dplyr::rename(cellID = "Var1",
                  BioRegion = "Var2") %>%
    dplyr::mutate(cellID = as.numeric(as.character(.data$cellID))) %>%
    dplyr::filter(.data$Dist <= units::set_units(near_dist_km, "km"))


  # Because of some quirks of boundary - we do the Coral Sea first. Then the rest
  distcs <- dist %>%
    dplyr::filter(.data$BioRegion == "Coral Sea")

  dat$BioRegion[distcs$cellID] <- "Coral Sea" # Add the Coral Sea ones to the original dat

  # Then continue on with the addition of the other groups
  dist <- dist %>%
    dplyr::slice(which.min(.data$Dist), .by = tidyselect::all_of("cellID")) %>%
    dplyr::select(-tidyselect::all_of("Dist"))

  dat <- dplyr::left_join(dat, dist, by = "cellID") %>%
    dplyr::mutate(BioRegion.z = "None",
      BioRegion = dplyr::coalesce(.data$BioRegion.x, .data$BioRegion.y, .data$BioRegion.z)) %>%
    # dplyr::mutate(BioRegion = forcats::fct_explicit_na(.data$BioRegion, na_level = "None")) %>%
    dplyr::select(-tidyselect::all_of(c("BioRegion.x", "BioRegion.y", "BioRegion.z"))) %>%
    dplyr::relocate("BioRegion", .after = "TripCode") %>%
    sf::st_drop_geometry(dat) %>% # dat in, dat out
    dplyr::left_join(mbr %>%
                       sf::st_drop_geometry() %>%
                       dplyr::distinct(.data$REGION, .data$Colour),
                     by = c("BioRegion" = "REGION")) %>%
    planktonr_dat(Survey = Survey, Type = Type)

  return(dat)

}
