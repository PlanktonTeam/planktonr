#' Add bioregions data to existing df
#'
#' @param df A dataframe with columns `.data$Longitude` and `.data$Latitude`
#' @param near_dist_km What buffer distance to use to find a BioRegion match
#' #'
#' @return A dataframe with Marine Bioregions added
#' @export
#'
#' @examples
#' df <- pr_get_Raw("cpr_derived_indices_data") %>%
#'   pr_rename() %>%
#'   pr_add_Bioregions()
#'
#' df <- pr_get_Raw("cpr_derived_indices_data") %>%
#'   pr_rename() %>%
#'   pr_add_Bioregions(near_dist_km = 250)
#'
#' @importFrom rlang .data
pr_add_Bioregions <- function(df, near_dist_km = 0) {
  # Ensure df is of the correct class
  if (!("sf") %in% class(df[])) {
    df <- df %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84", remove = FALSE)
  }

  # First add Marine Bioregions
  df <- df %>%
    sf::st_join(mbr %>% dplyr::select(-"Colour"), join = sf::st_within) %>%
    dplyr::rename(BioRegion = "REGION") %>%
    dplyr::mutate(cellID = dplyr::row_number())

  # Then do the ones that are missing
  dfna <- df %>%
    dplyr::filter(is.na(.data$BioRegion)) %>%
    dplyr::select(-"BioRegion")

  dist <- dfna %>%
    sf::st_distance(mbr)

  colnames(dist) <- mbr$REGION
  rownames(dist) <- dfna$cellID

  dist <- dist %>%
    as.data.frame.table(responseName = "Dist") %>%
    dplyr::rename(
      cellID = "Var1",
      BioRegion = "Var2"
    ) %>%
    dplyr::mutate(cellID = as.numeric(as.character(.data$cellID))) %>%
    dplyr::filter(.data$Dist <= units::set_units(near_dist_km, "km"))


  # Because of some quirks of boundary - we do the Coral Sea first. Then the rest
  distcs <- dist %>%
    dplyr::filter(.data$BioRegion == "Coral Sea")

  df$BioRegion[distcs$cellID] <- "Coral Sea" # Add the Coral Sea ones to the original df

  # Then continue on with the addition of the other groups
  dist <- dist %>%
    dplyr::group_by(.data$cellID) %>%
    dplyr::slice(which.min(.data$Dist)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"Dist")

  df <- dplyr::left_join(df, dist, by = "cellID") %>%
    dplyr::mutate(
      BioRegion.z = "None",
      BioRegion = dplyr::coalesce(.data$BioRegion.x, .data$BioRegion.y, .data$BioRegion.z)
    ) %>%
    # dplyr::mutate(BioRegion = forcats::fct_explicit_na(.data$BioRegion, na_level = "None")) %>%
    dplyr::select(-c("BioRegion.x", "BioRegion.y", "BioRegion.z")) %>%
    dplyr::relocate("BioRegion", .after = "TripCode") %>%
    sf::st_drop_geometry(df) %>% # DF in, DF out
    dplyr::left_join(
      mbr %>%
        sf::st_drop_geometry() %>%
        dplyr::distinct(.data$REGION, .data$Colour),
      by = c("BioRegion" = "REGION")
    )

  return(df)
}
