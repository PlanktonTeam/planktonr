#' Add bioregions data to existing df
#'
#' @param df A dataframe with columns `.data$Longitude` and `.data$Latitude`
#' @param join use join arguments from sf::st_join for joining to CPR samples to bioregions.
#' @param ... to allow use of join when used within another function
#'
#' @return A dataframe with Marine Bioregions added
#' @export
#'
#' @examples
#'  df <- pr_get_Raw("cpr_derived_indices_data") %>%
#'   pr_rename() %>%
#'    pr_add_Bioregions("st_nearest_feature")
#' @importFrom rlang .data
pr_add_Bioregions <- function(df, join = "st_within", ...){

  # First add Marine Bioregions
  df <- df %>%
    dplyr::select("Longitude", "Latitude") %>%  # file with columns named .data$Longitude, .data$Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(mbr, join = sf::st_within) %>%
    dplyr::select("REGION") %>%
    dplyr::bind_cols(df) %>%
    dplyr::rename(BioRegion = .data$REGION) %>%  # Below is temporary fix for state-based waters near bioregions
    dplyr::mutate(BioRegion = dplyr::case_when(.data$Longitude >= sf::st_bbox(mbr[mbr$REGION == "Coral Sea",])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$REGION == "Coral Sea",])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$REGION == "Coral Sea",])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$REGION == "Coral Sea",])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "Coral Sea",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$REGION == "Temperate East",])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$REGION == "Temperate East",])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$REGION == "Temperate East",])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$REGION == "Temperate East",])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "Temperate East",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$REGION == "South-west",])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$REGION == "South-west",])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$REGION == "South-west",])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$REGION == "South-west",])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "South-west",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$REGION == "South-east",])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$REGION == "South-east",])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$REGION == "South-east",])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$REGION == "South-east",])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "South-east",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$REGION == "North",])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$REGION == "North",])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$REGION == "North",])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$REGION == "North",])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "North",
                                               TRUE ~ .data$BioRegion)) %>%
    dplyr::relocate(.data$BioRegion, .after = .data$Latitude) %>%
    tibble::as_tibble()

  if(join == 'st_nearest_feature'){

    df <- df %>%
      dplyr::mutate(BioRegion = dplyr::case_when(.data$Longitude > 145 & is.na(.data$BioRegion) ~ 'Other',
        TRUE ~ .data$BioRegion))

    dfn <- df %>%
      dplyr::filter(is.na(.data$BioRegion)) %>%
      dplyr::select("geometry") %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%
      sf::st_join(mbr, join = sf::st_nearest_feature) %>%
      dplyr::select("REGION") %>%
      dplyr::bind_cols(df %>%
                         dplyr::filter(is.na(.data$BioRegion)) %>%
                         dplyr::select(-c("BioRegion", "geometry"))) %>%
      dplyr::rename(BioRegion = .data$REGION)

    df <- df %>%
      dplyr::filter(!is.na(.data$BioRegion)) %>%
      dplyr::bind_rows(dfn)

    x <- df %>%
      dplyr::select("geometry") %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%
      sf::st_join(mbr, join = sf::st_nearest_feature)

    df <- data.frame(sf::st_distance(x, mbr)) %>%
      apply(1, FUN = min) %>%
      data.frame() %>%
      dplyr::rename(DistanceFromBioregion_m = 1) %>%
      dplyr::bind_cols(df) %>%
      dplyr::mutate(DistanceFromBioregion_m = dplyr::case_when(!.data$BioRegion %in% c("Other") ~ .data$DistanceFromBioregion_m)) %>%
      dplyr::relocate(.data$DistanceFromBioregion_m, .after = .data$BioRegion)

  }

  return(df)

}
