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
#' df <-   df <- pr_get_Raw("cpr_derived_indices_data") %>%
#'   pr_rename() %>%
#'    pr_add_Bioregions("st_nearest_feature")
#' @importFrom rlang .data
pr_add_Bioregions <- function(df, join = "st_within", ...){

  # First add Marine Bioregions
  df <- df %>%
    dplyr::select(.data$Longitude, .data$Latitude) %>%  # file with columns named .data$Longitude, .data$Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(mbr, join = sf::st_within) %>%
    dplyr::select(.data$REGION) %>%
    dplyr::bind_cols(df) %>%
    dplyr::rename(BioRegion = .data$REGION) %>%  # Below is temporary fix for state-based waters near bioregions
    dplyr::mutate(BioRegion = dplyr::case_when(.data$Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==17,])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==17,])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$OBJECTID == 17,])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==17,])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "Coral Sea",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==2,])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==2,])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==2,])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==2,])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "Temperate East",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==7,])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==7,])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==7,])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==7,])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "South-west",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==13,])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==13,])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==13,])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==13,])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "South-east",
                                               .data$Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==1,])$xmin &
                                                 .data$Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==1,])$xmax &
                                                 .data$Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==1,])$ymin &
                                                 .data$Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==1,])$ymax &
                                                 rlang::are_na(.data$BioRegion) == TRUE ~ "North",
                                               TRUE ~ .data$BioRegion)) %>%
    dplyr::relocate(.data$BioRegion, .after = .data$Latitude) %>%
    tibble::as_tibble()

  if(join == 'st_nearest_feature'){

    df <- df %>%
      dplyr::mutate(BioRegion = dplyr::case_when(.data$Latitude < -47.17 & is.na(.data$BioRegion) ~ 'Southern Ocean',
                                          .data$Longitude > 145 & is.na(.data$BioRegion) ~ 'Other',
                                          TRUE ~ .data$BioRegion))

    dfn <- df %>%
      dplyr::filter(is.na(.data$BioRegion)) %>%
      dplyr::select(.data$geometry) %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%  # file with columns named .data$Longitude, .data$Latitude
      sf::st_join(mbr, join = sf::st_nearest_feature) %>%
      dplyr::select(.data$REGION) %>%
      dplyr::bind_cols(df %>% dplyr::filter(is.na(.data$BioRegion)) %>% dplyr::select(-c(.data$BioRegion, .data$geometry))) %>%
      dplyr::rename(BioRegion = .data$REGION)

    df <- df %>%
      dplyr::filter(!is.na(.data$BioRegion)) %>%
      dplyr::bind_rows(dfn)

    x <- df %>%
      dplyr::select(.data$geometry) %>%
      sf::st_as_sf(sf_column_name = "geometry") %>%  # file with columns named .data$Longitude, .data$Latitude
      sf::st_join(mbr, join = sf::st_nearest_feature)

    df <- data.frame(sf::st_distance(x, mbr)) %>%
      apply(1, FUN = min) %>%
      data.frame() %>%
      dplyr::rename(DistanceFromBioregion_m = 1) %>%
      dplyr::bind_cols(df) %>%
      dplyr::mutate(DistanceFromBioregion_m = dplyr::case_when(!.data$BioRegion %in% c("Southern Ocean", "Other") ~ .data$DistanceFromBioregion_m)) %>%
      dplyr::relocate(.data$DistanceFromBioregion_m, .after = .data$BioRegion)

  }

  return(df)

  # # Then lets do IMCRA Provinvial Bioregions
  # df <- df %>%
  #   sf::st_as_sf(sf_column_name = "geometry", crs = "+proj=longlat +datum=WGS84") %>%
  #   sf::st_join(imcra_pb, join = sf::st_within) %>%
  #   tibble::as_tibble() %>%
  #   dplyr::select(.data$WATER_TYPE) %>%
  #   dplyr::bind_cols(df) %>%
  #   dplyr::rename(IMCRA_pb = .data$WATER_TYPE) %>%
  #   dplyr::relocate(.data$IMCRA_pb, .after = .data$BioRegion)

  # # Then lets do IMCRA Mesoscale Bioregions
  # df <- df %>%
  #   dplyr::select(.data$Longitude, .data$Latitude) %>%  # file with columns named .data$Longitude, .data$Latitude
  #   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
  #   sf::st_join(imcra_meso, join = sf::st_within) %>%
  #   tibble::as_tibble() %>%
  #   dplyr::select(.data$WATER_TYPE) %>%
  #   dplyr::bind_cols(df) %>%
  #   dplyr::rename(IMCRA_meso = .data$WATER_TYPE) %>%
  #   dplyr::relocate(.data$IMCRA_meso, .after = .data$IMCRA_pb)

}
