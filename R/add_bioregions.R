
#' Add bioregions data to existing df
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom magrittr "%>%"
add_bioregions <- function(df){

  # First add Marine Bioregions
  df <- df %>%
    dplyr::select(Longitude, Latitude) %>%  # file with columns named Longitude, Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(mbr, join = sf::st_within) %>%
    dplyr::select(REGION) %>%
    dplyr::bind_cols(df, .) %>%
    dplyr::rename(BioRegion = REGION) %>%  # Below is temporary fix for state-based waters near bioregions
    dplyr::mutate(BioRegion = dplyr::case_when(Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==17,])$xmin &
                                                 Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==17,])$xmax &
                                                 Latitude >= sf::st_bbox(mbr[mbr$OBJECTID == 17,])$ymin &
                                                 Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==17,])$ymax &
                                                 rlang::are_na(BioRegion) == TRUE ~ "Coral Sea",
                                               Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==2,])$xmin &
                                                 Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==2,])$xmax &
                                                 Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==2,])$ymin &
                                                 Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==2,])$ymax &
                                                 rlang::are_na(BioRegion) == TRUE ~ "Temperate East",
                                               Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==7,])$xmin &
                                                 Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==7,])$xmax &
                                                 Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==7,])$ymin &
                                                 Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==7,])$ymax &
                                                 rlang::are_na(BioRegion) == TRUE ~ "South-west",
                                               Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==13,])$xmin &
                                                 Longitude <= sf::st_bbox(mbr[mbr$OBJECTID==13,])$xmax &
                                                 Latitude >= sf::st_bbox(mbr[mbr$OBJECTID==13,])$ymin &
                                                 Latitude <= sf::st_bbox(mbr[mbr$OBJECTID==13,])$ymax &
                                                 rlang::are_na(BioRegion) == TRUE ~ "South-east",
                                               TRUE ~ BioRegion))

  # Then lets do IMCRA Provinvial Bioregions
  df <- df %>%
    dplyr::select(Longitude, Latitude) %>%  # file with columns named Longitude, Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(imcra_pb, join = sf::st_within) %>%
    dplyr::select(c(WATER_TYPE)) %>%
    dplyr::bind_cols(df, .) %>%
    dplyr::rename(IMCRA_pb = WATER_TYPE)

  # Then lets do IMCRA Mesoscale Bioregions
  df <- df %>%
    dplyr::select(Longitude, Latitude) %>%  # file with columns named Longitude, Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(imcra_meso, join = sf::st_within) %>%
    dplyr::select(c(WATER_TYPE)) %>%
    dplyr::bind_cols(df, .) %>%
    dplyr::rename(IMCRA_meso = WATER_TYPE)

}
