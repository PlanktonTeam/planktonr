#' Add bioregions data to existing df
#'
#' @param df A dataframe with columns `.data$Longitude` and `.data$Latitude`
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_add_bioregions <- function(df){

  # First add Marine Bioregions
  df <- df %>%
    select(.data$Longitude, .data$Latitude) %>%  # file with columns named .data$Longitude, .data$Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(mbr, join = sf::st_within) %>%
    select(.data$REGION) %>%
    bind_cols(df, .) %>%
    rename(BioRegion = .data$REGION) %>%  # Below is temporary fix for state-based waters near bioregions
    mutate(BioRegion = case_when(.data$Longitude >= sf::st_bbox(mbr[mbr$OBJECTID==17,])$xmin &
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
                                               TRUE ~ .data$BioRegion))

  # Then lets do IMCRA Provinvial Bioregions
  df <- df %>%
    # select(.data$Longitude, .data$Latitude) %>%  # file with columns named .data$Longitude, .data$Latitude
    sf::st_as_sf(sf_column_name = "geometry", crs = "+proj=longlat +datum=WGS84") %>%
    # sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(imcra_pb, join = sf::st_within) %>%
    tibble::as_tibble() %>%
    select(.data$WATER_TYPE) %>%
    bind_cols(df, .) %>%
    rename(IMCRA_pb = .data$WATER_TYPE)

  # Then lets do IMCRA Mesoscale Bioregions
  df <- df %>%
    select(.data$Longitude, .data$Latitude) %>%  # file with columns named .data$Longitude, .data$Latitude
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    sf::st_join(imcra_meso, join = sf::st_within) %>%
    tibble::as_tibble() %>%
    select(.data$WATER_TYPE) %>%
    bind_cols(df, .) %>%
    rename(IMCRA_meso = .data$WATER_TYPE)

}
