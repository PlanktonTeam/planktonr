# Get Larval Fish Trip Data
#
# @return A dataframe with Larval Fish Trip Data
# @export
#
# @examples
# df <- pr_get_LFTrips()
# @import dplyr
# @importFrom rlang .data
# pr_get_LFTrips <- function(){
#
#   LFSamp <- readr::read_csv(system.file("extdata", "BGC_LFish_Samples.csv", package = "planktonr", mustWork = TRUE), na = "", show_col_types = FALSE,
#                             col_types = readr::cols(FLAG_COMMENT = readr::col_character())) %>%
#     pr_rename() %>%
#     pr_apply_Time() %>%
#     dplyr::select("i_Sample":"SampleDate_Local", "Year":"SampleDate_Local", "Latitude":"Comments")
# }



#' Get Larval Fish Sample Data
#'
#' @return A dataframe with Raw Larval Fish Data
#' @export
#'
#' @examples
#' df <- pr_get_LFData()
pr_get_LFData <- function(){
  df <- planktonr::pr_get_Raw("bgc_larval_fish_count_raw_data") %>%
    pr_rename() %>%
    tidyr::pivot_longer(cols = -c("Project", "StationName", "Latitude", "Longitude", "TripCode",
                                  "SampleTime_UTC", "SampleTime_Local", "Year_Local", "Month_Local",
                                  "Day_Local", "Time_Local24hr", "SampleDepth_m", "Temperature_degC",
                                  "Salinity_psu", "Volume_m3", "Vessel", "TowType", "GearMesh_um",
                                  "Bathymetry_m", "QC_flag"), names_to = "Species", values_to = "Count") %>%
    dplyr::mutate(Species2 = stringr::str_replace(.data$Species, "_3", " (3"),
                  Species2 = stringr::str_replace(.data$Species2, "_", ": "),
                  Species2 = stringr::str_replace(.data$Species2, stringr::coll("."), " "),
                  Species2 = paste0(.data$Species2, ")")) %>%
    mutate(Abundance_1000m3 = (.data$Count/.data$Volume_m3)*1e3)

}


# Make count data of all larval fish
#
# @return A dataframe with Larval Fish Count Data
# @export
#
# @examples
# df <- pr_get_LFCountAll()
# @importFrom rlang .data
# pr_get_LFCountAll <- function(){
#
#   LFCount <- pr_get_LFData() %>%
#     dplyr::mutate(Header = paste(.data$ScientificName, .data$SPCode, sep = " ")) %>%
#     dplyr::select(-.data$ScientificName, -.data$SPCode) %>%
#     dplyr::arrange(.data$Header) %>%
#     tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
#     dplyr::arrange(.data$SampleDate_Local)
# }



# Make BGC data for larval fish
#
# @return A dataframe with Larval Fish BGC Data
# @export
#
# @examples
# df <- pr_get_LFCountBGC()
# @importFrom rlang .data
# pr_get_LFCountBGC <- function(){
#   LFCountBGC <- pr_get_LFData() %>%
#     dplyr::filter(grepl('IMOS', .data$ProjectName)) %>%
#     dplyr::mutate(Header = paste(.data$ScientificName, .data$SPCode, sep = " ")) %>%
#     dplyr::select(-c(.data$ScientificName, .data$SPCode, .data$Temperature_degC, .data$Salinity_psu, .data$FlagComments)) %>%
#     dplyr::arrange(.data$Header) %>%
#     tidyr::pivot_wider(names_from = .data$Header, values_from = .data$TaxonCount, values_fill = 0) %>%
#     dplyr::arrange(.data$SampleDate_Local)
# }



# Plot of larval fish distribution
#
# @param df Dataframe of larval fish data from pr_get_LFData
# @param SpeciesName The species you wish to plot
# @param interactive Whether to return an interactive (leaflet; default) or ggplot.
#
# @return A plot element
# @export
#
# @examples
# pr_get_LFData() %>%
#    pr_plot_LarvalFishDist(SpeciesName = "Acanthuridae_37437900", interactive = TRUE)
# pr_plot_LarvalFishDist <- function(df, SpeciesName, interactive = TRUE){
#
#   dat <- df %>%
#     dplyr::group_by(.data$Species, .data$Latitude, .data$Longitude) %>%
#     dplyr::summarise(Species = dplyr::first(.data$Species),
#                      Species2 = dplyr::first(.data$Species2),
#                      Latitude = dplyr::first(.data$Latitude),
#                      Longitude = dplyr::first(.data$Longitude),
#                      Count = max(.data$Count),
#                      .groups = "drop") %>%
#     tidyr::complete(.data$Species, tidyr::nesting(Longitude, Latitude), fill = list(Count = 0)) %>%
#     dplyr::mutate(Presence = dplyr::if_else(.data$Count > 0, "Present", "Absent")) %>%
#     dplyr::arrange(.data$Presence)
#
#   dat_sp <- dplyr::filter(dat, .data$Species2 == SpeciesName)
#
#   if (interactive == TRUE){
#
#   pal <- leaflet::colorFactor(c("grey", "navy"), domain = c("Absent", "Present"))
#
#   title1 <- htmltools::div(
#     htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title1 {
#                                                 text-align: center;
#                                                 background: rgba(255,255,255,0);
#                                                 font-weight: bold;
#                                                 font-size: 16px;
#                                                 margin: 0;
#                                                 margin-right: 6px}")),
#     unique(dat_sp$Species2))
#
#   map <- leaflet::leaflet() %>%
#     leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
#     leaflet::addCircleMarkers(data = dat_sp,
#                               lat = ~ Latitude,
#                               lng = ~ Longitude,
#                               color = ~pal(Presence),
#                               opacity = 1,
#                               fillOpacity = 1,
#                               radius = ~ifelse(Presence == "Present", 5, 2),) %>%
#     leaflet::addControl(title1,
#                         position = "topright",
#                         className = "map-title1")
#
#   } else {
#     #TODO
#     # Non-interactive not available at the moment
#   }
#
#
# }







