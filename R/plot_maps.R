#' PCI plot for CPR data
#'
#' @param df dataframe with location and seasonal PCI data
#'
#' @return plot of PCI around Australia
#' @export
#'
#' @examples
#' pr_get_PCIData() %>% pr_plot_PCImap()
pr_plot_PCImap <- function(df) {
  cprmap <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = df, ggplot2::aes(x = .data$Longitude, y = .data$Latitude, fill = .data$PCI), interpolate = TRUE) +
    ggplot2::scale_fill_gradient(low = "light green", high = "darkgreen",
                                 breaks = c(0,1,2,3),
                                 labels = c("No Colour", "Very Pale Green", "Pale Green", "Green")) +
    ggplot2::facet_wrap(~ .data$Season) +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    theme_pr() +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 20)) +
    ggplot2::theme(axis.title = ggplot2::element_blank())

  return(cprmap)
}



#' Sidebar panel plot of selected NRS stations
#'
#' @param sites A string vector containing site codes to plot
#' @param Survey Which Survey to plot ("NRS", "Coastal", "LTM")
#' @param Type Must be phytoplankton for SOTS to plot, otherwise it has no impact on the plot
#'
#' @return a map of the selected stations
#' @export
#'
#' @examples
#' sites <- c("MAI", "PHB")
#' pmap <- pr_plot_NRSmap(sites, Type = 'Phytoplankton')
#' pmap <- pr_plot_NRSmap(sites, Survey = "LTM")
pr_plot_NRSmap <- function(sites, Survey = "NRS", Type = 'Zooplankton'){

  if(Survey == "NRS"){
    meta_sf <- meta_sf
  } else if (Survey == "LTM") {
    meta_sf <- meta_sf %>%
      dplyr::filter(.data$Code %in% c("MAI", "PHB", "ROT"))
  } else if (Survey == "Coastal") {
    meta_sf <- csDAT
  }

  if (Type == 'Phytoplankton'){
    meta_sf <- readr::read_csv(system.file("extdata", "BGC_StationInfo.csv", package = "planktonr", mustWork = TRUE),
                           na = "",
                           show_col_types = FALSE,
                           col_types = readr::cols(
                             StationStartDate = readr::col_date(format = "%d/%m/%Y"))) %>%
      pr_rename() %>%
      dplyr::filter(.data$StationCode == "SOTS") %>%
      dplyr::select(Station = StationName, Code = StationCode, Latitude, Longitude) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
      dplyr::bind_rows(meta_sf)
    limlatmin <- -50
  } else {
    meta_sf
    limlatmin <- -45
  }

  meta_sf <- meta_sf %>%
    dplyr::mutate(Colour = dplyr::if_else(.data$Code %in% sites, "Red", "Blue")) %>%
    sf::st_as_sf() # This seems to strip away some of the tibble stuff that makes the filter not work...
  # dplyr::filter(.data$Code %in% df$StationCode)

  col <- meta_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::all_of(c("Code", "Colour"))) %>%
    tibble::deframe()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::geom_sf(data = meta_sf, ggplot2::aes(colour = .data$Code), size = 5, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = col) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(limlatmin, -9)) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))

  return(p1)
}

#' Title Sidebar panel plot of selected CPR bioregions
#'
#' @param sites A string vector containing CPR bioregions to plot
#'
#' @return a map of the selected bioregions
#' @export
#'
#'
#' @examples
#' cprmap <- pr_plot_CPRmap(sites = c("Temperate East", "South-west",
#'                                     "South-east", "North", "North-west"))
pr_plot_CPRmap <-  function(sites){

  bioregionSelection <- mbr %>%
    dplyr::mutate(Colour = dplyr::if_else(.data$REGION %in% sites, .data$Colour, "NA"))

  col <- bioregionSelection %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct() %>%
    dplyr::select(tidyselect::all_of(c("REGION", "Colour"))) %>%
    tibble::deframe()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = bioregionSelection, colour = "black", ggplot2::aes(fill = .data$REGION)) +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   axis.line = ggplot2::element_blank())

  return(p1)
}

#' Sidebar panel plot of voyages
#'
#' @param df dataframe containing all locations to plot
#' @param dfs dataframe of sample locations to plot
#' @param Country countries to plot on map
#'
#' @return a map of the selected bioregions
#' @export
#'
#'
#' @examples
#' df <- pr_get_NRSMicro("GO-SHIP")
#' dfs <- df %>% dplyr::slice(1:5000)
#' voyagemap <- pr_plot_Voyagemap(df, dfs, Country = c("AUstralia", "New Zealand"))
pr_plot_Voyagemap <-  function(df, dfs, Country = c("AUstralia")){

  MapOz <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf", country = Country)

  voy_sf <- df %>%
    dplyr::select(tidyselect::all_of(c("Longitude", "Latitude"))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(.data$Latitude)) %>%
    dplyr::mutate(Lat = as.factor(.data$Latitude),
                  Colour = dplyr::if_else(.data$Latitude %in% dfs$Latitude, "Red", "Blue")) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    sf::st_as_sf() %>%
    sf::st_shift_longitude()

  col <- voy_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::all_of(c("Lat", "Colour"))) %>%
    tibble::deframe()

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    ggplot2::geom_sf(data = voy_sf, ggplot2::aes(colour = .data$Lat), size = 3, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = col) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))

  return(p1)
}



#' Frequency plot of the selected species
#'
#' @param df dataframe of format similar to output of pr_get_fmap_data()
#' @param species species to plot
#' @param interactive ggplot if false, plotlist of leaflets if true
#'
#' @return a plot of frequency of occurrence of chosen species
#' @export
#'
#' @examples
#' df <- data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
#'                  freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
#'                  Season = c("December - February","March - May",
#'                  "June - August","September - November"),
#'                  Taxon = 'Acartia danae',
#'                  Survey = 'CPR')
#' plot <- pr_plot_FreqMap(df, species = 'Acartia danae', interactive = TRUE)
pr_plot_FreqMap <- function(df, species, interactive = TRUE){

  dfa <- df %>%
    dplyr::select(tidyselect::all_of(c("Season", "Latitude", "Longitude", "Survey"))) %>%
    dplyr::distinct()

  dff <- df %>%
    dplyr::filter(.data$Taxon %in% species) %>%
    dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Seen in 25%",'50%', '75%', '100% of Samples'))) %>%
    dplyr::arrange(.data$freqfac)

  if(interactive == FALSE){
    cols <- c("lightblue1", "skyblue3", "blue1", "navyblue")

    Species <- unique(df$Taxon)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = MapOz) +
      ggplot2::geom_point(data=dfa, ggplot2::aes(x=.data$Longitude, y=.data$Latitude, pch = .data$Survey), colour='light grey', size = 1) +
      ggplot2::geom_point(data=dff, ggplot2::aes(x=.data$Longitude, y=.data$Latitude, colour=.data$freqfac, pch = .data$Survey), size = 2) +
      ggplot2::facet_wrap( ~ .data$Season, dir = "v") +
      ggplot2::labs(title = Species) +
      ggplot2::scale_colour_manual(name = "", values = cols, drop = FALSE) +
      theme_pr() +
      ggplot2::theme(title = ggplot2::element_text(face = "italic"),
                     # legend.title = ggplot2::element_text(face = "plain", size = 12),
                     axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "snow1"),
                     legend.key = ggplot2::element_blank())

    return(p)

  } else {

    df <- dff %>% dplyr::group_split(.data$Season)

    plotlist <- function(dflist){

      CPRpal <- leaflet::colorFactor(c("lightblue1", "skyblue3", "dodgerblue", "blue1", "navyblue"), domain = dflist$freqfac)
      NRSpal <- leaflet::colorFactor(c("#CCFFCC", "#99FF99", "#669933", "#009900", "#006600"), domain = dflist$freqfac)

      dfCPR <- dflist %>% dplyr::filter(.data$Survey == 'CPR')
      dfNRS <- dflist %>% dplyr::filter(.data$Survey == 'NRS')

      title1 <- htmltools::div(
        htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title1 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                font-weight: bold;
                                                font-size: 16px;
                                                margin: 0;
                                                margin-right: 6px}")),
        unique(dflist$Season))

      title2 <- htmltools::div(
        htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title2 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                # font-weight: bold;
                                                font-style: italic;
                                                font-size: 16px;
                                                margin: 0;
                                                margin-right: 6px}")),
        unique(dflist$Taxon))

      fmap <- leaflet::leaflet() %>%
        leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
        leaflet::addPolygons(data = mbr,  group = "Marine Bioregions",
                             color = ~Colour, fill = ~Colour,
                             opacity = 1, fillOpacity = 0.3,
                             weight = 1) %>%
        leaflet::addCircleMarkers(data = dfa, group = 'All Samples',
                                  lat = ~ Latitude, lng = ~ Longitude,
                                  radius = 1,
                                  color = 'light grey',
                                  fill = 'light grey') %>%
        leaflet::addCircleMarkers(data = dfCPR, group = 'Continuous Plankton Recorder',
                                  lat = ~ Latitude, lng = ~ Longitude,
                                  radius = 5,
                                  color = ~CPRpal(freqfac),
                                  fill = ~CPRpal(freqfac)) %>%
        leaflet::addCircleMarkers(data = dfNRS , group = 'National Reference Stations',
                                  lat = ~ Latitude, lng = ~ Longitude,
                                  color = ~NRSpal(freqfac),
                                  fill = ~NRSpal(freqfac),
                                  radius = 5) %>%
        leaflet::addControl(title1,
                            position = "topright",
                            className = "map-title1") %>%
        leaflet::addControl(title2,
                            position = "topright",
                            className = "map-title2")  %>%
        leaflet::addLayersControl( # Layers control
          overlayGroups = c("National Reference Stations", "Continuous Plankton Recorder", "Marine Bioregions"),
          position = "bottomleft",
          options = leaflet::layersControlOptions(collapsed = FALSE, fill = NA))
    }

    plotlist <- purrr::map(df, plotlist)

    return(plotlist)
  }
}




#' IMOS progress plot
#'
#' @param df output from pr_get_ProgressMapData
#' @param interactive Should the plot be interactive with leaflet?
#' @param labels TRUE/FALSE Should labels be added to leaflet plot? Adding labels adds more information but slows down the rendering.
#'
#' @return a plot of IMOS progress
#' @export
#'
#' @examples
#' df <- pr_get_ProgressMapData("NRS")
#' plot <- pr_plot_ProgressMap(df)
pr_plot_ProgressMap <- function(df, interactive = FALSE, labels = TRUE){

  if (interactive == TRUE){

    if (is.data.frame(df) == TRUE){ # planktonr will likely return a df
      df_CPR <- df %>%
        dplyr::filter(.data$Survey == "CPR" & (!is.na(.data$ZoopAbundance_m3) | !is.na(.data$PhytoAbundance_CellsL)))

      df_PCI <- df %>%
        dplyr::filter(.data$Survey == "CPR" & is.na(.data$ZoopAbundance_m3) & is.na(.data$PhytoAbundance_CellsL))

      df_NRS <- df %>% dplyr::filter(.data$Survey == "NRS")
    } else if (is.data.frame(df) == FALSE){ # boo will return a list to shrink size

      df_CPR <- df$CPR %>%
        dplyr::filter((!is.na(.data$ZoopAbundance_m3) | !is.na(.data$PhytoAbundance_CellsL)))

      df_PCI <- df$CPR %>%
        dplyr::filter(is.na(.data$ZoopAbundance_m3) & is.na(.data$PhytoAbundance_CellsL))

      df_NRS <- df$NRS

    }

    if(labels == TRUE){
      labs_cpr <- lapply(seq(nrow(df_CPR)), function(i) {
        paste("<strong>Sample Date:</strong>", df_CPR$SampleTime_Local[i], "<br>",
              "<strong>Bioregion:</strong>", df_CPR$Name[i], "<br>",
              "<strong>Latitude:</strong>", df_CPR$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", df_CPR$Longitude[i], "<br>",
              "<strong>Phytoplankton Abundance (L\u207B\u00B9)</strong>: ", round(df_CPR$PhytoAbundance_CellsL[i],2), "<br>",
              "<strong>Zooplankton Abundance (m\u207B\u00B3)</strong>: ", round(df_CPR$ZoopAbundance_m3[i],2), "<br>",
              "<strong>Phytoplankton Colour Index:</strong>", df_CPR$PCI[i], "<br>")})

      labs_pci <- lapply(seq(nrow(df_CPR)), function(i) {
        paste("<strong>Sample Date:</strong>", df_CPR$SampleTime_Local[i], "<br>",
              "<strong>Bioregion:</strong>", df_CPR$Name[i], "<br>",
              "<strong>Latitude:</strong>", df_CPR$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", df_CPR$Longitude[i], "<br>",
              "<strong>Phytoplankton Colour Index:</strong>", df_CPR$PCI[i], "<br>")})

      labs_mbr <- lapply(seq(nrow(mbr)), function(i) {
        if (mbr$REGION[i] != "Southern Ocean Region"){
          br = " Bioregion"
        } else {
          br = ""
        }
        sapply(strwrap(
          paste("<strong>The ", mbr$REGION[i], br,"</strong>",
                "is characterised by",(CPRinfo %>% dplyr::filter(mbr$REGION[i] == .data$BioRegion))$Features, "<br>"),
          width = 60, simplify = FALSE),
          paste, collapse = "<br>")
      })

      labs_mbr <- lapply(labs_mbr, htmltools::HTML)
      labs_cpr <- lapply(labs_cpr, htmltools::HTML)
      labs_pci <- lapply(labs_pci, htmltools::HTML)
    } else {
      labs_mbr <- NULL
      labs_cpr <- NULL
      labs_pci <- NULL
    }

    if (is.data.frame(df) == FALSE){
      labs_nrs <- lapply(seq(nrow(df_NRS)), function(i) {

        if (lubridate::year(df_NRS$End_Date[i]) < 2020){
          EndDate <- paste0(df_NRS$End_Date[i], " (discontinued)")
        } else {
          EndDate <- paste0(df_NRS$End_Date[i], " (ongoing)")
        }

        paste("<strong>National Reference Station:</strong>", df_NRS$Name[i], "<br>",
              "<strong>First Sampling:</strong>", df_NRS$Start_Date[i], "<br>",
              "<strong>Last Sampling:</strong>", EndDate, "<br>",
              "<strong>Latitude:</strong>", df_NRS$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", df_NRS$Longitude[i], "<br>",
              "<strong>Number of Sampling Trips:</strong>", df_NRS$Samples[i], "<br>")})
      labs_nrs <- lapply(labs_nrs, htmltools::HTML)
    } else if (is.data.frame(df) == TRUE){
      labs_nrs <- NULL
    }


    title1 <- htmltools::div(
      htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title1 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                font-weight: bold;
                                                font-size: 24px;
                                                margin: 0;
                                                margin-right: 6px}")),
      "Plankton sampling progress")

    title2 <- htmltools::div(
      htmltools::tags$style(htmltools::HTML(".leaflet-control.map-title2 {
                                                text-align: center;
                                                background: rgba(255,255,255,0);
                                                font-size: 16px;
                                                margin: 0;
                                                margin-right: 6px}")),
      "Hover cursor over items of interest")

    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
      leaflet::addPolygons(data = mbr,  group = "Marine Bioregions",
                           color = ~Colour,
                           fill = ~Colour,
                           opacity = 0.4,
                           fillOpacity = 0.4,
                           weight = 1,
                           label = labs_mbr
      ) %>%
      leaflet::addCircleMarkers(data = df_CPR,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                fill = ~Colour,
                                color = ~Colour,
                                radius = 3, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "Continuous Plankton Recorder (Phyto/Zoo Counts)",
                                label = labs_cpr,
      ) %>%
      leaflet::addCircleMarkers(data = df_PCI,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                fill = ~Colour,
                                color = ~Colour,
                                radius = 1, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "Continuous Plankton Recorder (PCI Only)",
                                label = labs_pci,
      ) %>%
      leaflet::addCircleMarkers(data = df_NRS,
                                lat = ~ Latitude,
                                lng = ~ Longitude,
                                color = "#FFA500",
                                radius = 10, fillOpacity = 0.8, opacity = 1, weight = 1,
                                group = "National Reference Stations",
                                label = labs_nrs,
      ) %>%
      leaflet::addControl(title1,
                          position = "topright",
                          className = "map-title1"
      ) %>%
      leaflet::addControl(title2,
                          position = "topright",
                          className = "map-title2"
      ) %>%
      leaflet::addLayersControl( # Layers control
        overlayGroups = c("National Reference Stations",
                          "Continuous Plankton Recorder (Phyto/Zoo Counts)",
                          "Continuous Plankton Recorder (PCI Only)",
                          "Marine Bioregions"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE, fill = NA)) %>%
      leaflet::addMiniMap() %>%  # add a minimap
      leaflegend::addLegendFactor(pal = leaflet::colorFactor("#FFA500", "National Reference Stations"),
                                  shape = "circle", values = "National Reference Stations") %>%
      leaflet::addLegend("topleft",
                         colors = mbr %>%
                           sf::st_drop_geometry() %>%
                           dplyr::distinct(.data$REGION, .keep_all = TRUE) %>%
                           dplyr::pull(.data$Colour),
                         labels = mbr %>%
                           sf::st_drop_geometry() %>%
                           dplyr::distinct(.data$REGION) %>%
                           dplyr::pull(.data$REGION),
                         title = "Bioregions",
                         opacity = 1) %>%
      leaflet::hideGroup("Continuous Plankton Recorder (PCI Only)")



    return(map)

  } else {

    MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                         returnclass = "sf")

    PMapData2 <- df %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

    PMapSum <- merge(df %>%
                       dplyr::summarise(Sums = dplyr::n(),
                                        .by = tidyselect::all_of(c("Region", "Survey"))) %>%
                       dplyr::mutate(label = paste0(.data$Region, ' = ', .data$Sums)),
                     df %>%
                       dplyr::summarise(Lats = mean(.data$Latitude),
                                        Lons = mean(.data$Longitude),
                                        .by = tidyselect::all_of(c("Region", "Survey")))) %>%
      sf::st_as_sf(coords = c("Lons", "Lats"), crs = 4326)

    nudgex = c(-2,5.5,2,8.5,9.5,0,0,4)
    nudgey = c(-3,0,1,0,0,7,-2,10)
    # GAB, GBR, NA, NEAC, SEAC, SO, Tas, WA

    Survey <- df %>%
      dplyr::select(tidyselect::all_of("Survey")) %>%
      dplyr::distinct()

    gg <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
      ggplot2::coord_sf(xlim = c(105, 170), ylim = c(-54, -7), expand = FALSE) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank())


    if ("CPR" %in% Survey$Survey) {
      gg <- gg +
        ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == "CPR"),
                         size = 1, ggplot2::aes(color =.data$Region), show.legend = TRUE) +
        ggplot2::theme(legend.position = "inside",
                       legend.position.inside = c(.01, .99),
                       legend.justification = c("left", "top"),
                       legend.box.just = "left",
                       legend.background = ggplot2::element_rect(fill = "transparent", colour = "NA"), #transparent legend bg
                       legend.box.background = ggplot2::element_rect(fill = "transparent", colour = "NA")) #transparent legend panel) #+
      # ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == "CPR"),
      #                       size = 5, ggplot2::aes(label = .data$label, color = .data$Region),
      #                       show.legend = FALSE, check_overlap = TRUE, nudge_x = nudgex, nudge_y = nudgey)
    }

    if ("NRS" %in% Survey$Survey) {
      gg <- gg +
        ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == "NRS"), size = 3) +
        ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == "NRS"),
                              size = 4, ggplot2::aes(label = .data$label),
                              show.legend = FALSE, nudge_y = -1)
    }

    return(gg)
  }

}

