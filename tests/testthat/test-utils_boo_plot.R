testthat::test_that("Correct function output", {

  # TODO Just download indices once
  testthat::expect_equal(class(pr_plot_NRSmap(sites = c("NSI", "PHB")))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_plot_CPRmap(sites = c("Temperate East", "South-west")))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_TimeSeries())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_TimeSeries())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_Trends(Trend = "Year"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_Trends(Trend = "Month"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_Trends(Trend = "Raw"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "ZoopAbundance_m3") %>%
                                 pr_plot_Climatology(Trend = "Year"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
                                 dplyr::filter(Parameters == "PhytoAbundance_CellsL") %>%
                                 pr_plot_tsclimate())[1], "patchwork")

  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton") %>%
                                 pr_plot_tsfg(Scale = "Actual", Trend = "Raw"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton") %>%
                                 pr_plot_tsfg(Scale = "Percent", Trend = "Month"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton") %>%
                                 pr_plot_tsfg(Scale = "Actual", Trend = "Year"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(planktonr::pr_get_EOVs(Survey = "NRS") %>%
                                 dplyr::filter(.data$Parameters != 'Oxygen_umolL', .data$StationCode =="PHB") %>%
                                 pr_plot_EOVs(EOV = "Biomass_mgm3", trans = "identity",
                                              labels = TRUE))[1], "patchwork")

  testthat::expect_equal(class(planktonr::pr_get_EOVs(Survey = "CPR") %>%
                                 dplyr::filter(BioRegion == "South-east") %>%
                                 pr_plot_EOVs(EOV = "BiomassIndex_mgm3", trans = "identity",
                                              labels = FALSE))[1], "patchwork")

  testthat::expect_equal(class(planktonr::pr_get_EOVs(Survey = "LTM") %>%
                                 dplyr::filter(.data$StationCode =="PHB") %>%
                                 pr_plot_EOVs(EOV = "Temperature_degC", trans = "identity",
                                              labels = FALSE))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(Parameters == "SecchiDepth_m") %>%
                                 pr_plot_Enviro(Trend = "None", trans = "identity"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(Parameters == "SecchiDepth_m") %>%
                                 pr_plot_Enviro(Trend = "Smoother", trans = "identity"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(Parameters == "SecchiDepth_m") %>%
                                 pr_plot_Enviro(Trend = "Linear", trans = "identity"))[1], "patchwork")

  testthat::expect_equal(class(data.frame(Month = rep(seq(1,12,1),2),
                                          daynight = c(rep("day", 12), rep("night", 12)),
                                          CopeAbundance_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_DayNight())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(data.frame(Month = rep(seq(1,12,1),2),
                                          daynight = c(rep("day", 12), rep("night", 12)),
                                          PhytoAbund_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_DayNight())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(data.frame(SST = runif(24, 5, 25),
                                          Project = c(rep("cpr", 12), rep("nrs", 12)),
                                          Species_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_STI())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_ProgressMapData(Survey = c("CPR", "NRS")) %>%
                                 pr_plot_ProgressMap())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_ProgressMapData(Survey = c("CPR", "NRS"), interactive = TRUE) %>%
                                 pr_plot_ProgressMap(interactive = TRUE, labels = FALSE))[1], "leaflet")

  testthat::expect_equal(class(pr_get_ProgressMapData(Survey = c("CPR", "NRS"), interactive = TRUE) %>%
                                 pr_plot_ProgressMap(interactive = TRUE))[1], "leaflet")

  testthat::expect_equal(class(data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                                          freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                                          Season = c("December - February","March - May",
                                                     "June - August","September - November"),
                                          Taxon = 'Acartia danae',
                                          Survey = 'CPR') %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = FALSE))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                                          freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                                          Season = c("December - February","March - May",
                                                     "June - August","September - November"),
                                          Taxon = 'Acartia danae',
                                          Survey = 'CPR') %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = TRUE))[1], "list")



  testthat::expect_equal(class(pr_plot_Gantt(pr_get_CPRTrips()))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_plot_Gantt(pr_get_NRSTrips()))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_TaxaAccum(Survey = "NRS", Type = "Zooplankton") %>%
                                 pr_plot_TaxaAccum())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_get_NRSEnvContour(Data = "Pico") %>%
                                 dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
                                               StationCode %in% c('YON', 'PHB', 'NSI')) %>%
                                 pr_plot_NRSEnvContour(na.fill = TRUE))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSEnvContour(Data = "Pico") %>%
                                 dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
                                               StationCode %in% c('YON', 'PHB', 'NSI')) %>%
                                 pr_plot_NRSEnvContour(na.fill = FALSE))[1], "patchwork")

  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton") %>% pr_plot_PieFG())[1], "ggplot2::ggplot")
  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton") %>% pr_plot_PieFG())[1], "ggplot2::ggplot")


  testthat::expect_equal(class(pr_get_PCIData() %>% pr_plot_PCImap())[1], "ggplot2::ggplot")

  testthat::expect_equal(class(pr_plot_Voyagemap(pr_get_NRSMicro(Survey = "GO-SHIP"),
                                                 pr_get_NRSMicro(Survey = "GO-SHIP") %>% dplyr::slice(1:5000),
                                                 Country = c("Australia", "New Zealand")))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(
    planktonr::pr_get_NRSMicro() %>%
      tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
      dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
      pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = 'none'))[1], "ggplot2::ggplot")


  testthat::expect_equal(class(
    planktonr::pr_get_NRSMicro(Survey = "Coastal") %>%
      tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
      dplyr::filter(StationCode %in% c("DEE", "DEB")) %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
      pr_plot_box("Bacterial_Temperature_Index_KD"))[1], "ggplot2::ggplot")

  testthat::expect_equal(class(
    pr_get_NRSMicro(Survey = "GO-SHIP") %>%
      dplyr::filter(Parameters == 'Archaea_unique_ASVs', SampleDepth_m < 101) %>%
      pr_plot_latitude(na.fill = TRUE))[1], "patchwork")

})
