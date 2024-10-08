testthat::test_that("Correct function output", {
  testthat::expect_equal(class(data.frame(StationCode = c("NSI", "PHB")) %>%
                                 pr_plot_NRSmap())[1], "gg")

  testthat::expect_equal(class(data.frame(BioRegion = c("Temperate East", "South-west")) %>%
                                 pr_plot_CPRmap())[1], "gg")

  testthat::expect_equal(class(pr_get_Indices("NRS", "Z") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_TimeSeries("NRS"))[1], "gg")

  testthat::expect_equal(class(pr_get_Indices("CPR", "Z") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_TimeSeries("CPR"))[1], "gg")

  testthat::expect_equal(class(pr_get_Indices("NRS", "Z") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_Trends(Trend = "Year", Survey = "NRS"))[1], "gg")

  testthat::expect_equal(class(pr_get_Indices("CPR", "Z") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_Trends(Trend = "Month", Survey = "CPR"))[1], "gg")

  testthat::expect_equal(class(pr_get_Indices("CPR", "Z") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_Trends(Trend = "Raw", Survey = "CPR"))[1], "gg")

  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Z") %>%
                                 dplyr::filter(Parameters == "ZoopAbundance_m3") %>%
                                 pr_plot_Climatology("CPR", "Year"))[1], "gg")

  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "P") %>%
                                 dplyr::filter(Parameters == "PhytoAbundance_CellsL") %>%
                                 pr_plot_tsclimate("NRS"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_FuncGroups("NRS", "P") %>%
                                 pr_plot_tsfg(Scale = "Actual", Trend = "Raw"))[1], "gg")

  testthat::expect_equal(class(pr_get_FuncGroups("NRS", "P") %>%
                                 pr_plot_tsfg(Scale = "Percent", Trend = "Month"))[1], "gg")

  testthat::expect_equal(class(pr_get_FuncGroups("CPR", "P") %>%
                                 pr_plot_tsfg(Scale = "Actual", Trend = "Year"))[1], "gg")

  testthat::expect_equal(class(planktonr::pr_get_EOVs("NRS") %>%
                                 dplyr::filter(.data$Parameters != 'Oxygen_umolL',
                                                                               !.data$StationCode %in% c('NIN', 'ESP')) %>%
                                 pr_get_Coeffs() %>%
                                 pr_plot_EOVs(EOV = "Biomass_mgm3", Survey = "NRS", trans = "identity",
                                              labels = "yes"))[1], "patchwork")

  testthat::expect_equal(class(planktonr::pr_get_EOVs("CPR") %>%
                                 pr_get_Coeffs() %>%
                                 pr_plot_EOVs(EOV = "Biomass_mgm3", Survey = "CPR", trans = "identity",
                                              labels = "no"))[1], "patchwork")

  testthat::expect_equal(class(planktonr::pr_get_EOVs("LTM") %>%
                                 pr_get_Coeffs() %>%
                                 pr_plot_EOVs(EOV = "Biomass_mgm3", Survey = "LTM", trans = "identity",
                                              labels = "no"))[1], "patchwork")

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
                                 pr_plot_DayNight())[1], "gg")

  testthat::expect_equal(class(data.frame(Month = rep(seq(1,12,1),2),
                                          daynight = c(rep("day", 12), rep("night", 12)),
                                          PhytoAbund_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_DayNight())[1], "gg")

  testthat::expect_equal(class(data.frame(SST = runif(24, 5, 25),
                                          Project = c(rep("cpr", 12), rep("nrs", 12)),
                                          Species_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_STI())[1], "gg")

  testthat::expect_equal(class(pr_get_ProgressMapData(c("CPR", "NRS")) %>%
                                 pr_plot_ProgressMap())[1], "gg")

  testthat::expect_equal(class(pr_get_ProgressMapData(c("CPR", "NRS"), interactive = TRUE) %>%
                                 pr_plot_ProgressMap(interactive = TRUE, labels = FALSE))[1], "leaflet")

  testthat::expect_equal(class(pr_get_ProgressMapData(c("CPR", "NRS"), interactive = TRUE) %>%
                                 pr_plot_ProgressMap(interactive = TRUE))[1], "leaflet")

  testthat::expect_equal(class(data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                                          freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                                          Season = c("December - February","March - May",
                                                     "June - August","September - November"),
                                          Taxon = 'Acartia danae',
                                          Survey = 'CPR') %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = FALSE))[1], "gg")

  testthat::expect_equal(class(data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                                          freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                                          Season = c("December - February","March - May",
                                                     "June - August","September - November"),
                                          Taxon = 'Acartia danae',
                                          Survey = 'CPR') %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = TRUE))[1], "list")



  testthat::expect_equal(class(pr_plot_Gantt(pr_get_CPRTrips(), Survey = "CPR"))[1], "gg")

  testthat::expect_equal(class(pr_plot_Gantt(pr_get_NRSTrips(), Survey = "NRS"))[1], "gg")

  testthat::expect_equal(class(pr_get_TaxaAccum(Survey = "NRS", Type = "Z") %>% pr_plot_TaxaAccum(Survey = "NRS", Type = "Z"))[1], "gg")

  testthat::expect_equal(class(pr_get_NRSEnvContour("Pico") %>%
                                 dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
                                               StationCode %in% c('YON', 'PHB', 'NSI')) %>%
                                 pr_plot_NRSEnvContour(Interpolation = TRUE, Fill_NA = TRUE))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSEnvContour("Pico") %>%
                                 dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
                                               StationCode %in% c('YON', 'PHB', 'NSI')) %>%
                                 pr_plot_NRSEnvContour(Interpolation = FALSE))[1], "patchwork")

  testthat::expect_equal(class(pr_get_FuncGroups("CPR", "P") %>% pr_plot_PieFG())[1], "gg")
  testthat::expect_equal(class(pr_get_FuncGroups("NRS", "Z") %>% pr_plot_PieFG())[1], "gg")


  testthat::expect_equal(class(pr_get_PCIData() %>% pr_plot_PCImap())[1], "gg")

  testthat::expect_equal(class(pr_plot_Voyagemap(pr_get_NRSMicro("GO-SHIP"),
                                                 pr_get_NRSMicro("GO-SHIP") %>% dplyr::slice(1:5000),
                                                 Country = c("AUstralia", "New Zealand")))[1], "gg")

  testthat::expect_equal(class(
    planktonr::pr_get_NRSMicro() %>%
      tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
      dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
      pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = 'none'))[1], "gg")


  testthat::expect_equal(class(
    planktonr::pr_get_NRSMicro('Coastal') %>%
      tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
      dplyr::filter(StationCode %in% c("DEE", "DEB")) %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
      pr_plot_box("Bacterial_Temperature_Index_KD"))[1], "gg")

  testthat::expect_equal(class(
    pr_get_NRSMicro('GO-SHIP') %>%
      dplyr::filter(Parameters == 'Archaea_unique_ASVs', SampleDepth_m < 101) %>%
      pr_plot_latitude(Fill_NA = TRUE, maxGap = 5))[1], "patchwork")

})
