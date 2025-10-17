# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_plot_NRSmap creates ggplot for specified sites", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_plot_NRSmap(sites = c("NSI", "PHB")))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_CPRmap creates ggplot for specified bioregions", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_plot_CPRmap(sites = c("Temperate East", "South-west")))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_TimeSeries creates ggplot for NRS Zooplankton biomass", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_TimeSeries())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_TimeSeries creates ggplot for CPR Zooplankton biomass index", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_TimeSeries())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_Trends creates ggplot with Year trend for NRS Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "Biomass_mgm3") %>%
                                 pr_plot_Trends(Trend = "Year"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_Trends creates ggplot with Month trend for CPR Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_Trends(Trend = "Month"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_Trends creates ggplot with Raw trend for CPR Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "BiomassIndex_mgm3") %>%
                                 pr_plot_Trends(Trend = "Raw"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_Climatology creates ggplot with Year trend for CPR Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                                 dplyr::filter(Parameters == "ZoopAbundance_m3") %>%
                                 pr_plot_Climatology(Trend = "Year"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_tsclimate creates patchwork plot for NRS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
                                 dplyr::filter(Parameters == "PhytoAbundance_CellsL") %>%
                                 pr_plot_tsclimate())[1], "patchwork")
})

testthat::test_that("pr_plot_tsfg creates ggplot with Actual scale and Raw trend for NRS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton") %>%
                                 pr_plot_tsfg(Scale = "Actual", Trend = "Raw"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_tsfg creates ggplot with Percent scale and Month trend for NRS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton") %>%
                                 pr_plot_tsfg(Scale = "Percent", Trend = "Month"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_tsfg creates ggplot with Actual scale and Year trend for CPR Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton") %>%
                                 pr_plot_tsfg(Scale = "Actual", Trend = "Year"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_EOVs creates patchwork plot for NRS biomass with labels", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(planktonr::pr_get_EOVs(Survey = "NRS") %>%
                                 dplyr::filter(.data$Parameters != 'Oxygen_umolL', .data$StationCode =="PHB") %>%
                                 pr_plot_EOVs(EOV = "Biomass_mgm3", trans = "identity",
                                              labels = TRUE))[1], "patchwork")
})

testthat::test_that("pr_plot_EOVs creates patchwork plot for CPR biomass index without labels", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(planktonr::pr_get_EOVs(Survey = "CPR") %>%
                                 dplyr::filter(BioRegion == "South-east") %>%
                                 pr_plot_EOVs(EOV = "BiomassIndex_mgm3", trans = "identity",
                                              labels = FALSE))[1], "patchwork")
})

testthat::test_that("pr_plot_EOVs creates patchwork plot for LTM temperature", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(planktonr::pr_get_EOVs(Survey = "LTM") %>%
                                 dplyr::filter(.data$StationCode =="PHB") %>%
                                 pr_plot_EOVs(EOV = "Temperature_degC", trans = "identity",
                                              labels = FALSE))[1], "patchwork")
})

testthat::test_that("pr_plot_Enviro creates patchwork plot with no trend for Secchi depth", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(Parameters == "SecchiDepth_m") %>%
                                 pr_plot_Enviro(Trend = "None", trans = "identity"))[1], "patchwork")
})

testthat::test_that("pr_plot_Enviro creates patchwork plot with smoother trend for Secchi depth", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(Parameters == "SecchiDepth_m") %>%
                                 pr_plot_Enviro(Trend = "Smoother", trans = "identity"))[1], "patchwork")
})

testthat::test_that("pr_plot_Enviro creates patchwork plot with linear trend for Secchi depth", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(Parameters == "SecchiDepth_m") %>%
                                 pr_plot_Enviro(Trend = "Linear", trans = "identity"))[1], "patchwork")
})

testthat::test_that("pr_plot_ProgressMap creates static ggplot for combined surveys", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_ProgressMapData(Survey = c("CPR", "NRS")) %>%
                                 pr_plot_ProgressMap())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_ProgressMap creates interactive leaflet map without labels", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_ProgressMapData(Survey = c("CPR", "NRS"), interactive = TRUE) %>%
                                 pr_plot_ProgressMap(interactive = TRUE, labels = FALSE))[1], "leaflet")
})

testthat::test_that("pr_plot_ProgressMap creates interactive leaflet map with labels", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_ProgressMapData(Survey = c("CPR", "NRS"), interactive = TRUE) %>%
                                 pr_plot_ProgressMap(interactive = TRUE))[1], "leaflet")
})

testthat::test_that("pr_plot_Gantt creates ggplot for CPR trip timeline", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_plot_Gantt(pr_get_CPRTrips()))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_Gantt creates ggplot for NRS trip timeline", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_plot_Gantt(pr_get_NRSTrips()))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_TaxaAccum creates ggplot for NRS Zooplankton taxa accumulation", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_TaxaAccum(Survey = "NRS", Type = "Zooplankton") %>%
                                 pr_plot_TaxaAccum())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_NRSEnvContour creates patchwork plot with NA filling", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_NRSEnvContour(Data = "Pico") %>%
                                 dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
                                               StationCode %in% c('YON', 'PHB', 'NSI')) %>%
                                 pr_plot_NRSEnvContour(na.fill = TRUE))[1], "patchwork")
})

testthat::test_that("pr_plot_NRSEnvContour creates patchwork plot without NA filling", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_NRSEnvContour(Data = "Pico") %>%
                                 dplyr::filter(Parameters == "Prochlorococcus_cellsmL",
                                               StationCode %in% c('YON', 'PHB', 'NSI')) %>%
                                 pr_plot_NRSEnvContour(na.fill = FALSE))[1], "patchwork")
})

testthat::test_that("pr_plot_PieFG creates ggplot for CPR Phytoplankton functional groups", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton") %>% pr_plot_PieFG())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_PieFG creates ggplot for NRS Zooplankton functional groups", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton") %>% pr_plot_PieFG())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_PCImap creates ggplot for PCI data", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_PCIData() %>% pr_plot_PCImap())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_Voyagemap creates ggplot for GO-SHIP voyage tracks", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_plot_Voyagemap(pr_get_NRSMicro(Survey = "GO-SHIP"),
                                                 pr_get_NRSMicro(Survey = "GO-SHIP") %>% dplyr::slice(1:5000),
                                                 Country = c("Australia", "New Zealand")))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_scatter creates ggplot for microbial data scatter plot", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(
    planktonr::pr_get_NRSMicro() %>%
      tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
      dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
      pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = 'none'))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_box creates ggplot for Coastal microbial data boxplot", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(
    planktonr::pr_get_NRSMicro(Survey = "Coastal") %>%
      tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
      dplyr::filter(StationCode %in% c("DEE", "DEB")) %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
      pr_plot_box("Bacterial_Temperature_Index_KD"))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_latitude creates patchwork plot for GO-SHIP Archaea ASVs by latitude", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(
    pr_get_NRSMicro(Survey = "GO-SHIP") %>%
      dplyr::filter(Parameters == 'Archaea_unique_ASVs', SampleDepth_m < 101) %>%
      pr_plot_latitude(na.fill = TRUE))[1], "patchwork")
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_plot_DayNight creates ggplot for copepod abundance day/night comparison", {
  testthat::expect_equal(class(data.frame(Month = rep(seq(1,12,1),2),
                                          daynight = c(rep("day", 12), rep("night", 12)),
                                          CopeAbundance_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_DayNight())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_DayNight creates ggplot for phytoplankton abundance day/night comparison", {
  testthat::expect_equal(class(data.frame(Month = rep(seq(1,12,1),2),
                                          daynight = c(rep("day", 12), rep("night", 12)),
                                          PhytoAbund_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_DayNight())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_STI creates ggplot for species temperature index", {
  testthat::expect_equal(class(data.frame(SST = runif(24, 5, 25),
                                          Project = c(rep("cpr", 12), rep("nrs", 12)),
                                          Species_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_STI())[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_FreqMap creates static ggplot for species frequency", {
  testthat::expect_equal(class(data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                                          freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                                          Season = c("December - February","March - May",
                                                     "June - August","September - November"),
                                          Taxon = 'Acartia danae',
                                          Survey = 'CPR') %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = FALSE))[1], "ggplot2::ggplot")
})

testthat::test_that("pr_plot_FreqMap creates interactive map for species frequency", {
  testthat::expect_equal(class(data.frame(Longitude = c(110, 130, 155, 150), Latitude = c(-10, -35, -27, -45),
                                          freqfac = as.factor(c("Absent", "Seen in 25%",'50%', '75%')),
                                          Season = c("December - February","March - May",
                                                     "June - August","September - November"),
                                          Taxon = 'Acartia danae',
                                          Survey = 'CPR') %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = TRUE))[1], "list")
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_plot_TimeSeries handles invalid data structure", {
  testthat::expect_error(pr_plot_TimeSeries(data.frame(x = 1:10)))
})

testthat::test_that("pr_plot_Trends handles invalid Trend parameter", {
  mock_data <- list(Data = data.frame(SampleDateUTC = Sys.Date(), Values = 1:10))
  class(mock_data) <- c("planktonr_dat", class(mock_data))
  testthat::expect_error(pr_plot_Trends(mock_data, Trend = "INVALID"))
})

testthat::test_that("pr_plot_FreqMap handles missing species parameter", {
  test_data <- data.frame(Longitude = 110, Latitude = -35, freqfac = "Seen in 25%", 
                          Season = "December - February", Taxon = 'Species A')
  testthat::expect_error(pr_plot_FreqMap(test_data, species = NULL))
})
