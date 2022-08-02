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

  testthat::expect_equal(class(planktonr::pr_get_PolicyData("NRS") %>%
                                 pr_get_Coeffs() %>%
                                 pr_plot_EOV(EOV = "Biomass_mgm3", Survey = "NRS", trans = "identity",
                                             labels = "yes"))[1], "patchwork")

  testthat::expect_equal(class(planktonr::pr_get_PolicyData("CPR") %>%
                                 pr_get_Coeffs() %>%
                                 pr_plot_EOV(EOV = "Biomass_mgm3", Survey = "CPR", trans = "identity",
                                             labels = "no"))[1], "patchwork")

  testthat::expect_equal(class(planktonr::pr_get_PolicyData("LTM") %>%
                                 pr_get_Coeffs() %>%
                                 pr_plot_EOV(EOV = "Biomass_mgm3", Survey = "LTM", trans = "identity",
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

  testthat::expect_equal(class(data.frame(Long = c(110, 130, 155, 150), Lat = c(-10, -35, -27, -45),
                                          freqfac = c("Absent", "Seen in 25%","50%", "75%"),
                                          Season = c("December - February","March - May","June - August","September - November"),
                                          Taxon = "Acartia danae") %>%
                                 pr_plot_FreqMap(species = 'Acartia danae', interactive = FALSE))[1], "gg")

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

  testthat::expect_equal(class(data.frame(sst = runif(24, 5, 25),
                                          Project = c(rep("cpr", 12), rep("nrs", 12)),
                                          Species_m3 = runif(24, 0.1, 10),
                                          Species = "Acartia danae") %>%
                                 pr_plot_STI())[1], "gg")

  testthat::expect_equal(class(pr_get_ProgressMap(c("CPR", "NRS")) %>%
                                 pr_plot_ProgressMap())[1], "gg")

})
