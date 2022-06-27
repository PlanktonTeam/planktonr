testthat::test_that("Correct function output", {
  testthat::expect_equal(class(data.frame(StationCode = c("NSI", "PHB")) %>%
                                 pr_plot_NRSmap())[1], "gg")

  testthat::expect_equal(class(data.frame(BioRegion = c("Temperate East", "South-west")) %>%
                                 pr_plot_CPRmap())[1], "gg")

  testthat::expect_equal(class(pr_get_indices("NRS", "Z") %>%
                                 dplyr::filter(parameters == "Biomass_mgm3") %>%
                                 pr_plot_timeseries('NRS', 'matter'))[1], "gg")

  testthat::expect_equal(class(pr_get_indices("CPR", "Z") %>%
                                 dplyr::filter(parameters == "Biomass_mgm3") %>%
                                 pr_plot_timeseries("CPR", "matter"))[1], "gg")

  testthat::expect_equal(class(pr_get_indices("NRS", "Z") %>%
                                 dplyr::filter(parameters == 'Biomass_mgm3') %>%
                                 pr_plot_trends(trend = "Year", survey = "NRS"))[1], "gg")

  testthat::expect_equal(class(pr_get_indices("CPR", "Z") %>%
                                 dplyr::filter(parameters == 'BiomassIndex_mgm3') %>%
                                 pr_plot_trends(trend = "Month", survey = "CPR"))[1], "gg")

  testthat::expect_equal(class(pr_get_indices("CPR", "Z") %>%
                                 dplyr::filter(parameters == 'Biomass_mgm3') %>%
                                 pr_plot_trends(trend = "Raw", survey = "CPR"))[1], "gg")

  testthat::expect_equal(class(pr_get_indices(Survey = "CPR", Type = "Z") %>%
                                 dplyr::filter(parameters == "ZoopAbundance_m3") %>%
                                 pr_plot_climate("CPR", "Year", "matter"))[1], "gg")

  testthat::expect_equal(class(pr_get_indices(Survey = "NRS", Type = "P") %>%
                                 dplyr::filter(parameters == "PhytoAbundance_CellsL") %>%
                                 pr_plot_tsclimate("NRS"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_fg("NRS", "P") %>%
                                 pr_plot_tsfg(Scale = "Actual", trend = "Raw", pal = "matter"))[1], "gg")

  testthat::expect_equal(class(pr_get_fg("NRS", "P") %>%
                                 pr_plot_tsfg(Scale = "Percent", trend = "Month", pal = "matter"))[1], "gg")

  testthat::expect_equal(class(pr_get_fg("NRS", "P") %>%
                                 pr_plot_tsfg(Scale = "Actual", trend = "Year", pal = "matter"))[1], "gg")

  testthat::expect_equal(class(data.frame(SampleTime_Local = c("2010-01-01","2010-02-25",
                                                               "2010-06-21","2010-04-11","2010-08-05"),
                                          StationName = "Port Hacking", parameters = "Biomass_mgm3",
                                          Values = runif(5, 1, 50), fv = runif(5, 1, 50),
                                          anomaly = runif(5, 1, 3), Month_Local = runif(5, 1, 6)) %>%
                                 pr_plot_EOV("Biomass_mgm3", "NRS", "identity", "matter", "yes"))[1], "patchwork")

  testthat::expect_equal(class(data.frame(SampleTime_Local = c("2010-01-01","2010-02-25",
                                                               "2010-06-21","2010-04-11","2010-08-05"),
                                          StationName = "Port Hacking", parameters = "Biomass_mgm3",
                                          Values = runif(5, 1, 50), fv = runif(5, 1, 50),
                                          anomaly = runif(5, 1, 3), Month_Local = runif(5, 1, 6)) %>%
                                 pr_plot_EOV("Biomass_mgm3", "CPR", "identity", "matter", "no"))[1], "patchwork")

  testthat::expect_equal(class(data.frame(SampleTime_Local = c("2010-01-01","2010-02-25",
                                                               "2010-06-21","2010-04-11","2010-08-05"),
                                          StationName = "Port Hacking", parameters = "Biomass_mgm3",
                                          Values = runif(5, 1, 50), fv = runif(5, 1, 50),
                                          anomaly = runif(5, 1, 3), Month_Local = runif(5, 1, 6)) %>%
                                 pr_plot_EOV("Biomass_mgm3", "LTM", "identity", "matter", "no"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(parameters == "SecchiDepth_m") %>%
                                 pr_plot_env_var(pal = "matter", trend = "None", Scale = "identity"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(parameters == "SecchiDepth_m") %>%
                                 pr_plot_env_var(pal = "matter", trend = "Smoother", Scale = "identity"))[1], "patchwork")

  testthat::expect_equal(class(pr_get_NRSChemistry() %>%
                                 dplyr::filter(parameters == "SecchiDepth_m") %>%
                                 pr_plot_env_var(pal = "matter", trend = "Linear", Scale = "identity"))[1], "patchwork")

  testthat::expect_equal(class(data.frame(Long = c(110, 130, 155, 150), Lat = c(-10, -35, -27, -45),
                                          freqfac = c("Absent", "Seen in 25%",'50%', '75%'),
                                          Season = c("December - February","March - May","June - August","September - November"),
                                          Taxon = 'Acartia danae') %>%
                                 pr_plot_fmap())[1], "gg")

  testthat::expect_equal(class(data.frame(Month = rep(seq(1,12,1),2),
                                          daynight = c(rep('day', 12), rep('night', 12)),
                                          CopeAbundance_m3 = runif(24, 0.1, 10),
                                          Species = 'Acartia danae') %>%
                                 pr_plot_daynight())[1], "gg")

  testthat::expect_equal(class(data.frame(sst = runif(24, 5, 25),
                                          Project = c(rep('cpr', 12), rep('nrs', 12)),
                                          Species_m3 = runif(24, 0.1, 10),
                                          Species = 'Acartia danae') %>%
                                 pr_plot_sti())[1], "gg")
})
