testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_Site(), "character")
  testthat::expect_type(pr_get_s3site(), "character")
  testthat::expect_type(pr_get_Raw("bgc_tss_data"), "list")
  testthat::expect_type(pr_get_s3("bgc_trip"), "list")
  testthat::expect_type(pr_get_s3("bgc_trip.csv"), "list")

  testthat::expect_type(pr_get_PlanktonInfo(Type = "P"), "list")
  testthat::expect_type(pr_get_PlanktonInfo(Type = "Z"), "list")

  testthat::expect_type(pr_get_NRSStation() %>%
                          pr_add_StationName(), "list")

  testthat::expect_type(data.frame(TripCode = "MAI20220628") %>%
                          pr_add_StationCode(), "list")

  testthat::expect_type(pr_get_NRSStation() %>%
                          pr_add_StationCode(), "list")

  testthat::expect_type(pr_get_NRSStation() %>%
                          pr_reorder(), "list")

  testthat::expect_type(data.frame(StationCode = c("MAI", "NSI", "PHB")) %>%
                          pr_reorder(), "list")

  testthat::expect_type(data.frame(SST = c(27.4, 45), SST_Flag = c(1, 4)) %>%
                          pr_apply_Flags(), "list")

  testthat::expect_type(pr_get_Indices("NRS", "P") %>%
                          pr_apply_Time(), "list")

  testthat::expect_true(pr_get_Indices("NRS", "P") %>%
                          pr_apply_Time() %>%
                          colnames() %in% "Month_Local" %>%
                          any())

  testthat::expect_equal(data.frame(Species = c("IncorrectSpecies cf.", "CorrectSpecies1",
                                                NA, "CorrectSpecies2", "Incorrect spp.,
                                                Incorrect/Species")) %>%
                           pr_filter_Species(), data.frame(Species = c("CorrectSpecies1", "CorrectSpecies2")))

  testthat::expect_true(data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
                                   BioVolume_um3m3 = c(100, 150),
                                   PhytoAbund_m3 = c(10, 8)) %>%
                          pr_add_Carbon("CPR") %>%
                          colnames() %in% "Carbon" %>%
                          any())

  testthat::expect_true(data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
                                   Biovolume_um3L = c(100, 150),
                                   Cells_L = c(10, 8)) %>%
                          pr_add_Carbon("NRS") %>%
                          colnames() %in% "Carbon" %>%
                          any())

  pr_harmonic(2, 2) %>%
    testthat::expect_type("double") %>%
    testthat::expect_length(4)

  testthat::expect_type(planktonr::pr_get_PolicyData("NRS") %>% dplyr::filter(.data$Parameters != 'Oxygen_umolL',
                                                                              !.data$StationCode %in% c('NIN', 'ESP')) %>%
                          pr_get_Coeffs(), "list")

  testthat::expect_type(planktonr::pr_get_PolicyData("LTM") %>%
                          pr_get_Coeffs(), "list")

  testthat::expect_type(pr_get_NonTaxaColumns(Survey = "NRS", Type = "Z"), "character")
  testthat::expect_equal(pr_get_NonTaxaColumns(Survey = "NRS", Type = "Z"),
                         c("Project", "StationName", "StationCode", "TripCode", "Latitude",
                           "Longitude", "SampleTime_Local", "SampleTime_UTC", "Year_Local",
                           "Month_Local", "Day_Local", "Time_Local24hr", "SampleDepth_m",
                           "CTDSST_degC", "CTDChlaSurf_mgm3", "CTDSalinity_psu",
                           "Biomass_mgm3", "AshFreeBiomass_mgm3"))

  testthat::expect_type(pr_get_Indices("NRS", "P") %>% pr_remove_outliers(2), "list")
  testthat::expect_type(pr_get_Indices("CPR", "P") %>% pr_remove_outliers(2), "list")
  testthat::expect_type(pr_get_Indices("NRS", "Z") %>% pr_remove_outliers(2), "list")
  testthat::expect_type(pr_get_Indices("CPR", "Z") %>% pr_remove_outliers(2), "list")

  testthat::expect_type(pr_get_SpeciesInfo(Type = "P"), "list")
  testthat::expect_type(pr_get_SpeciesInfo(Type = "Z"), "list")

})

