testthat::test_that("pr_get_Site returns character vector", {
  testthat::expect_type(planktonr:::pr_get_Site(), "character")
})

testthat::test_that("pr_get_s3site returns character vector", {
  testthat::expect_type(planktonr:::pr_get_s3site(), "character")
})

testthat::test_that("pr_get_Raw returns list for bgc_tss_data", {
  testthat::expect_type(pr_get_Raw("bgc_tss_data"), "list")
})

testthat::test_that("pr_get_s3 returns list with and without .csv extension", {
  testthat::expect_type(pr_get_s3("bgc_trip"), "list")
  testthat::expect_type(pr_get_s3("bgc_trip.csv"), "list")
})

testthat::test_that("pr_add_StationCode adds station code from TripCode", {
  testthat::expect_type(data.frame(TripCode = "MAI20220628") %>%
                          pr_add_StationCode(), "list")
})

testthat::test_that("data.frame with StationCode creates list", {
  testthat::expect_type(data.frame(StationCode = c("MAI", "NSI", "PHB")), "list")
})

testthat::test_that("pr_apply_Flags filters data based on quality flags", {
  testthat::expect_type(data.frame(SST = c(27.4, 45), SST_Flag = c(1, 4)) %>%
                          pr_apply_Flags(), "list")
})

testthat::test_that("pr_apply_Time adds time-based columns to indices data", {
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
                          pr_apply_Time(), "list")
})

testthat::test_that("pr_apply_Time adds Month_Local column", {
  testthat::expect_true(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
                          pr_apply_Time() %>%
                          colnames() %in% "Month_Local" %>%
                          any())
})

testthat::test_that("pr_filter_Species removes invalid species names", {
  testthat::expect_equal(data.frame(Species = c("IncorrectSpecies cf.", "CorrectSpecies1",
                                                NA, "CorrectSpecies2", "Incorrect spp.,
                                                Incorrect/Species")) %>%
                           pr_filter_Species(), data.frame(Species = c("CorrectSpecies1", "CorrectSpecies2")))
})

testthat::test_that("pr_add_Carbon adds Carbon column for CPR data", {
  testthat::expect_true(data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
                                   BioVolume_um3m3 = c(100, 150),
                                   PhytoAbund_m3 = c(10, 8)) %>%
                          pr_add_Carbon("CPR") %>%
                          colnames() %in% "Carbon" %>%
                          any())
})

testthat::test_that("pr_add_Carbon adds Carbon column for NRS data", {
  testthat::expect_true(data.frame(TaxonGroup = c("Dinoflagellate", "Cyanobacteria"),
                                   Biovolume_um3L = c(100, 150),
                                   Cells_L = c(10, 8)) %>%
                          pr_add_Carbon("NRS") %>%
                          colnames() %in% "Carbon" %>%
                          any())
})

testthat::test_that("pr_harmonic returns numeric vector of length 4", {
  pr_harmonic(2, 2) %>%
    testthat::expect_type("double") %>%
    testthat::expect_length(4)
})

testthat::test_that("pr_get_EOVs returns list for NRS survey with filters", {
  testthat::expect_type(planktonr::pr_get_EOVs(Survey = "NRS") %>%
                          dplyr::filter(.data$Parameters != 'Oxygen_umolL',
                                        !.data$StationCode %in% c('NIN', 'ESP')), "list")
})

testthat::test_that("pr_get_EOVs returns list for LTM survey", {
  testthat::expect_type(planktonr::pr_get_EOVs(Survey = "LTM"), "list")
})

testthat::test_that("pr_get_NonTaxaColumns returns character vector for NRS Zooplankton", {
  testthat::expect_type(pr_get_NonTaxaColumns(Survey = "NRS", Type = "Zooplankton"), "character")
})

testthat::test_that("pr_get_NonTaxaColumns returns expected column names for NRS Zooplankton", {
  testthat::expect_equal(pr_get_NonTaxaColumns(Survey = "NRS", Type = "Zooplankton"),
                         c("Project", "StationName", "StationCode", "TripCode", "Latitude",
                           "Longitude", "SampleTime_Local", "SampleTime_UTC", "Year_Local",
                           "Month_Local", "Day_Local", "Time_Local24hr", "SampleDepth_m",
                           "CTDSST_degC", "CTDChlaSurf_mgm3", "CTDSalinity_psu",
                           "Biomass_mgm3", "AshFreeBiomass_mgm3"))
})

testthat::test_that("pr_remove_outliers returns list for NRS Phytoplankton indices", {
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>% pr_remove_outliers(2), "list")
})

testthat::test_that("pr_remove_outliers returns list for CPR Phytoplankton indices", {
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Phytoplankton") %>% pr_remove_outliers(2), "list")
})

testthat::test_that("pr_remove_outliers returns list for NRS Zooplankton indices", {
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Zooplankton") %>% pr_remove_outliers(2), "list")
})

testthat::test_that("pr_remove_outliers returns list for CPR Zooplankton indices", {
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>% pr_remove_outliers(2), "list")
})

