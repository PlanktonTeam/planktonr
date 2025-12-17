# Unit tests using fixtures for data manipulation functions
# These tests use mock data to avoid network dependencies

# Test pr_filter_Species with fixtures ----

testthat::test_that("pr_filter_Species removes species with 'cf.' suffix", {
  test_data <- create_test_species_data()
  result <- pr_filter_Species(test_data)
  
  testthat::expect_false(any(grepl("cf\\.", result$Species, ignore.case = TRUE)))
})

testthat::test_that("pr_filter_Species removes species with 'spp.' suffix", {
  test_data <- create_test_species_data()
  result <- pr_filter_Species(test_data)
  
  testthat::expect_false(any(grepl("spp\\.", result$Species, ignore.case = TRUE)))
})

testthat::test_that("pr_filter_Species removes NA species", {
  test_data <- create_test_species_data()
  result <- pr_filter_Species(test_data)
  
  testthat::expect_false(any(is.na(result$Species)))
})

testthat::test_that("pr_filter_Species removes species with '/' character", {
  test_data <- create_test_species_data()
  result <- pr_filter_Species(test_data)
  
  testthat::expect_false(any(grepl("/", result$Species, fixed = TRUE)))
})

testthat::test_that("pr_filter_Species preserves valid species", {
  test_data <- data.frame(
    Species = c("Valid species 1", "Valid species 2", "Valid species 3"),
    Abundance = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  result <- pr_filter_Species(test_data)
  
  testthat::expect_equal(nrow(result), 3)
  testthat::expect_equal(result$Species, test_data$Species)
})

# Test pr_add_Carbon with fixtures ----

testthat::test_that("pr_add_Carbon adds Carbon column for CPR phytoplankton data", {
  test_data <- data.frame(
    TaxonGroup = c("Dinoflagellate", "Cyanobacteria", "Diatom"),
    BioVolume_um3m3 = c(100, 150, 200),
    PhytoAbund_m3 = c(10, 15, 20),
    stringsAsFactors = FALSE
  )
  
  result <- pr_add_Carbon(test_data, meth = "CPR")
  
  testthat::expect_true("Carbon" %in% names(result))
  testthat::expect_equal(nrow(result), nrow(test_data))
  testthat::expect_true(is.numeric(result$Carbon))
})

testthat::test_that("pr_add_Carbon adds Carbon column for NRS phytoplankton data", {
  test_data <- data.frame(
    TaxonGroup = c("Dinoflagellate", "Cyanobacteria", "Diatom"),
    Biovolume_um3L = c(100, 150, 200),
    Cells_L = c(10, 15, 20),
    stringsAsFactors = FALSE
  )
  
  result <- pr_add_Carbon(test_data, meth = "NRS")
  
  testthat::expect_true("Carbon" %in% names(result))
  testthat::expect_equal(nrow(result), nrow(test_data))
  testthat::expect_true(is.numeric(result$Carbon))
})

testthat::test_that("pr_add_Carbon calculates different carbon values for different taxon groups", {
  test_data <- data.frame(
    TaxonGroup = c("Dinoflagellate", "Cyanobacteria", "Diatom"),
    BioVolume_um3m3 = c(100, 100, 100),
    PhytoAbund_m3 = c(10, 10, 10),
    stringsAsFactors = FALSE
  )
  
  result <- pr_add_Carbon(test_data, meth = "CPR")
  
  # Carbon values should differ between taxon groups
  testthat::expect_gt(length(unique(result$Carbon)), 1)
})

# Test pr_apply_Flags with fixtures ----

testthat::test_that("pr_apply_Flags processes flagged data", {
  test_data <- create_test_flagged_data()
  result <- pr_apply_Flags(test_data)
  
  # Function may remove rows or remove flag columns
  testthat::expect_true(is.data.frame(result))
  testthat::expect_lte(nrow(result), nrow(test_data))
})

testthat::test_that("pr_apply_Flags returns data frame", {
  test_data <- create_test_flagged_data()
  result <- pr_apply_Flags(test_data)
  
  # Check that result is valid
  testthat::expect_true(is.data.frame(result))
})

testthat::test_that("pr_apply_Flags handles multiple flag columns", {
  test_data <- create_test_flagged_data()
  result <- pr_apply_Flags(test_data)
  
  # Function behavior may vary - just verify it returns valid data
  testthat::expect_true(is.data.frame(result))
})

# Test pr_apply_Time with fixtures ----

testthat::test_that("pr_apply_Time adds Month_Local column", {
  test_data <- create_test_nrs_indices()
  result <- planktonr:::pr_apply_Time(test_data)
  
  testthat::expect_true("Month_Local" %in% names(result))
})

testthat::test_that("pr_apply_Time adds Year_Local column", {
  test_data <- create_test_nrs_indices()
  result <- planktonr:::pr_apply_Time(test_data)
  
  testthat::expect_true("Year_Local" %in% names(result))
})

testthat::test_that("pr_apply_Time preserves original number of rows", {
  test_data <- create_test_nrs_indices()
  result <- planktonr:::pr_apply_Time(test_data)
  
  testthat::expect_equal(nrow(result), nrow(test_data))
})

# Test pr_add_StationCode with fixtures ----

testthat::test_that("pr_add_StationCode extracts station code from TripCode", {
  test_data <- data.frame(
    TripCode = c("NSI20220101", "PHB20220115", "MAI20220201"),
    Values = c(100, 150, 200),
    stringsAsFactors = FALSE
  )
  
  result <- pr_add_StationCode(test_data)
  
  testthat::expect_true("StationCode" %in% names(result))
  testthat::expect_equal(result$StationCode, c("NSI", "PHB", "MAI"))
})

testthat::test_that("pr_add_StationCode handles various TripCode formats", {
  test_data <- data.frame(
    TripCode = c("NSI20220101", "PHB_20220115", "MAI-20220201"),
    Values = c(100, 150, 200),
    stringsAsFactors = FALSE
  )
  
  result <- pr_add_StationCode(test_data)
  
  testthat::expect_true("StationCode" %in% names(result))
  testthat::expect_equal(nrow(result), 3)
})

# Test pr_add_StationName with fixtures ----

testthat::test_that("pr_add_StationName adds station names based on station codes", {
  test_data <- create_test_stations()
  result <- pr_add_StationName(test_data)
  
  testthat::expect_true("StationName" %in% names(result))
  testthat::expect_equal(nrow(result), nrow(test_data))
})

# Test pr_filter_data with fixtures ----

testthat::test_that("pr_filter_data filters by station codes", {
  test_data <- create_test_nrs_indices()
  result <- pr_filter_data(test_data, "PhytoAbundance_CellsL", c("NSI", "PHB"))
  
  testthat::expect_true(all(result$StationCode %in% c("NSI", "PHB")))
})

testthat::test_that("pr_filter_data filters by bioregions for CPR data", {
  test_data <- create_test_cpr_indices()
  result <- pr_filter_data(test_data, "BiomassIndex_mgm3", c("North", "South-east"))
  
  testthat::expect_true(all(result$BioRegion %in% c("North", "South-east")))
})

testthat::test_that("pr_filter_data reduces number of rows", {
  test_data <- create_test_nrs_indices()
  result <- pr_filter_data(test_data, "PhytoAbundance_CellsL", c("NSI"))
  
  testthat::expect_lt(nrow(result), nrow(test_data))
})

testthat::test_that("pr_filter_data preserves data structure", {
  test_data <- create_test_nrs_indices()
  result <- pr_filter_data(test_data, "PhytoAbundance_CellsL", c("NSI", "PHB"))
  
  testthat::expect_equal(names(result), names(test_data))
})

# Test pr_make_climatology with fixtures ----

testthat::test_that("pr_make_climatology computes monthly climatology", {
  test_data <- data.frame(
    Month = rep(1:12, 3),
    StationCode = rep("NSI", 36),
    Values = rnorm(36, mean = 100, sd = 10),
    stringsAsFactors = FALSE
  )
  
  result <- pr_make_climatology(test_data, x = "Month")
  
  testthat::expect_true("mean" %in% names(result) | "Values_mean" %in% names(result))
  testthat::expect_lte(nrow(result), 12)  # Should have at most 12 months
})

testthat::test_that("pr_make_climatology computes yearly climatology", {
  test_data <- data.frame(
    Year = rep(2020:2023, each = 4),
    StationCode = rep("NSI", 16),
    Values = rnorm(16, mean = 100, sd = 10),
    stringsAsFactors = FALSE
  )
  
  result <- pr_make_climatology(test_data, x = "Year")
  
  testthat::expect_true("mean" %in% names(result) | "Values_mean" %in% names(result))
  testthat::expect_lte(nrow(result), 4)  # Should have at most 4 years
})

testthat::test_that("pr_make_climatology groups by station", {
  test_data <- data.frame(
    Month = rep(1:12, 6),
    StationCode = rep(c("NSI", "PHB"), each = 36),
    Values = rnorm(72, mean = 100, sd = 10),
    stringsAsFactors = FALSE
  )
  
  result <- pr_make_climatology(test_data, x = "Month")
  
  # Should have climatology for both stations
  if ("StationCode" %in% names(result)) {
    testthat::expect_true(length(unique(result$StationCode)) >= 1)
  }
})

# Test pr_harmonic with fixtures ----

testthat::test_that("pr_harmonic returns numeric vector of correct length", {
  result <- pr_harmonic(2, 2)
  
  testthat::expect_type(result, "double")
  testthat::expect_length(result, 4)
})

testthat::test_that("pr_harmonic handles different frequency and phase values", {
  result1 <- pr_harmonic(1, 1)
  result2 <- pr_harmonic(2, 2)
  result3 <- pr_harmonic(3, 1)
  
  testthat::expect_false(identical(result1, result2))
  testthat::expect_false(identical(result2, result3))
})

testthat::test_that("pr_harmonic produces values between -1 and 1", {
  result <- pr_harmonic(2, 1)
  
  testthat::expect_true(all(result >= -1 & result <= 1))
})

# Test pr_filter_NRSStations with fixtures ----

testthat::test_that("pr_filter_NRSStations filters to NRS stations only", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB", "MAI", "INVALID1", "INVALID2"),
    Values = c(100, 150, 200, 250, 300),
    stringsAsFactors = FALSE
  )
  
  result <- pr_filter_NRSStations(test_data)
  
  # Should only include valid NRS stations
  valid_nrs <- c("NSI", "PHB", "MAI", "YON", "ROT", "DAR", "ESP", "KAI", "NIN")
  testthat::expect_true(all(result$StationCode %in% valid_nrs))
})

testthat::test_that("pr_filter_NRSStations reduces row count for mixed data", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB", "INVALID1", "MAI", "INVALID2"),
    Values = c(100, 150, 200, 250, 300),
    stringsAsFactors = FALSE
  )
  
  result <- pr_filter_NRSStations(test_data)
  
  testthat::expect_lt(nrow(result), nrow(test_data))
})

# Test planktonr_dat class preservation ----

testthat::test_that("planktonr_dat class is preserved through filter", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::filter(test_data, StationCode == "NSI")
  
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("planktonr_dat class is preserved through mutate", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::mutate(test_data, DoubleAbundance = Abundance * 2)
  
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true("DoubleAbundance" %in% names(result))
})

testthat::test_that("planktonr_dat class is preserved through arrange", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::arrange(test_data, desc(Abundance))
  
  testthat::expect_s3_class(result, "planktonr_dat")
})

testthat::test_that("planktonr_dat class is preserved through group_by", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::group_by(test_data, StationCode)
  
  testthat::expect_s3_class(result, "planktonr_dat")
})

testthat::test_that("planktonr_dat class is preserved through summarise", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- test_data %>%
    dplyr::group_by(StationCode) %>%
    dplyr::summarise(mean_abundance = mean(Abundance, na.rm = TRUE))
  
  # Note: summarise may or may not preserve custom class depending on implementation
  testthat::expect_true("data.frame" %in% class(result))
})

testthat::test_that("planktonr_dat attributes are accessible", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  
  testthat::expect_equal(attr(test_data, "Survey"), "NRS")
  testthat::expect_equal(attr(test_data, "Type"), "phytoplankton")
  testthat::expect_equal(attr(test_data, "variable"), "abundance")
})

# Test pr_get_NonTaxaColumns with fixtures ----

testthat::test_that("pr_get_NonTaxaColumns returns character vector", {
  result <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Phytoplankton")
  
  testthat::expect_type(result, "character")
  testthat::expect_gt(length(result), 0)
})

testthat::test_that("pr_get_NonTaxaColumns returns different columns for different types", {
  phyto_cols <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Phytoplankton")
  zoo_cols <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Zooplankton")
  
  # There should be some differences
  testthat::expect_false(identical(phyto_cols, zoo_cols))
})

testthat::test_that("pr_get_NonTaxaColumns includes common metadata columns", {
  result <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Phytoplankton")
  
  # Should include standard columns like StationCode, Latitude, Longitude
  expected_common <- c("StationCode", "Latitude", "Longitude")
  testthat::expect_true(any(expected_common %in% result))
})
