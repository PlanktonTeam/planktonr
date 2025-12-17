# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_get_Indices returns data frame for NRS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Phytoplankton")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
  
  # Check expected columns
  expected_cols <- c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz",
                     "Latitude", "Longitude", "StationName", "StationCode", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check expected parameters
  expected_params <- c("PhytoBiomassCarbon_pgL", "PhytoAbundance_CellsL", "DiatomDinoflagellateRatio",
                       "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity", "PhytoEvenness")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))
  
  # Check data types
  testthat::expect_true(is.numeric(result$Values))
  testthat::expect_true(inherits(result$SampleTime_Local, c("POSIXct", "Date")))
})

testthat::test_that("pr_get_Indices returns data frame for NRS Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Zooplankton")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Zooplankton")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
  
  # Check expected parameters
  expected_params <- c("Biomass_mgm3", "AshFreeBiomass_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3",
                       "AvgTotalLengthCopepod_mm", "NoCopepodSpecies_Sample", "ShannonCopepodDiversity")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))
})

testthat::test_that("pr_get_Indices returns data frame for NRS Water", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Water")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
  
  # Check expected parameters
  expected_params <- c("Secchi_m", "MLDtemp_m", "MLDsal_m", "DCM_m",
                       "CTDTemperature_degC", "CTDSalinity_PSU", "CTDChlaF_mgm3")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))
})

testthat::test_that("pr_get_Indices returns data frame for CPR Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "CPR", Type = "Phytoplankton")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Phytoplankton")
  testthat::expect_equal(attr(result, "Survey"), "CPR")
  
  # Check expected columns (CPR has BioRegion instead of StationCode)
  expected_cols <- c("SampleTime_Local", "Year_Local", "Month_Local", "BioRegion",
                     "Latitude", "Longitude", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check expected parameters
  expected_params <- c("PCI", "PhytoBiomassCarbon_pgm3", "PhytoAbundance_Cellsm3", "DiatomDinoflagellateRatio",
                       "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))
})

testthat::test_that("pr_get_Indices returns data frame for CPR Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "CPR", Type = "Zooplankton")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Zooplankton")
  testthat::expect_equal(attr(result, "Survey"), "CPR")
  
  # Check expected parameters (CPR uses BiomassIndex instead of Biomass)
  expected_params <- c("BiomassIndex_mgm3", "ZoopAbundance_m3", "CopeAbundance_m3",
                       "AvgTotalLengthCopepod_mm", "NoCopepodSpecies_Sample")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))
})

testthat::test_that("pr_get_Indices returns data frame for CPR Water", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "CPR", Type = "Water")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "CPR")
  
  # CPR Water only has PCI parameter
  params <- unique(result$Parameters)
  testthat::expect_true("PCI" %in% params)
})

testthat::test_that("pr_get_Indices returns data frame for SOTS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "SOTS", Type = "Phytoplankton")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Phytoplankton")
  testthat::expect_equal(attr(result, "Survey"), "SOTS")
  
  # Check expected columns
  expected_cols <- c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz",
                     "Latitude", "Longitude", "StationName", "StationCode", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check StationCode is SOTS
  testthat::expect_true(all(result$StationCode == "SOTS"))
  
  # Check expected parameters (same as NRS Phytoplankton)
  expected_params <- c("PhytoBiomassCarbon_pgL", "PhytoAbundance_CellsL", "DiatomDinoflagellateRatio",
                       "AvgCellVol_um3", "NoPhytoSpecies_Sample", "ShannonPhytoDiversity")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))
})

testthat::test_that("pr_get_Indices filters Port Hacking 4 from NRS data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton")
  
  # Should not contain Port Hacking 4
  testthat::expect_false("Port Hacking 4" %in% result$StationName)
})

testthat::test_that("pr_get_Indices applies bioregions to CPR data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "CPR", Type = "Zooplankton")
  
  # Should have BioRegion column
  testthat::expect_true("BioRegion" %in% names(result))
  testthat::expect_true(all(!is.na(result$BioRegion)))
})

testthat::test_that("pr_get_Indices accepts near_dist_km parameter for CPR", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Should work without error
  testthat::expect_no_error(pr_get_Indices(Survey = "CPR", Type = "Phytoplankton", near_dist_km = 250))
})

testthat::test_that("pr_filter_data filters CPR Zooplankton indices by bioregion", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
    pr_filter_data("BiomassIndex_mgm3", c("North", "South-west"))
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  
  # Should only contain specified bioregions
  testthat::expect_true(all(result$BioRegion %in% c("North", "South-west")))
  
  # Should only contain specified parameter
  testthat::expect_true(all(result$Parameters == "BiomassIndex_mgm3"))
})

testthat::test_that("pr_filter_data filters NRS Phytoplankton indices by station", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
    pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB"))
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  
  # Should only contain specified stations
  testthat::expect_true(all(result$StationCode %in% c("NSI", "PHB")))
  
  # Should only contain specified parameter
  testthat::expect_true(all(result$Parameters == "PhytoBiomassCarbon_pgL"))
})

testthat::test_that("pr_filter_data accepts multiple parameters", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
    pr_filter_data(c("PhytoBiomassCarbon_pgL", "PhytoAbundance_CellsL"), "NSI")
  
  testthat::expect_s3_class(result, "data.frame")
  
  # Should contain both parameters
  params <- unique(result$Parameters)
  testthat::expect_true(all(c("PhytoBiomassCarbon_pgL", "PhytoAbundance_CellsL") %in% params))
})

testthat::test_that("pr_filter_data accepts multiple stations/regions", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Indices(Survey = "NRS", Type = "Water") %>%
    pr_filter_data("Secchi_m", c("NSI", "PHB", "MAI"))
  
  testthat::expect_s3_class(result, "data.frame")
  
  # Should contain all specified stations
  stations <- unique(result$StationCode)
  testthat::expect_true(all(stations %in% c("NSI", "PHB", "MAI")))
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_make_climatology computes monthly climatology correctly", {
  test_data <- data.frame(
    Month = rep(1:12, 10),
    StationCode = "NSI",
    Values = runif(120, min = 0, max = 10)
  )
  
  result <- pr_make_climatology(test_data, "Month")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 12)  # One row per month
  
  # Check expected columns
  expected_cols <- c("Month", "StationCode", "mean", "N", "sd", "se")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check that N is correct (10 values per month)
  testthat::expect_true(all(result$N == 10))
  
  # Check that mean values are numeric and reasonable
  testthat::expect_true(is.numeric(result$mean))
  testthat::expect_true(all(result$mean >= 0 & result$mean <= 10))
  
  # Check that sd and se are calculated
  testthat::expect_true(is.numeric(result$sd))
  testthat::expect_true(is.numeric(result$se))
  testthat::expect_true(all(!is.na(result$sd)))
})

testthat::test_that("pr_make_climatology handles different grouping variables", {
  test_data <- data.frame(
    Month = rep(1:12, 5),
    Year = rep(2018:2022, each = 12),
    StationCode = rep("NSI", 60),
    Values = runif(60, min = 0, max = 100)
  )
  
  # Group by Month
  clim_month <- pr_make_climatology(test_data, "Month")
  testthat::expect_equal(nrow(clim_month), 12)
  
  # Group by Year
  clim_year <- pr_make_climatology(test_data, "Year")
  testthat::expect_equal(nrow(clim_year), 5)
})

testthat::test_that("pr_make_climatology handles multiple stations", {
  test_data <- data.frame(
    Month = rep(1:12, 6),
    StationCode = rep(c("NSI", "PHB", "MAI"), each = 24),
    Values = runif(72, min = 0, max = 100)
  )
  
  result <- pr_make_climatology(test_data, "Month")
  
  # Should have 12 months x 3 stations = 36 rows
  testthat::expect_equal(nrow(result), 36)
  
  # Should have all three stations
  stations <- unique(result$StationCode)
  testthat::expect_equal(length(stations), 3)
  testthat::expect_true(all(c("NSI", "PHB", "MAI") %in% stations))
})

testthat::test_that("pr_make_climatology handles NA values correctly", {
  test_data <- data.frame(
    Month = rep(1:12, 10),
    StationCode = "NSI",
    Values = c(runif(100, min = 0, max = 10), rep(NA, 20))
  )
  
  result <- pr_make_climatology(test_data, "Month")
  
  # Should compute means ignoring NAs
  testthat::expect_true(all(!is.na(result$mean)))
  testthat::expect_true(all(result$mean >= 0))
})

testthat::test_that("pr_make_climatology computes standard error correctly", {
  test_data <- data.frame(
    Month = rep(1, 10),
    StationCode = "NSI",
    Values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )
  
  result <- pr_make_climatology(test_data, "Month")
  
  # Verify se = sd / sqrt(N)
  expected_se <- result$sd / sqrt(result$N)
  testthat::expect_equal(result$se, expected_se)
})

testthat::test_that("pr_filter_data works with mock indices data", {
  mock_indices <- data.frame(
    SampleDateUTC = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
    StationCode = rep(c("NSI", "PHB", "MAI"), 4),
    BioRegion = rep(c("NSI", "PHB", "MAI"), 4),
    Parameters = "BiomassIndex",
    Values = runif(12, 0, 100),
    stringsAsFactors = FALSE
  )
  mock_indices <- planktonr_dat(mock_indices, Type = "Zooplankton", Survey = "NRS", Variable = "test")
  
  filtered <- pr_filter_data(mock_indices, "BiomassIndex", c("NSI", "PHB"))
  
  testthat::expect_s3_class(filtered, "planktonr_dat")
  testthat::expect_true(all(filtered$StationCode %in% c("NSI", "PHB")))
  testthat::expect_false("MAI" %in% filtered$StationCode)
})

testthat::test_that("pr_filter_data works with CPR-style data (BioRegion)", {
  mock_cpr <- data.frame(
    SampleTime_Local = seq.POSIXt(as.POSIXct("2020-01-01"), as.POSIXct("2020-12-01"), length.out = 12),
    BioRegion = rep(c("North", "South-west", "South-east"), 4),
    Parameters = rep(c("PCI", "BiomassIndex_mgm3"), 6),
    Values = runif(12, 0, 100),
    stringsAsFactors = FALSE
  )
  mock_cpr <- planktonr_dat(mock_cpr, Type = "Zooplankton", Survey = "CPR")
  
  filtered <- pr_filter_data(mock_cpr, "BiomassIndex_mgm3", c("North", "South-west"))
  
  testthat::expect_s3_class(filtered, "planktonr_dat")
  testthat::expect_true(all(filtered$BioRegion %in% c("North", "South-west")))
  testthat::expect_true(all(filtered$Parameters == "BiomassIndex_mgm3"))
})

testthat::test_that("pr_filter_data handles single parameter and single station", {
  mock_indices <- data.frame(
    Month = 1:12,
    StationCode = rep(c("NSI", "PHB"), 6),
    StationName = rep(c("North Stradbroke Island", "Port Hacking"), 6),
    Parameters = rep(c("Param1", "Param2"), 6),
    Values = runif(12, 0, 100)
  )
  mock_indices <- planktonr_dat(mock_indices, Type = "Phytoplankton", Survey = "NRS")
  
  filtered <- pr_filter_data(mock_indices, "Param1", "NSI")
  
  testthat::expect_true(all(filtered$Parameters == "Param1"))
  testthat::expect_true(all(filtered$StationCode == "NSI"))
  testthat::expect_true(nrow(filtered) < nrow(mock_indices))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_get_Indices validates Survey parameter type", {
  testthat::expect_error(
    pr_get_Indices(Survey = 123, Type = "Phytoplankton"),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_Indices(Survey = c("NRS", "CPR"), Type = "Phytoplankton"),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_Indices(Survey = NULL, Type = "Phytoplankton"),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_Indices validates Survey parameter value", {
  testthat::expect_error(
    pr_get_Indices(Survey = "INVALID", Type = "Phytoplankton"),
    regexp = "must be one of 'NRS', 'CPR', or 'SOTS'"
  )
  
  testthat::expect_error(
    pr_get_Indices(Survey = "nrs", Type = "Phytoplankton"),
    regexp = "must be one of 'NRS', 'CPR', or 'SOTS'"
  )
})

testthat::test_that("pr_get_Indices validates Type parameter type", {
  testthat::expect_error(
    pr_get_Indices(Survey = "NRS", Type = 123),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_Indices(Survey = "NRS", Type = c("Phytoplankton", "Zooplankton")),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_Indices validates Type parameter value", {
  testthat::expect_error(
    pr_get_Indices(Survey = "NRS", Type = "INVALID"),
    regexp = "must be one of 'Phytoplankton', 'Zooplankton', or 'Water'"
  )
})

testthat::test_that("pr_get_Indices rejects SOTS Zooplankton combination", {
  testthat::expect_error(
    pr_get_Indices(Survey = "SOTS", Type = "Zooplankton"),
    regexp = "There is no zooplankton data for SOTS"
  )
})

testthat::test_that("pr_filter_data validates df parameter", {
  testthat::expect_error(
    pr_filter_data("not_a_dataframe", "Param", "Station"),
    regexp = "'dat' must be a data frame"
  )
  
  testthat::expect_error(
    pr_filter_data(list(a = 1, b = 2), "Param", "Station"),
    regexp = "'dat' must be a data frame"
  )
})

testthat::test_that("pr_filter_data validates planktonr_dat class", {
  df <- data.frame(Parameters = "Test", Values = 1:10, StationCode = "NSI")
  
  testthat::expect_error(
    pr_filter_data(df, "Test", "NSI"),
    regexp = "'dat' must be a planktonr_dat object"
  )
})

testthat::test_that("pr_filter_data validates empty data frame", {
  empty_df <- data.frame(Parameters = character(), Values = numeric(), StationCode = character())
  empty_df <- planktonr_dat(empty_df, Type = "Phytoplankton", Survey = "NRS")
  
  testthat::expect_error(
    pr_filter_data(empty_df, "Param", "NSI"),
    regexp = "data frame 'dat' is empty"
  )
})

testthat::test_that("pr_filter_data validates Parameter parameter", {
  mock_df <- data.frame(
    Parameters = rep("Param1", 10),
    Values = 1:10,
    StationCode = "NSI"
  )
  mock_df <- planktonr_dat(mock_df, Type = "Phytoplankton", Survey = "NRS")
  
  testthat::expect_error(
    pr_filter_data(mock_df, 123, "NSI"),
    regexp = "'Parameter' must be a character string"
  )
  
  testthat::expect_error(
    pr_filter_data(mock_df, NULL, "NSI"),
    regexp = "'Parameter' must be a character string"
  )
})

testthat::test_that("pr_filter_data validates StationRegion parameter", {
  mock_df <- data.frame(
    Parameters = rep("Param1", 10),
    Values = 1:10,
    StationCode = "NSI"
  )
  mock_df <- planktonr_dat(mock_df, Type = "Phytoplankton", Survey = "NRS")
  
  testthat::expect_error(
    pr_filter_data(mock_df, "Param1", 123),
    regexp = "'StationRegion' must be a character string"
  )
  
  testthat::expect_error(
    pr_filter_data(mock_df, "Param1", NULL),
    regexp = "'StationRegion' must be a character string"
  )
})

testthat::test_that("pr_make_climatology handles missing grouping variable", {
  test_data <- data.frame(
    StationCode = "NSI",
    Values = 1:10
  )
  
  # Should error because NonexistentColumn is not in the data
  testthat::expect_error(
    pr_make_climatology(test_data, "NonexistentColumn"),
    regexp = "Can't subset elements that don't exist|doesn't exist"
  )
})

testthat::test_that("pr_make_climatology handles missing Values column", {
  test_data <- data.frame(
    Month = 1:12,
    StationCode = "NSI"
  )
  
  # Should error because Values column is missing
  testthat::expect_error(
    pr_make_climatology(test_data, "Month"),
    regexp = "Column `Values` not found|object 'Values' not found"
  )
})

testthat::test_that("pr_make_climatology handles missing StationCode column", {
  test_data <- data.frame(
    Month = 1:12,
    Values = runif(12)
  )
  
  # Should error because StationCode column is missing
  testthat::expect_error(
    pr_make_climatology(test_data, "Month"),
    regexp = "Can't subset elements that don't exist|StationCode.*doesn't exist"
  )
})

testthat::test_that("pr_get_Indices accepts valid Survey and Type combinations", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # All valid NRS combinations
  testthat::expect_no_error(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton"))
  testthat::expect_no_error(pr_get_Indices(Survey = "NRS", Type = "Zooplankton"))
  testthat::expect_no_error(pr_get_Indices(Survey = "NRS", Type = "Water"))
  
  # All valid CPR combinations
  testthat::expect_no_error(pr_get_Indices(Survey = "CPR", Type = "Phytoplankton"))
  testthat::expect_no_error(pr_get_Indices(Survey = "CPR", Type = "Zooplankton"))
  testthat::expect_no_error(pr_get_Indices(Survey = "CPR", Type = "Water"))
  
  # Valid SOTS combinations
  testthat::expect_no_error(pr_get_Indices(Survey = "SOTS", Type = "Phytoplankton"))
  testthat::expect_no_error(pr_get_Indices(Survey = "SOTS", Type = "Water"))
})
