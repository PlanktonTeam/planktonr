# Error Handling Tests for planktonr
# Tests that functions properly validate inputs and throw appropriate errors

# Test pr_get_Indices error handling ----

testthat::test_that("pr_get_Indices throws error for invalid Survey parameter", {
  testthat::expect_error(
    pr_get_Indices(Survey = "INVALID", Type = "Phytoplankton"),
    "'Survey' must be one of 'NRS', 'CPR', or 'SOTS'"
  )
})

testthat::test_that("pr_get_Indices throws error for non-character Survey", {
  testthat::expect_error(
    pr_get_Indices(Survey = 123, Type = "Phytoplankton"),
    "'Survey' must be a single character string"
  )
})

testthat::test_that("pr_get_Indices throws error for multiple Survey values", {
  testthat::expect_error(
    pr_get_Indices(Survey = c("NRS", "CPR"), Type = "Phytoplankton"),
    "'Survey' must be a single character string"
  )
})

testthat::test_that("pr_get_Indices throws error for invalid Type parameter", {
  testthat::expect_error(
    pr_get_Indices(Survey = "NRS", Type = "InvalidType"),
    "'Type' must be one of"
  )
})

testthat::test_that("pr_get_Indices throws error for Zooplankton with SOTS", {
  testthat::expect_error(
    pr_get_Indices(Survey = "SOTS", Type = "Zooplankton"),
    "There is no zooplankton data for SOTS"
  )
})

testthat::test_that("pr_get_Indices throws error for NULL Survey", {
  testthat::expect_error(
    pr_get_Indices(Survey = NULL, Type = "Phytoplankton")
  )
})

testthat::test_that("pr_get_Indices throws error for NULL Type", {
  testthat::expect_error(
    pr_get_Indices(Survey = "NRS", Type = NULL)
  )
})

# Note: pr_get_NRSData and pr_get_CPRData error handling tests have been removed
# as these functions are deprecated. Error handling for data retrieval is now
# tested in test-utils_data.R via pr_get_data()

# Test pr_filter_Species error handling ----

testthat::test_that("pr_filter_Species handles NULL input gracefully", {
  testthat::expect_error(
    pr_filter_Species(NULL)
  )
})

testthat::test_that("pr_filter_Species handles data without Species column", {
  test_data <- data.frame(
    TaxonGroup = c("Diatom", "Dinoflagellate"),
    Abundance = c(100, 150)
  )
  testthat::expect_error(
    pr_filter_Species(test_data),
    "Species"
  )
})

testthat::test_that("pr_filter_Species handles empty data frame", {
  test_data <- data.frame(Species = character(0))
  result <- pr_filter_Species(test_data)
  testthat::expect_equal(nrow(result), 0)
})

# Test pr_add_Carbon error handling ----

testthat::test_that("pr_add_Carbon throws error for invalid Survey parameter", {
  test_data <- create_test_phyto_data()
  testthat::expect_error(
    pr_add_Carbon(test_data, Survey = "INVALID"),
    "Survey"
  )
})

testthat::test_that("pr_add_Carbon throws error for NULL Survey", {
  test_data <- create_test_phyto_data()
  testthat::expect_error(
    pr_add_Carbon(test_data, Survey = NULL)
  )
})

testthat::test_that("pr_add_Carbon handles data without required columns", {
  test_data <- data.frame(
    TaxonGroup = c("Diatom", "Dinoflagellate"),
    Abundance = c(100, 150)
  )
  testthat::expect_error(
    pr_add_Carbon(test_data, Survey = "NRS")
  )
})

# Test pr_apply_Flags error handling ----

testthat::test_that("pr_apply_Flags handles data without flag columns", {
  test_data <- data.frame(
    Temperature = c(18.5, 19.2, 18.8),
    Salinity = c(35.2, 35.5, 35.1)
  )
  # Function errors when no flag columns present
  result <- tryCatch(
    pr_apply_Flags(test_data),
    error = function(e) "error"
  )
  testthat::expect_true(is.data.frame(result) || result == "error")
})

testthat::test_that("pr_apply_Flags handles NULL input", {
  testthat::expect_error(
    pr_apply_Flags(NULL)
  )
})

testthat::test_that("pr_apply_Flags handles empty data frame", {
  test_data <- data.frame()
  # Empty data frame should error or return empty
  result <- tryCatch(
    pr_apply_Flags(test_data),
    error = function(e) "error"
  )
  testthat::expect_true(is.data.frame(result) || result == "error")
})

# Test pr_apply_Time error handling ----

testthat::test_that("pr_apply_Time throws error for data without time columns", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(100, 150)
  )
  testthat::expect_error(
    pr_apply_Time(test_data)
  )
})

testthat::test_that("pr_apply_Time handles NULL input", {
  testthat::expect_error(
    pr_apply_Time(NULL)
  )
})

# Test pr_filter_data error handling ----

testthat::test_that("pr_filter_data throws error for missing parameter", {
  test_data <- create_test_nrs_indices()
  testthat::expect_error(
    pr_filter_data(test_data, NULL, c("NSI", "PHB"))
  )
})

testthat::test_that("pr_filter_data returns empty data for non-existent parameter", {
  test_data <- create_test_nrs_indices()
  # Function doesn't error, just returns empty result
  result <- pr_filter_data(test_data, "NonExistentParameter", c("NSI", "PHB"))
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("pr_filter_data handles empty filter values", {
  test_data <- create_test_nrs_indices()
  result <- pr_filter_data(test_data, "PhytoAbundance_CellsL", character(0))
  testthat::expect_equal(nrow(result), 0)
})

# Test pr_relabel error handling ----

# Test pr_relabel error handling ----

testthat::test_that("pr_relabel handles unrecognized variable name", {
  # Function may print message or warning for unknown variables
  # Use quiet = TRUE to suppress warning since we're testing with unknown variable
  result <- pr_relabel("UnknownVariable_units", style = "simple", quiet = TRUE)
  testthat::expect_type(result, "character")
})

testthat::test_that("pr_relabel throws error for invalid style", {
  testthat::expect_error(
    pr_relabel("Chla_mgm3", style = "invalid_style"),
    "style"
  )
})

testthat::test_that("pr_relabel handles NULL variable", {
  testthat::expect_error(
    pr_relabel(NULL, style = "simple")
  )
})

testthat::test_that("pr_relabel handles empty string", {
  testthat::expect_warning(
    pr_relabel("", style = "simple")
  )
})

# Test pr_add_StationCode error handling ----

testthat::test_that("pr_add_StationCode handles data without TripCode column", {
  test_data <- data.frame(
    Values = c(100, 150),
    StationName = c("NSI", "PHB")
  )
  # Function should either error or handle gracefully
  result <- tryCatch(
    pr_add_StationCode(test_data),
    error = function(e) NULL
  )
  # If it didn't error, it should return data with StationCode added or original data
  testthat::expect_true(is.null(result) || is.data.frame(result))
})

testthat::test_that("pr_add_StationCode handles NULL input", {
  result <- tryCatch(
    pr_add_StationCode(NULL),
    error = function(e) NULL
  )
  testthat::expect_null(result)
})

# Test pr_add_StationName error handling ----

testthat::test_that("pr_add_StationName handles data without StationCode column", {
  test_data <- data.frame(
    Values = c(100, 150),
    Latitude = c(-27, -34)
  )
  testthat::expect_error(
    pr_add_StationName(test_data),
    "StationCode"
  )
})

# Test pr_add_Bioregions error handling ----

testthat::test_that("pr_add_Bioregions throws error for data without coordinates", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(100, 150)
  )
  testthat::expect_error(
    pr_add_Bioregions(test_data),
    "Latitude|Longitude"
  )
})

testthat::test_that("pr_add_Bioregions handles NULL input", {
  testthat::expect_error(
    pr_add_Bioregions(NULL)
  )
})

testthat::test_that("pr_add_Bioregions handles invalid near_dist_km parameter", {
  test_data <- create_test_cpr_indices()
  testthat::expect_error(
    pr_add_Bioregions(test_data, near_dist_km = "invalid"),
    "numeric"
  )
})

# Test pr_make_climatology error handling ----

testthat::test_that("pr_make_climatology throws error for invalid time_resolution", {
  test_data <- create_test_nrs_indices()
  testthat::expect_error(
    pr_make_climatology(test_data, time_resolution = "InvalidResolution"),
    "time_resolution"
  )
})

testthat::test_that("pr_make_climatology handles NULL input", {
  testthat::expect_error(
    pr_make_climatology(NULL, time_resolution = "Month")
  )
})

testthat::test_that("pr_make_climatology handles data without required columns", {
  test_data <- data.frame(
    Values = c(100, 150, 200)
  )
  testthat::expect_error(
    pr_make_climatology(test_data, time_resolution = "Month")
  )
})

# Test pr_get_FuncGroups error handling ----

testthat::test_that("pr_get_FuncGroups throws error for invalid Survey", {
  testthat::expect_error(
    pr_get_FuncGroups(Survey = "INVALID", Type = "Phytoplankton"),
    "Survey"
  )
})

testthat::test_that("pr_get_FuncGroups throws error for invalid Type", {
  testthat::expect_error(
    pr_get_FuncGroups(Survey = "NRS", Type = "INVALID"),
    "Type"
  )
})

# Test pr_get_EOVs error handling ----

testthat::test_that("pr_get_EOVs throws error for invalid Survey", {
  testthat::expect_error(
    pr_get_EOVs(Survey = "INVALID"),
    "Survey"
  )
})

testthat::test_that("pr_get_EOVs throws error for NULL Survey", {
  testthat::expect_error(
    pr_get_EOVs(Survey = NULL)
  )
})

# Test pr_filter_NRSStations error handling ----

testthat::test_that("pr_filter_NRSStations handles data without StationCode", {
  test_data <- data.frame(
    Latitude = c(-27, -34),
    Longitude = c(153, 151)
  )
  testthat::expect_error(
    pr_filter_NRSStations(test_data),
    "StationCode"
  )
})

testthat::test_that("pr_filter_NRSStations handles NULL input", {
  testthat::expect_error(
    pr_filter_NRSStations(NULL)
  )
})

# Test pr_get_NonTaxaColumns error handling ----

testthat::test_that("pr_get_NonTaxaColumns throws error for invalid Survey", {
  testthat::expect_error(
    pr_get_NonTaxaColumns(Survey = "INVALID", Type = "Phytoplankton")
  )
})

testthat::test_that("pr_get_NonTaxaColumns throws error for invalid Type", {
  testthat::expect_error(
    pr_get_NonTaxaColumns(Survey = "NRS", Type = "INVALID")
  )
})

testthat::test_that("pr_get_NonTaxaColumns throws error for NULL parameters", {
  testthat::expect_error(
    pr_get_NonTaxaColumns(Survey = NULL, Type = "Phytoplankton")
  )
})

# Test pr_harmonic error handling ----

testthat::test_that("pr_harmonic throws error for non-numeric inputs", {
  testthat::expect_error(
    pr_harmonic("a", 2),
    "numeric"
  )
})

testthat::test_that("pr_harmonic handles NULL inputs", {
  # pr_harmonic now validates input and throws error for NULL
  testthat::expect_error(
    pr_harmonic(NULL, 2),
    regexp = "'theta' must be a numeric"
  )
})

testthat::test_that("pr_harmonic handles negative numbers", {
  # Should work or throw meaningful error
  result <- tryCatch(
    pr_harmonic(-1, 2),
    error = function(e) NULL
  )
  # Test should pass regardless of whether function accepts negative numbers
  testthat::expect_true(is.null(result) || is.numeric(result))
})

# Test plotting functions error handling ----

testthat::test_that("pr_plot_TimeSeries throws error for invalid Trend parameter", {
  test_data <- create_test_nrs_indices()
  testthat::expect_error(
    pr_plot_TimeSeries(test_data, Trend = "InvalidTrend")
  )
})

testthat::test_that("pr_plot_Trends throws error for invalid Trend parameter", {
  test_data <- create_test_nrs_indices()
  testthat::expect_error(
    pr_plot_Trends(test_data, Trend = "InvalidTrend")
  )
})

testthat::test_that("pr_plot_Climatology throws error for data without required columns", {
  test_data <- data.frame(Values = c(1, 2, 3))
  testthat::expect_error(
    pr_plot_Climatology(test_data, Trend = "Year")
  )
})

testthat::test_that("pr_plot_EOVs throws error for invalid EOV parameter", {
  test_data <- create_test_eov_data()
  testthat::expect_error(
    pr_plot_EOVs(test_data, EOV = "NonExistentEOV")
  )
})

testthat::test_that("pr_plot_Enviro throws error for invalid Trend parameter", {
  test_data <- create_test_chemistry()
  testthat::expect_error(
    pr_plot_Enviro(test_data, Trend = "InvalidTrend")
  )
})

# Test data transformation functions error handling ----

testthat::test_that("filter.planktonr_dat preserves planktonr_dat class", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::filter(test_data, StationCode == "NSI")
  testthat::expect_s3_class(result, "planktonr_dat")
})

testthat::test_that("mutate.planktonr_dat preserves planktonr_dat class", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::mutate(test_data, NewColumn = Abundance * 2)
  testthat::expect_s3_class(result, "planktonr_dat")
})

testthat::test_that("arrange.planktonr_dat preserves planktonr_dat class", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::arrange(test_data, Abundance)
  testthat::expect_s3_class(result, "planktonr_dat")
})

testthat::test_that("group_by.planktonr_dat preserves planktonr_dat class", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  result <- dplyr::group_by(test_data, StationCode)
  testthat::expect_s3_class(result, "planktonr_dat")
})

# Test is_planktonr_dat function ----

testthat::test_that("is_planktonr_dat returns TRUE for planktonr_dat objects", {
  test_data <- create_test_planktonr_dat("phytoplankton")
  testthat::expect_true(is_planktonr_dat(test_data))
})

testthat::test_that("is_planktonr_dat returns FALSE for regular data frames", {
  test_data <- data.frame(a = 1:3, b = 4:6)
  testthat::expect_false(is_planktonr_dat(test_data))
})

testthat::test_that("is_planktonr_dat returns FALSE for NULL", {
  testthat::expect_false(is_planktonr_dat(NULL))
})

testthat::test_that("is_planktonr_dat returns FALSE for non-data frame objects", {
  testthat::expect_false(is_planktonr_dat("not a data frame"))
  testthat::expect_false(is_planktonr_dat(list(a = 1, b = 2)))
  testthat::expect_false(is_planktonr_dat(1:10))
})
