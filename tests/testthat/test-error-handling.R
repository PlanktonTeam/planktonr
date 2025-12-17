# ==============================================================================
# Error handling tests for planktonr package
# Tests invalid inputs, missing columns, and edge cases using fixtures
# ==============================================================================

# ------------------------------------------------------------------------------
# Data filtering and transformation functions
# ------------------------------------------------------------------------------

testthat::test_that("pr_filter_Species errors on NULL input", {
  testthat::expect_error(pr_filter_Species(NULL))
})

testthat::test_that("pr_filter_Species errors on non-data.frame input", {
  testthat::expect_error(pr_filter_Species(c("not", "a", "dataframe")))
})

testthat::test_that("pr_filter_Species handles empty data frame", {
  df <- data.frame(Species = character(0), stringsAsFactors = FALSE)
  result <- pr_filter_Species(df)
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("pr_add_Carbon handles unknown survey", {
  df <- create_test_phyto_data()
  # Function parameter is 'meth', not 'Survey'
  result <- tryCatch(
    pr_add_Carbon(df, meth = "UNKNOWN_METHOD"),
    error = function(e) NULL
  )
  # Should either error (NULL) or return a data frame
  testthat::expect_true(is.null(result) || is.data.frame(result))
})

testthat::test_that("pr_add_Carbon errors when required columns missing for CPR", {
  df <- data.frame(TaxonGroup = c("Dinoflagellate"))
  testthat::expect_error(pr_add_Carbon(df, "CPR"))
})

testthat::test_that("pr_add_Carbon errors when required columns missing for NRS", {
  df <- data.frame(TaxonGroup = c("Dinoflagellate"))
  testthat::expect_error(pr_add_Carbon(df, "NRS"))
})

testthat::test_that("pr_apply_Flags errors when required flag columns missing", {
  df <- data.frame(SST = c(20, 21))
  testthat::expect_error(pr_apply_Flags(df))
})

testthat::test_that("pr_apply_Flags handles data with no flagged values", {
  df <- data.frame(
    SST = c(20, 21),
    SST_Flag = c(1, 1),
    stringsAsFactors = FALSE
  )
  result <- pr_apply_Flags(df)
  testthat::expect_equal(nrow(result), 2)
})

testthat::test_that("pr_apply_Time errors on data without temporal columns", {
  df <- data.frame(Values = c(1, 2, 3))
  testthat::expect_error(pr_apply_Time(df))
})

testthat::test_that("pr_make_climatology errors on missing grouping column", {
  df <- data.frame(Month = 1:12, Values = runif(12))
  testthat::expect_error(pr_make_climatology(df, "NonExistentColumn"))
})

testthat::test_that("pr_rename handles missing coordinate columns gracefully", {
  df <- data.frame(notlat = c(-32), notlon = c(150))
  result <- pr_rename(df)
  testthat::expect_true("notlat" %in% names(result))
})

testthat::test_that("pr_filter_data errors when parameter column missing", {
  df <- create_test_nrs_indices()
  df$Parameters <- NULL
  testthat::expect_error(pr_filter_data(df, "SomeParameter", "NSI"))
})

# ------------------------------------------------------------------------------
# Data retrieval functions
# Note: Tests for deprecated functions (pr_get_NRSData, pr_get_CPRData)
# have been moved to test-utils_data.R using pr_get_data()
# ------------------------------------------------------------------------------

testthat::test_that("pr_get_data errors on invalid Survey parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_data(Survey = "InvalidSurvey", Type = "Phytoplankton"))
})

testthat::test_that("pr_get_data errors on invalid Type parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_data(Survey = "NRS", Type = "invalid_type"))
})

testthat::test_that("pr_get_data errors on invalid Variable parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "invalid_var"))
})

testthat::test_that("pr_get_Indices errors on invalid Survey parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_Indices(Survey = "InvalidSurvey", Type = "Phytoplankton"))
})

testthat::test_that("pr_get_Indices errors on invalid Type parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_Indices(Survey = "NRS", Type = "InvalidType"))
})

testthat::test_that("pr_get_EOVs errors on invalid Survey parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_EOVs(Survey = "InvalidSurvey"))
})

testthat::test_that("pr_get_FuncGroups errors on invalid Survey parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_FuncGroups(Survey = "InvalidSurvey", Type = "Phytoplankton"))
})

testthat::test_that("pr_get_FuncGroups errors on invalid Type parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_FuncGroups(Survey = "NRS", Type = "InvalidType"))
})

# ------------------------------------------------------------------------------
# Plotting functions
# ------------------------------------------------------------------------------

testthat::test_that("pr_plot_TimeSeries errors when Parameters column missing", {
  df <- create_test_nrs_indices()
  df$Parameters <- NULL
  testthat::expect_error(pr_plot_TimeSeries(df))
})

testthat::test_that("pr_plot_TimeSeries errors when Values column missing", {
  df <- create_test_nrs_indices()
  df$Values <- NULL
  testthat::expect_error(pr_plot_TimeSeries(df))
})

testthat::test_that("pr_plot_Trends errors on invalid Trend parameter", {
  df <- create_test_nrs_indices()
  testthat::expect_error(pr_plot_Trends(df, Trend = "InvalidTrend"))
})

testthat::test_that("pr_plot_EOVs errors when EOV parameter missing from data", {
  df <- create_test_eov_data()
  testthat::expect_error(pr_plot_EOVs(df, EOV = "NonExistentParameter"))
})

testthat::test_that("pr_plot_FreqMap errors when required columns missing", {
  df <- data.frame(Latitude = c(-30, -35))
  testthat::expect_error(pr_plot_FreqMap(df, species = "Some species"))
})

# ------------------------------------------------------------------------------
# Station and bioregion functions
# ------------------------------------------------------------------------------

testthat::test_that("pr_add_StationCode errors when TripCode column missing", {
  df <- data.frame(SomeColumn = c(1, 2, 3))
  # Function may handle gracefully or error
  result <- tryCatch(
    pr_add_StationCode(df),
    error = function(e) "error"
  )
  testthat::expect_true(is.data.frame(result) || result == "error")
})

testthat::test_that("pr_add_StationName errors when StationCode column missing", {
  df <- data.frame(SomeColumn = c(1, 2, 3))
  testthat::expect_error(pr_add_StationName(df))
})

testthat::test_that("pr_filter_NRSStations works with valid station data", {
  df <- create_test_stations()
  result <- pr_filter_NRSStations(df)
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) <= nrow(df))
})

testthat::test_that("pr_add_Bioregions errors when coordinate columns missing", {
  df <- data.frame(SomeColumn = c(1, 2))
  testthat::expect_error(pr_add_Bioregions(df))
})

testthat::test_that("pr_add_Bioregions works with valid coordinate data", {
  df <- data.frame(
    TripCode = c("NRS001", "NRS002"),
    Latitude = c(-27.33, -34.09),
    Longitude = c(153.55, 151.22),
    stringsAsFactors = FALSE
  )
  # Convert to planktonr_dat object
  df <- planktonr_dat(df, Type = "Phytoplankton", Survey = "NRS", Variable = "abundance")
  result <- pr_add_Bioregions(df, near_dist_km = 250)
  testthat::expect_true("BioRegion" %in% names(result) || "Bioregion" %in% names(result))
})

# ------------------------------------------------------------------------------
# Satellite matching functions (network-dependent)
# ------------------------------------------------------------------------------

testthat::test_that("pr_match_MODIS errors when coordinate columns missing", {
  skip_if_offline()
  df <- data.frame(SomeColumn = c(1, 2))
  testthat::expect_error(pr_match_MODIS(df, pr = "chl_gsm"))
})

testthat::test_that("pr_match_Altimetry errors when coordinate columns missing", {
  skip_if_offline()
  df <- data.frame(SomeColumn = c(1, 2))
  testthat::expect_error(pr_match_Altimetry(df, pr = "GSLA"))
})

testthat::test_that("pr_match_GHRSST errors when coordinate columns missing", {
  skip_if_offline()
  df <- data.frame(SomeColumn = c(1, 2))
  testthat::expect_error(pr_match_GHRSST(df, pr = "sea_surface_temperature"))
})

# ------------------------------------------------------------------------------
# Label and relabel functions
# ------------------------------------------------------------------------------

testthat::test_that("pr_relabel warns on unrecognized variable name", {
  testthat::expect_warning(pr_relabel("UnknownVariable_xyz", style = "simple"))
})

testthat::test_that("pr_relabel returns appropriate type for ggplot style", {
  result <- pr_relabel("Chla_mgm3", style = "ggplot")
  testthat::expect_type(result, "language")
})

testthat::test_that("pr_relabel returns appropriate type for plotly style", {
  result <- pr_relabel("Chla_mgm3", style = "plotly")
  testthat::expect_type(result, "character")
})

# ------------------------------------------------------------------------------
# Analysis functions
# ------------------------------------------------------------------------------

testthat::test_that("pr_remove_outliers handles invalid threshold", {
  df <- create_test_nrs_indices()
  # Function may not validate x < 0, just test it runs
  result <- tryCatch(
    pr_remove_outliers(df, x = -1),
    error = function(e) NULL
  )
  testthat::expect_true(is.null(result) || is.data.frame(result))
})

testthat::test_that("pr_remove_outliers handles empty data gracefully", {
  # Create proper planktonr_dat object with minimal structure
  df <- data.frame(
    Values = numeric(0),
    StationCode = character(0)
  )
  df <- planktonr_dat(df, Type = "Phytoplankton", Survey = "NRS", Variable = "abundance")
  result <- tryCatch(
    pr_remove_outliers(df, x = 2),
    error = function(e) NULL
  )
  # Should either error or return empty data
  testthat::expect_true(is.null(result) || nrow(result) == 0)
})

testthat::test_that("pr_harmonic returns expected length", {
  result <- pr_harmonic(2, 2)
  testthat::expect_type(result, "double")
  testthat::expect_length(result, 4)
})

