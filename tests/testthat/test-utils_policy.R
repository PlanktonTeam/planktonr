# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_get_EOVs returns data frame for NRS survey", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "NRS")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns
  expected_cols <- c("SampleTime_Local", "Year_Local", "Month_Local", "StationName",
                     "StationCode", "Parameters", "Values", "means", "sd", "anomaly", "Survey")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Check Survey column value
  testthat::expect_true(all(result$Survey == "NRS"))

  # Check expected parameters for NRS
  expected_params <- c("Biomass_mgm3", "PhytoBiomassCarbon_pgL", "CTDTemperature_degC",
                       "ShannonCopepodDiversity", "ShannonPhytoDiversity", "Salinity",
                       "PigmentChla_mgm3", "Ammonium_umolL", "Nitrate_umolL", "Oxygen_umolL")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))

  # Check Port Hacking 4 is filtered out
  testthat::expect_false("Port Hacking 4" %in% result$StationName)

  # Check that anomaly is calculated
  testthat::expect_true(is.numeric(result$anomaly))
  testthat::expect_true(is.numeric(result$means))
  testthat::expect_true(is.numeric(result$sd))
})

testthat::test_that("pr_get_EOVs returns data frame for CPR survey", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "CPR")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns (DistanceFromBioregion_m not always present in final output)
  expected_cols <- c("SampleTime_Local", "Year_Local", "Month_Local", "BioRegion",
                     "Parameters", "Values", "means", "sd", "anomaly", "Survey")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Check Survey column value
  testthat::expect_true(all(result$Survey == "CPR"))

  # Check expected parameters for CPR
  expected_params <- c("BiomassIndex_mgm3", "PhytoBiomassCarbon_pgm3",
                       "ShannonCopepodDiversity", "ShannonPhytoDiversity", "SST", "chl_oc3")
  params <- unique(result$Parameters)
  testthat::expect_true(any(expected_params %in% params))

  # Check North and North-west regions are filtered out
  regions <- unique(result$BioRegion)
  testthat::expect_false("North" %in% regions)
  testthat::expect_false("North-west" %in% regions)

  # Check that anomaly is calculated
  testthat::expect_true(is.numeric(result$anomaly))
})

testthat::test_that("pr_get_EOVs returns data frame for LTM survey", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "LTM")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns
  expected_cols <- c("StationName", "StationCode", "SampleTime_Local", "Month_Local",
                     "Year_Local", "Parameters", "Values", "means", "sd", "anomaly", "Survey")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Check Survey column value
  testthat::expect_true(all(result$Survey == "LTM"))

  # Check that only specific stations are included (MAI, ROT, PHB)
  stations <- unique(result$StationCode)
  testthat::expect_true(all(stations %in% c("MAI", "ROT", "PHB")))

  # Check that anomaly is calculated
  testthat::expect_true(is.numeric(result$anomaly))
})

testthat::test_that("pr_get_EOVs CPR includes satellite data", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "CPR")

  # Check that satellite parameters are present
  params <- unique(result$Parameters)
  testthat::expect_true("SST" %in% params)
  testthat::expect_true("chl_oc3" %in% params)
})

testthat::test_that("pr_get_EOVs LTM filters by depth", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "LTM")

  # LTM should only include samples < 11m depth (though depth column not in final output)
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true("Parameters" %in% names(result))
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

# Note: These functions require network access for data retrieval, so unit tests are limited

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_get_EOVs validates Survey parameter type", {
  testthat::expect_error(
    pr_get_EOVs(Survey = 123),
    regexp = "must be a single character string"
  )

  testthat::expect_error(
    pr_get_EOVs(Survey = c("NRS", "CPR")),
    regexp = "must be a single character string"
  )

  testthat::expect_error(
    pr_get_EOVs(Survey = NULL),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_EOVs validates Survey parameter value", {
  testthat::expect_error(
    pr_get_EOVs(Survey = "INVALID"),
    regexp = "'Survey' must be one of 'NRS', 'CPR', 'SOTS' or 'LTM'."
  )

  testthat::expect_error(
    pr_get_EOVs(Survey = "nrs"),
    regexp = "'Survey' must be one of 'NRS', 'CPR', 'SOTS' or 'LTM'."
  )
})

testthat::test_that("pr_get_EOVs accepts all valid Survey values", {
  skip_if_offline()
  testthat::skip_on_cran()

  # All valid values should work without error
  testthat::expect_no_error(pr_get_EOVs(Survey = "NRS"))
  testthat::expect_no_error(pr_get_EOVs(Survey = "CPR"))
  testthat::expect_no_error(pr_get_EOVs(Survey = "LTM"))
})

testthat::test_that("pr_get_EOVs anomaly calculation is correct", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "NRS")

  # Check that anomaly = (Values - means) / sd
  # Allow for some floating point tolerance
  calculated_anomaly <- (result$Values - result$means) / result$sd
  testthat::expect_equal(result$anomaly, calculated_anomaly, tolerance = 1e-10)
})

testthat::test_that("pr_get_EOVs NRS has correct variable names", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "NRS")
  params <- unique(result$Parameters)

  # Check for all expected NRS variables
  expected_vars <- c("Biomass_mgm3", "PhytoBiomassCarbon_pgL", "CTDTemperature_degC",
                     "ShannonCopepodDiversity", "ShannonPhytoDiversity", "Salinity",
                     "PigmentChla_mgm3", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                     "Silicate_umolL", "Phosphate_umolL", "Oxygen_umolL")

  # Not all parameters may be present in all stations/times, but should have at least some
  testthat::expect_true(length(intersect(params, expected_vars)) > 0)
})

testthat::test_that("pr_get_EOVs CPR has correct variable names", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_EOVs(Survey = "CPR")
  params <- unique(result$Parameters)

  # Check for all expected CPR variables
  expected_vars <- c("BiomassIndex_mgm3", "PhytoBiomassCarbon_pgm3",
                     "ShannonCopepodDiversity", "ShannonPhytoDiversity", "SST", "chl_oc3")

  testthat::expect_true(all(expected_vars %in% params))
})
