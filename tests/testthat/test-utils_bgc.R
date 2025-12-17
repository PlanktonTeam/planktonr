# ==============================================================================
# Tests for utils_bgc.R - non-deprecated BGC functions
# ==============================================================================

# ==============================================================================
# Integration tests (network-dependent) - skip offline
# ==============================================================================

testthat::test_that("pr_get_LTnuts returns combined long-term data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_LTnuts()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("Project", "StationName", "StationCode", "SampleTime_Local",
                     "Month_Local", "Year_Local", "SampleDepth_m", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Should combine data from LTM, NRS Chemistry, and NRS CTD
  projects <- unique(result$Project)
  testthat::expect_true("LTM" %in% projects | "NRS" %in% projects)
  
  # Should only contain specific stations (MAI, ROT, PHB)
  stations <- unique(result$StationCode)
  testthat::expect_true(all(stations %in% c("MAI", "ROT", "PHB")))
})

testthat::test_that("pr_get_LTnuts filters out -999 values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_LTnuts()
  
  # Should not contain -999 values
  testthat::expect_false(any(result$Values == -999, na.rm = TRUE))
})

testthat::test_that("pr_get_NRSEnvContour processes data correctly", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Pico")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Should not contain NA values (dropped)
  testthat::expect_false(any(is.na(result$Values)))
  
  # Should not contain SecchiDepth parameter
  testthat::expect_false("SecchiDepth_m" %in% unique(result$Parameters))
  
  # SampleTime_Local should be floored to month
  if(nrow(result) > 0 && inherits(result$SampleTime_Local, "POSIXct")) {
    # Check that times are at beginning of month (day = 1, hour = 0)
    testthat::expect_true(all(lubridate::day(result$SampleTime_Local) == 1))
  }
})

testthat::test_that("pr_get_NRSEnvContour works with Chemistry data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Chemistry")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSEnvContour works with Pico data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Pico")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSEnvContour works with Micro data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Micro")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

# ==============================================================================
# Unit tests using fixtures (no network dependency)
# ==============================================================================

testthat::test_that("chemistry data fixture has expected structure", {
  chem <- create_test_chemistry()
  testthat::expect_s3_class(chem, "data.frame")
  testthat::expect_true("Parameters" %in% names(chem))
  testthat::expect_true("Values" %in% names(chem))
  testthat::expect_true("StationCode" %in% names(chem))
  testthat::expect_true(nrow(chem) > 0)
})

testthat::test_that("chemistry data can be filtered by parameter", {
  chem <- create_test_chemistry()
  secchi <- chem[chem$Parameters == "SecchiDepth_m", ]
  testthat::expect_true(nrow(secchi) > 0)
  testthat::expect_true(all(secchi$Parameters == "SecchiDepth_m"))
})

testthat::test_that("chemistry data can be filtered by station", {
  chem <- create_test_chemistry()
  nsi <- chem[chem$StationCode == "NSI", ]
  testthat::expect_true(nrow(nsi) > 0)
  testthat::expect_true(all(nsi$StationCode == "NSI"))
})

testthat::test_that("chemistry data contains reasonable values", {
  chem <- create_test_chemistry()
  
  # Values should be numeric
  testthat::expect_true(is.numeric(chem$Values))
  
  # Values should be non-negative for most parameters
  testthat::expect_true(all(chem$Values >= 0, na.rm = TRUE))
})

testthat::test_that("chemistry data has temporal information", {
  chem <- create_test_chemistry()
  
  # Should have time-related columns
  testthat::expect_true("Month_Local" %in% names(chem) | "SampleTime_Local" %in% names(chem))
  
  # Month values should be 1-12
  if("Month_Local" %in% names(chem)) {
    testthat::expect_true(all(chem$Month_Local >= 1 & chem$Month_Local <= 12))
  }
})

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_NRSEnvContour validates Data parameter", {
  testthat::expect_error(
    pr_get_NRSEnvContour(Data = "InvalidData"),
    regexp = "must be one of 'Chemistry', 'Pico', or 'Micro'"
  )
  
  testthat::expect_error(
    pr_get_NRSEnvContour(Data = c("Chemistry", "Pico")),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_NRSEnvContour(Data = 999),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_NRSEnvContour accepts all valid Data values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # All valid values should work without error
  testthat::expect_no_error(pr_get_NRSEnvContour(Data = "Chemistry"))
  testthat::expect_no_error(pr_get_NRSEnvContour(Data = "Pico"))
  testthat::expect_no_error(pr_get_NRSEnvContour(Data = "Micro"))
})

