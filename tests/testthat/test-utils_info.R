# Tests for pr_get_info() unified information retrieval function

# ============================================================================
# Integration Tests (require network)
# ============================================================================

testthat::test_that("pr_get_info returns data frame for Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "Zooplankton")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true("Taxon Name" %in% names(result))
})

testthat::test_that("pr_get_info returns data frame for Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "Phytoplankton")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true("Taxon Name" %in% names(result))
})

testthat::test_that("pr_get_info accepts shorthand 'Z' for Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "Z")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_info accepts shorthand 'P' for Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "P")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_info returns data frame for NRS survey", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "NRS")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns for NRS policy info
  expected_cols <- c("StationName", "Latitude", "Longitude", "Region", "Features", "now")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Check that regions are assigned correctly
  regions <- unique(result$Region)
  expected_regions <- c("Tropical North", "GBR Lagoon", "South East", "South Central", "South West")
  testthat::expect_true(any(expected_regions %in% regions))
})

testthat::test_that("pr_get_info returns data frame for CPR survey", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "CPR")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns for CPR policy info
  expected_cols <- c("BioRegion", "SampleStartDate", "Miles", "Features")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Check BioRegions are present
  regions <- unique(result$BioRegion)
  expected_regions <- c("South-east", "South-west", "Temperate East", "Coral Sea",
                        "North-west", "North", "Southern Ocean Region")
  testthat::expect_true(any(expected_regions %in% regions))

  # Check that Miles is numeric and positive
  testthat::expect_true(is.numeric(result$Miles))
  testthat::expect_true(all(result$Miles > 0))
})

testthat::test_that("pr_get_info returns data frame for SOTS survey", {
  skip_if_offline()
  testthat::skip_on_cran()

  result <- pr_get_info(Source = "SOTS")

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns
  expected_cols <- c("StationName", "Latitude", "Longitude", "Region", "Features", "now")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Check that SOTS station is present
  testthat::expect_true("Southern Ocean Time Series" %in% result$StationName)

  # Check Region is Southern Ocean
  testthat::expect_true("Southern Ocean" %in% result$Region)
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_get_info errors on invalid Source", {
  testthat::expect_error(
    pr_get_info(Source = "invalid"),
    "'Source' must be one of"
  )
})

testthat::test_that("pr_get_info errors on NULL Source", {
  testthat::expect_error(
    pr_get_info(Source = NULL),
    "'Source' must be a single character string"
  )
})

testthat::test_that("pr_get_info errors on multiple Source values", {
  testthat::expect_error(
    pr_get_info(Source = c("NRS", "CPR")),
    "'Source' must be a single character string"
  )
})
