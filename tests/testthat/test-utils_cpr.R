# ==============================================================================
# Integration tests (network-dependent)
# ==============================================================================

testthat::test_that("pr_get_CPRData returns list for phytoplankton abundance htg subset", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_CPRData(Type = "phytoplankton", Variable = "abundance", Subset = "htg")
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_CPRData returns list for Zooplankton abundance species subset", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_CPRData(Type = "Zooplankton", Variable = "abundance", Subset = "species")
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_CPRTrips returns list", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_CPRTrips()
  testthat::expect_type(result, "list")
  testthat::expect_true(nrow(result) > 0)
})

# ==============================================================================
# Unit tests using fixtures (no network dependency)
# ==============================================================================

testthat::test_that("CPR data fixture has expected structure", {
  cpr <- create_test_cpr_data()
  
  testthat::expect_s3_class(cpr, "data.frame")
  testthat::expect_true("BioRegion" %in% names(cpr))
  testthat::expect_true("TaxonGroup" %in% names(cpr))
  testthat::expect_true(nrow(cpr) > 0)
})

testthat::test_that("CPR data can be filtered by bioregion", {
  cpr <- create_test_cpr_data()
  north <- cpr[cpr$BioRegion == "North", ]
  
  testthat::expect_true(nrow(north) > 0)
  testthat::expect_true(all(north$BioRegion == "North"))
})

testthat::test_that("CPR indices fixture has expected structure", {
  indices <- create_test_cpr_indices()
  
  testthat::expect_true("BioRegion" %in% names(indices))
  testthat::expect_true("Parameters" %in% names(indices))
  testthat::expect_true("Values" %in% names(indices))
})

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_CPRData errors on invalid Type parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_CPRData(Type = "InvalidType", Variable = "abundance", Subset = "htg"))
})

testthat::test_that("pr_get_CPRData errors on invalid Variable parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_CPRData(Type = "Phytoplankton", Variable = "invalid_var", Subset = "htg"))
})
