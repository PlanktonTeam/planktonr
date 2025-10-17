# ==============================================================================
# Integration tests (network-dependent)
# ==============================================================================

testthat::test_that("pr_get_DataLocs returns data frame for NRS survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_DataLocs("NRS")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_DataLocs returns data frame for CPR survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_DataLocs("CPR")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_DataLocs returns data frame for all surveys", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_DataLocs()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_match_Altimetry matches GSLA data at 1km resolution", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  locs <- tail(pr_get_DataLocs("NRS"), 5)
  result <- pr_match_Altimetry(locs, pr = "GSLA", res_spat = 1)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("pr_match_Altimetry matches GSLA data at 5km resolution", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  locs <- tail(pr_get_DataLocs("NRS"), 5)
  result <- pr_match_Altimetry(locs, pr = "GSLA", res_spat = 5)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("pr_match_MODIS matches chlorophyll data at 1km resolution", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  locs <- head(pr_get_DataLocs("NRS"), 5)
  result <- pr_match_MODIS(locs, pr = c("chl_gsm", "chl_oc3"), res_spat = 1)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("pr_match_MODIS matches chlorophyll data at 5km resolution", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  locs <- head(pr_get_DataLocs("NRS"), 5)
  result <- pr_match_MODIS(locs, pr = c("chl_gsm", "chl_oc3"), res_spat = 5)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("pr_match_GHRSST matches SST data at 1km resolution", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  locs <- head(pr_get_DataLocs("NRS"), 5)
  result <- pr_match_GHRSST(locs, pr = "sea_surface_temperature", res_spat = 1)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("pr_match_GHRSST matches SST data at 5km resolution", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  locs <- head(pr_get_DataLocs("NRS"), 5)
  result <- pr_match_GHRSST(locs, pr = "sea_surface_temperature", res_spat = 5)
  testthat::expect_s3_class(result, "data.frame")
})

# ==============================================================================
# Unit tests using fixtures (no network dependency)
# ==============================================================================

testthat::test_that("satellite location fixture has expected structure", {
  locs <- create_test_satellite_locs()
  
  testthat::expect_s3_class(locs, "data.frame")
  testthat::expect_true("Latitude" %in% names(locs))
  testthat::expect_true("Longitude" %in% names(locs))
  testthat::expect_true("SampleTime_UTC" %in% names(locs))
  testthat::expect_s3_class(locs$SampleTime_UTC, "POSIXct")
})

testthat::test_that("location data has valid coordinates", {
  locs <- create_test_satellite_locs()
  
  testthat::expect_true(all(locs$Latitude >= -90 & locs$Latitude <= 90))
  testthat::expect_true(all(locs$Longitude >= -180 & locs$Longitude <= 180))
})

testthat::test_that("satellite matching requires coordinate columns", {
  invalid_df <- data.frame(SomeColumn = c(1, 2, 3))
  
  testthat::expect_error(pr_match_MODIS(invalid_df, pr = "chl_gsm"))
  testthat::expect_error(pr_match_Altimetry(invalid_df, pr = "GSLA"))
  testthat::expect_error(pr_match_GHRSST(invalid_df, pr = "sea_surface_temperature"))
})

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_DataLocs errors on invalid survey", {
  skip_if_offline()
  testthat::expect_error(pr_get_DataLocs("InvalidSurvey"))
})
