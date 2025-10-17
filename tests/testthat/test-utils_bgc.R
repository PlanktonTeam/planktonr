# ==============================================================================
# Integration tests (network-dependent) - skip offline
# ==============================================================================

testthat::test_that("pr_get_NRSChemistry returns data frame", {
  skip_if_offline()
  result <- pr_get_NRSChemistry()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true("Parameters" %in% names(result))
})

testthat::test_that("pr_get_NRSPigments returns data frame in standard format", {
  skip_if_offline()
  result <- pr_get_NRSPigments()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSPigments returns data frame in binned format", {
  skip_if_offline()
  result <- pr_get_NRSPigments(Format = "binned")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSPico returns data frame", {
  skip_if_offline()
  result <- pr_get_NRSPico()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSMicro returns data frame for NRS survey", {
  skip_if_offline()
  result <- pr_get_NRSMicro(Survey = "NRS")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSMicro returns data frame for Coastal survey", {
  skip_if_offline()
  result <- pr_get_NRSMicro(Survey = "Coastal")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSMicro returns data frame for GO-SHIP survey", {
  skip_if_offline()
  result <- pr_get_NRSMicro(Survey = "GO-SHIP")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_CSChem returns data frame", {
  skip_if_offline()
  result <- pr_get_CSChem()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSTSS returns data frame", {
  skip_if_offline()
  result <- pr_get_NRSTSS()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSCTD returns data frame", {
  skip_if_offline()
  result <- pr_get_NRSCTD()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_LTnuts returns data frame", {
  skip_if_offline()
  result <- pr_get_LTnuts()
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSEnvContour returns data frame for Pico data", {
  skip_if_offline()
  result <- pr_get_NRSEnvContour(Data = "Pico")
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

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_NRSMicro errors on invalid Survey parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_NRSMicro(Survey = "InvalidSurvey"))
})

testthat::test_that("pr_get_NRSPigments errors on invalid Format parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_NRSPigments(Format = "invalid_format"))
})

testthat::test_that("pr_get_NRSEnvContour errors on invalid Data parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_NRSEnvContour(Data = "InvalidData"))
})
