# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_get_EOVs returns list for NRS survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_EOVs(Survey = "NRS"), "list")
})

testthat::test_that("pr_get_EOVs returns list for CPR survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_EOVs(Survey = "CPR"), "list")
})

testthat::test_that("pr_get_EOVs returns list for LTM survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_EOVs(Survey = "LTM"), "list")
})

testthat::test_that("pr_get_PolicyInfo returns list for NRS survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_PolicyInfo(Survey = "NRS"), "list")
})

testthat::test_that("pr_get_PolicyInfo returns list for CPR survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_PolicyInfo(Survey = "CPR"), "list")
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_get_EOVs returns correct structure for each survey type", {
  # These functions return static lists, so can be tested without network
  nrs_eov <- pr_get_EOVs(Survey = "NRS")
  testthat::expect_type(nrs_eov, "list")
  testthat::expect_true(length(nrs_eov) > 0)
  
  cpr_eov <- pr_get_EOVs(Survey = "CPR")
  testthat::expect_type(cpr_eov, "list")
  testthat::expect_true(length(cpr_eov) > 0)
})

testthat::test_that("pr_get_PolicyInfo returns correct structure for each survey type", {
  # These functions return static lists, so can be tested without network
  nrs_policy <- pr_get_PolicyInfo(Survey = "NRS")
  testthat::expect_type(nrs_policy, "list")
  testthat::expect_true(length(nrs_policy) > 0)
  
  cpr_policy <- pr_get_PolicyInfo(Survey = "CPR")
  testthat::expect_type(cpr_policy, "list")
  testthat::expect_true(length(cpr_policy) > 0)
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_get_EOVs handles invalid survey parameter", {
  testthat::expect_error(pr_get_EOVs(Survey = "INVALID"))
})

testthat::test_that("pr_get_PolicyInfo handles invalid survey parameter", {
  testthat::expect_error(pr_get_PolicyInfo(Survey = "INVALID"))
})
