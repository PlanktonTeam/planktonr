testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_pol("LTM"), "list")
  testthat::expect_type(pr_get_polInfo("NRS"), "list")
})
