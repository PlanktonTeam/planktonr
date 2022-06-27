testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_pol("NRS"), "list")
  testthat::expect_type(pr_get_pol("CPR"), "list")
  testthat::expect_type(pr_get_pol("LTM"), "list")
  testthat::expect_type(pr_get_polInfo("NRS"), "list")
  testthat::expect_type(pr_get_polInfo("CPR"), "list")
})
