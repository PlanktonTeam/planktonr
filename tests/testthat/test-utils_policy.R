testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_EOVs(Survey = "NRS"), "list")
  testthat::expect_type(pr_get_EOVs(Survey = "CPR"), "list")
  testthat::expect_type(pr_get_EOVs(Survey = "LTM"), "list")
  testthat::expect_type(pr_get_PolicyInfo(Survey = "NRS"), "list")
  testthat::expect_type(pr_get_PolicyInfo(Survey = "CPR"), "list")
})
