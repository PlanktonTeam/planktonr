testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_EOVs("NRS"), "list")
  testthat::expect_type(pr_get_EOVs("CPR"), "list")
  testthat::expect_type(pr_get_EOVs("LTM"), "list")
  testthat::expect_type(pr_get_PolicyInfo("NRS"), "list")
  testthat::expect_type(pr_get_PolicyInfo("CPR"), "list")
})
