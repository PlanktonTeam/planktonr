testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_PolicyData("NRS"), "list")
  testthat::expect_type(pr_get_PolicyData("CPR"), "list")
  testthat::expect_type(pr_get_PolicyData("LTM"), "list")
  testthat::expect_type(pr_get_PolicyInfo("NRS"), "list")
  testthat::expect_type(pr_get_PolicyInfo("CPR"), "list")
})
