testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_indices("NRS", "P"), "list")
  testthat::expect_type(pr_get_indices("NRS", "Z"), "list")
  testthat::expect_type(pr_get_indices("CPR", "P"), "list")
  testthat::expect_type(pr_get_indices("CPR", "Z"), "list")
})
