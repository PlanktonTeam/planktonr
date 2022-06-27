testthat::test_that("Correct function output", {
  testthat::expect_type(pr_relabel("Chla_mgm3", style = "ggplot"), "language")
  testthat::expect_type(pr_relabel("Chla_mgm3", style = "plotly"), "character")
  testthat::expect_type(pr_relabel("Chla_mgm3", style = "simple"), "list")
})
