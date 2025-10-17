# ============================================================================
# Unit Tests (no network required)
# ============================================================================

testthat::test_that("pr_relabel returns language expression for ggplot style", {
  testthat::expect_type(pr_relabel("Chla_mgm3", style = "ggplot"), "language")
})

testthat::test_that("pr_relabel returns character for plotly style", {
  testthat::expect_type(pr_relabel("Chla_mgm3", style = "plotly"), "character")
})

testthat::test_that("pr_relabel returns list for simple style", {
  testthat::expect_type(pr_relabel("Chla_mgm3", style = "simple"), "list")
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_relabel warns when variable name not recognized", {
  testthat::expect_warning(pr_relabel("Chla_mg", style = "simple"))
})

testthat::test_that("pr_relabel handles invalid style parameter", {
  testthat::expect_error(pr_relabel("Chla_mgm3", style = "INVALID"))
})
