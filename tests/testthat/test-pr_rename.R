# ============================================================================
# Unit Tests (no network required)
# ============================================================================

testthat::test_that("pr_rename converts uppercase coordinate column names to lowercase", {
  testthat::expect_type(data.frame(LATITUDE = -32, LONGITUDE = 160) %>% planktonr:::pr_rename(), "list")
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_rename handles data frames without coordinate columns", {
  testthat::expect_no_error(data.frame(x = 1:10, y = 11:20) %>% planktonr:::pr_rename())
})
