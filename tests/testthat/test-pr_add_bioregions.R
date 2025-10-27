# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_add_Bioregions adds bioregion column to CPR indices data", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Raw("cpr_derived_indices_data") %>%
   pr_rename() %>%
  pr_add_Bioregions(near_dist_km = 250), "list")
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_add_Bioregions handles data with latitude and longitude", {
  test_data <- data.frame(
    TripCode = paste0("TRIP", 1:3),
    Latitude = c(-35, -28, -42),
    Longitude = c(150, 153, 148),
    Values = c(10, 20, 30)
  )
  # Convert to planktonr_dat object
  test_data <- planktonr_dat(test_data, Type = "Phytoplankton", Survey = "NRS", Variable = "test")
  testthat::expect_no_error(pr_add_Bioregions(test_data, near_dist_km = 250))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_add_Bioregions handles missing coordinate columns", {
  test_data <- data.frame(x = 1:10, y = 11:20)
  testthat::expect_error(pr_add_Bioregions(test_data))
})

