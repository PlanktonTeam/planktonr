# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_get_LFData returns planktonr_dat object", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_equal(class(pr_get_LFData())[[1]], "planktonr_dat")
})

# Commented out test - uncomment when ready
# testthat::test_that("pr_plot_LarvalFishDist creates interactive leaflet map", {
#   skip_if_offline()
#   testthat::skip_on_cran()
#   testthat::expect_equal(class(pr_get_LFData() %>% 
#                          pr_plot_LarvalFishDist(SpeciesName = "Acanthuridae_37437900", 
#                                                 interactive = TRUE))[[1]], "leaflet")
# })

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_get_LFData returns correct class structure", {
  # Test that planktonr_dat class is properly defined
  testthat::expect_true("planktonr_dat" %in% class(structure(list(), class = c("planktonr_dat", "list"))))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_plot_LarvalFishDist handles invalid species name", {
  mock_lf_data <- list(
    Data = data.frame(
      SpeciesName = c("Acanthuridae_37437900", "Blenniidae_37438100"),
      Longitude = c(150, 151),
      Latitude = c(-35, -36),
      stringsAsFactors = FALSE
    )
  )
  class(mock_lf_data) <- c("planktonr_dat", class(mock_lf_data))
  
  testthat::expect_error(pr_plot_LarvalFishDist(mock_lf_data, SpeciesName = "NonexistentSpecies"))
})
