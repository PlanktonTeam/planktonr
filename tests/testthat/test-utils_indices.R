# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_get_Indices returns list for NRS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_Indices returns list for NRS Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_Indices returns list for NRS Water", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Water"), "list")
})

testthat::test_that("pr_get_Indices returns list for CPR Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_Indices returns list for CPR Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_Indices returns list for CPR Water", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Water"), "list")
})

testthat::test_that("pr_filter_data filters CPR Zooplankton indices by bioregion", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                          pr_filter_data("BiomassIndex_mgm3", c("North", "South-west")), "list")
})

testthat::test_that("pr_filter_data filters NRS Phytoplankton indices by station", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
                          pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB")), "list")
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_make_climatology computes monthly climatology", {
  testthat::expect_type(data.frame(Month = rep(1:12,10),
                                   StationCode = 'NSI',
                                   Values = runif(120, min=0, max=10)) %>%
                          pr_make_climatology("Month"), "list")
})

testthat::test_that("pr_filter_data works with mock indices data", {
  mock_indices <- data.frame(
    SampleDateUTC = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
    StationCode = rep(c("NSI", "PHB", "MAI"), 4),
    BioRegion = rep(c("NSI", "PHB", "MAI"), 4),  # Required for pr_filter_data
    Parameters = "BiomassIndex",
    Values = runif(12, 0, 100),
    stringsAsFactors = FALSE
  )
  mock_indices <- planktonr_dat(mock_indices, Type = "Zooplankton", Survey = "NRS", Variable = "test")
  
  filtered <- pr_filter_data(mock_indices, "BiomassIndex", c("NSI", "PHB"))
  testthat::expect_s3_class(filtered, "planktonr_dat")
  testthat::expect_true(all(filtered$StationCode %in% c("NSI", "PHB")))
})

testthat::test_that("pr_make_climatology handles different grouping variables", {
  test_data <- data.frame(
    Month = rep(1:12, 5),
    Year = rep(2018:2022, each = 12),
    StationCode = rep("NSI", 60),  # Required for pr_make_climatology
    Values = runif(60, min = 0, max = 100)
  )
  
  clim <- pr_make_climatology(test_data, "Month")
  testthat::expect_type(clim, "list")
  testthat::expect_equal(nrow(clim), 12)
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_get_Indices handles invalid survey parameter", {
  testthat::expect_error(pr_get_Indices(Survey = "INVALID", Type = "Phytoplankton"))
})

testthat::test_that("pr_get_Indices handles invalid type parameter", {
  testthat::expect_error(pr_get_Indices(Survey = "NRS", Type = "INVALID"))
})

testthat::test_that("pr_make_climatology handles missing grouping variable", {
  test_data <- data.frame(Values = 1:10)
  testthat::expect_error(pr_make_climatology(test_data, "NonexistentColumn"))
})
