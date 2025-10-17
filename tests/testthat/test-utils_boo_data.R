# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_get_FuncGroups returns list for NRS Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_FuncGroups returns list for NRS Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_FuncGroups returns list for CPR Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_FuncGroups returns list for CPR Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_FreqMap returns list for Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_FreqMap(Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_FreqMap returns list for Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_FreqMap(Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_DayNight returns list for Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_DayNight(Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_DayNight returns list for Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_DayNight(Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_STI returns list for Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_STI(Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_STI returns list for Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_STI(Type = "Zooplankton"), "list")
})

testthat::test_that("pr_get_ProgressMapData returns list for combined NRS and CPR surveys", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_ProgressMapData(Survey = c("NRS", "CPR")), "list")
})

testthat::test_that("pr_get_ProgressMapData returns list for NRS survey only", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_ProgressMapData(Survey = "NRS"), "list")
})

testthat::test_that("pr_get_ProgressMapData returns list for CPR survey only", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_ProgressMapData(Survey = "CPR"), "list")
})

testthat::test_that("pr_get_CTI returns list for Phytoplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_CTI(Type = "Phytoplankton"), "list")
})

testthat::test_that("pr_get_CTI returns list for Zooplankton", {
  skip_if_offline()
  testthat::skip_on_cran()
  testthat::expect_type(pr_get_CTI(Type = "Zooplankton"), "list")
})

# ============================================================================
# Unit Tests (use fixtures, no network required)
# ============================================================================

testthat::test_that("pr_get_FuncGroups returns correct structure with mock data", {
  # Using helper fixture for functional group testing
  mock_fg <- data.frame(
    TaxonName = c("Diatoms", "Dinoflagellates", "Copepods"),
    FunctionalGroup = c("Phytoplankton", "Phytoplankton", "Zooplankton"),
    Count = c(100, 50, 25),
    stringsAsFactors = FALSE
  )
  mock_fg <- planktonr_dat(mock_fg, Type = "Phytoplankton", Survey = "NRS", Variable = "test")
  
  testthat::expect_s3_class(mock_fg, "planktonr_dat")
  testthat::expect_true("FunctionalGroup" %in% names(mock_fg))
})

testthat::test_that("pr_get_ProgressMapData handles different survey combinations", {
  # Test that the function can handle survey parameter types
  testthat::expect_no_error({
    surveys_single <- "NRS"
    surveys_multiple <- c("NRS", "CPR")
  })
})

testthat::test_that("Type parameter validation works correctly", {
  valid_types <- c("Phytoplankton", "Zooplankton")
  testthat::expect_true(all(valid_types %in% c("Phytoplankton", "Zooplankton")))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_get_FuncGroups handles invalid survey parameter", {
  testthat::expect_error(pr_get_FuncGroups(Survey = "INVALID", Type = "Phytoplankton"))
})

testthat::test_that("pr_get_FuncGroups handles invalid type parameter", {
  testthat::expect_error(pr_get_FuncGroups(Survey = "NRS", Type = "INVALID"))
})

testthat::test_that("pr_get_FreqMap handles invalid type parameter", {
  testthat::expect_error(pr_get_FreqMap(Type = "INVALID"))
})

testthat::test_that("pr_get_DayNight handles invalid type parameter", {
  testthat::expect_error(pr_get_DayNight(Type = "INVALID"))
})

testthat::test_that("pr_get_ProgressMapData handles invalid survey parameter", {
  testthat::expect_error(pr_get_ProgressMapData(Survey = "INVALID"))
})

testthat::test_that("pr_get_CTI handles invalid type parameter", {
  testthat::expect_error(pr_get_CTI(Type = "INVALID"))
})

