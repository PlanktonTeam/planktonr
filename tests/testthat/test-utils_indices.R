testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton"), "list")
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Zooplankton"), "list")
  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Water"), "list")
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Phytoplankton"), "list")
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Zooplankton"), "list")
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Water"), "list")
  testthat::expect_type(pr_get_Indices(Survey = "CPR", Type = "Zooplankton") %>%
                          pr_filter_data("BiomassIndex_mgm3", c("North", "South-west")), "list")

  testthat::expect_type(pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") %>%
                          pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB")), "list")

  testthat::expect_type(data.frame(Month = rep(1:12,10),
                                   StationCode = 'NSI',
                                   Values = runif(120, min=0, max=10)) %>%
                          pr_make_climatology("Month"), "list")

})
