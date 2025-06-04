testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_NRSData(Type = "phytoplankton", Variable = "abundance", Subset = "htg"), "list")
  testthat::expect_type(pr_get_NRSData(Type = "Zooplankton", Variable = "abundance", Subset = "species"), "list")

  testthat::expect_type(pr_get_NRSStation(), "list")
  testthat::expect_type(pr_get_NRSTrips(), "list")
  testthat::expect_type(pr_get_NRSStation() %>% pr_filter_NRSStations(), "list")
})
