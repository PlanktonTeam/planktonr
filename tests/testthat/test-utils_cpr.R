testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_CPRData(Type = "phytoplankton", Variable = "abundance", Subset = "htg"), "list")
  testthat::expect_type(pr_get_CPRData(Type = "Zooplankton", Variable = "abundance", Subset = "species"), "list")
  testthat::expect_type(pr_get_CPRTrips(), "list")
})
