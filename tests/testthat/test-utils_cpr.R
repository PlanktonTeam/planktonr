testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_CPRData(Type = "phytoplankton", Variable = "abundance", Subset = "htg"), "list")
  testthat::expect_type(pr_get_CPRData(Type = "zooplankton", Variable = "abundance", Subset = "species"), "list")
  testthat::expect_type(pr_get_CPRTrips(), "list")
  testthat::expect_type(pr_get_CPRSamps(), "list")
})
