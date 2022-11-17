testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_LFData(), "list")
  testthat::expect_type(pr_get_LFData() %>% pr_plot_LarvalFishDist(SpeciesName = "Acanthuridae_37437900", interactive = TRUE), "leaflet")
})
