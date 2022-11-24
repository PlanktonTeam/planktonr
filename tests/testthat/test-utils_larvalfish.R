testthat::test_that("Correct function output", {
  testthat::expect_equal(class(pr_get_LFData())[[1]], "tbl_df")
  # testthat::expect_equal(class(pr_get_LFData() %>% pr_plot_LarvalFishDist(SpeciesName = "Acanthuridae_37437900", interactive = TRUE))[[1]], "leaflet")
})
