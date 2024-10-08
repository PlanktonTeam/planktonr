testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_Indices("NRS", "P"), "list")
  testthat::expect_type(pr_get_Indices("NRS", "Z"), "list")
  testthat::expect_type(pr_get_Indices("NRS", "W"), "list")
  testthat::expect_type(pr_get_Indices("CPR", "P"), "list")
  testthat::expect_type(pr_get_Indices("CPR", "Z"), "list")
  testthat::expect_type(pr_get_Indices("CPR", "W"), "list")
  testthat::expect_type(pr_get_Indices("CPR", "Z") %>%
                          pr_filter_data("BiomassIndex_mgm3", c("North", "South-west")), "list")

  testthat::expect_type(pr_get_Indices("NRS", "P") %>%
                          pr_filter_data("PhytoBiomassCarbon_pgL", c("NSI", "PHB")), "list")

  testthat::expect_type(data.frame(Month = rep(1:12,10),
                                   StationCode = 'NSI',
                                   Values = runif(120, min=0, max=10)) %>%
                          pr_make_climatology("Month"), "list")

})
