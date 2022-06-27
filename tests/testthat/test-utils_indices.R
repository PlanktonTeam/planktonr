testthat::test_that("Correct function output", {
  testthat::expect_type(pr_get_indices("NRS", "P"), "list")
  testthat::expect_type(pr_get_indices("NRS", "Z"), "list")
  testthat::expect_type(pr_get_indices("CPR", "P"), "list")
  testthat::expect_type(pr_get_indices("CPR", "Z"), "list")
  testthat::expect_type(data.frame(Month = rep(1:12,10),
                                   StationCode = 'NSI',
                                   Values = runif(120, min=0, max=10)) %>%
                          pr_make_climatology("Month"), "list")

})
