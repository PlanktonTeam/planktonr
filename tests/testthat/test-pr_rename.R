testthat::test_that("Correct function output", {
  testthat::expect_type(data.frame(LATITUDE = -32, LONGITUDE = 160) %>% pr_rename(), "list")
})
