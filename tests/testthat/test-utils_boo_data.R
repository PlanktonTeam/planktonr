testthat::test_that("Correct function output", {
  testthat::expect_type(planktonr::pr_get_fg("NRS", "Z"), "list")
  testthat::expect_type(pr_get_fMap_data("P"), "list")
  testthat::expect_type(pr_get_daynight("Z"), "list")
  testthat::expect_type(pr_get_sti("Z"), "list")
  testthat::expect_type(pr_get_SppCount("Zooplankton"), "list")
  testthat::expect_type(pr_get_facts(), "character")
  testthat::expect_type(pr_get_papers(), "character")
})
