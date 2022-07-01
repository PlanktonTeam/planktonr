testthat::test_that("Correct function output", {

  testthat::expect_type(planktonr::pr_get_fg("NRS", "P"), "list")
  testthat::expect_type(planktonr::pr_get_fg("NRS", "Z"), "list")
  testthat::expect_type(planktonr::pr_get_fg("CPR", "P"), "list")
  testthat::expect_type(planktonr::pr_get_fg("CPR", "Z"), "list")

  testthat::expect_type(pr_get_fMap_data("P"), "list")
  testthat::expect_type(pr_get_fMap_data("Z"), "list")

  testthat::expect_type(pr_get_daynight("P"), "list")
  testthat::expect_type(pr_get_daynight("Z"), "list")

  testthat::expect_type(pr_get_sti("P"), "list")
  testthat::expect_type(pr_get_sti("Z"), "list")

  testthat::expect_type(pr_get_SppCount("Zooplankton"), "list")

  testthat::expect_type(pr_get_facts(), "character")

  testthat::expect_type(pr_get_papers(), "character")

  testthat::expect_type(pr_get_ProgressMap(c("NRS", "CPR")), "list")
  testthat::expect_type(pr_get_ProgressMap("NRS"), "list")
  testthat::expect_type(pr_get_ProgressMap("CPR"), "list")

})

