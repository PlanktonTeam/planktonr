testthat::test_that("Correct function output", {

  testthat::expect_type(planktonr::pr_get_FuncGroups("NRS", "P"), "list")
  testthat::expect_type(planktonr::pr_get_FuncGroups("NRS", "Z"), "list")
  testthat::expect_type(planktonr::pr_get_FuncGroups("CPR", "P"), "list")
  testthat::expect_type(planktonr::pr_get_FuncGroups("CPR", "Z"), "list")

  testthat::expect_type(pr_get_FreqMap("P"), "list")
  testthat::expect_type(pr_get_FreqMap("Z"), "list")

  testthat::expect_type(pr_get_DayNight("P"), "list")
  testthat::expect_type(pr_get_DayNight("Z"), "list")

  testthat::expect_type(pr_get_STI("P"), "list")
  testthat::expect_type(pr_get_STI("Z"), "list")

  testthat::expect_type(pr_get_SppCount("P"), "list")
  testthat::expect_type(pr_get_SppCount("Z"), "list")

  testthat::expect_type(pr_get_Facts(), "character")

  testthat::expect_type(pr_get_Papers(), "character")

  testthat::expect_type(pr_get_ProgressMap(c("NRS", "CPR")), "list")
  testthat::expect_type(pr_get_ProgressMap("NRS"), "list")
  testthat::expect_type(pr_get_ProgressMap("CPR"), "list")

  testthat::expect_type(pr_get_CTI("P"), "list")
  testthat::expect_type(pr_get_CTI("Z"), "list")
})

