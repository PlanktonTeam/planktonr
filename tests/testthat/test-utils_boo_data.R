testthat::test_that("Correct function output", {

  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton"), "list")
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton"), "list")
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton"), "list")
  testthat::expect_type(planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Zooplankton"), "list")

  testthat::expect_type(pr_get_FreqMap(Type = "Phytoplankton"), "list")
  testthat::expect_type(pr_get_FreqMap(Type = "Zooplankton"), "list")

  testthat::expect_type(pr_get_DayNight(Type = "Phytoplankton"), "list")
  testthat::expect_type(pr_get_DayNight(Type = "Zooplankton"), "list")

  testthat::expect_type(pr_get_STI(Type = "Phytoplankton"), "list")
  testthat::expect_type(pr_get_STI(Type = "Zooplankton"), "list")

  testthat::expect_type(pr_get_ProgressMapData(Survey = c("NRS", "CPR")), "list")
  testthat::expect_type(pr_get_ProgressMapData(Survey = "NRS"), "list")
  testthat::expect_type(pr_get_ProgressMapData(Survey = "CPR"), "list")

  testthat::expect_type(pr_get_CTI(Type = "Phytoplankton"), "list")
  testthat::expect_type(pr_get_CTI(Type = "Zooplankton"), "list")
})

