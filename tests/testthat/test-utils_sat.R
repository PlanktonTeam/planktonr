testthat::test_that("Correct function output", {

  testthat::expect_type(pr_get_DataLocs("NRS"), "list")
  testthat::expect_type(pr_get_DataLocs("CPR"), "list")
  testthat::expect_type(pr_get_DataLocs(), "list")

  testthat::expect_type(tail(pr_get_DataLocs("NRS"), 5) %>% pr_match_Altimetry(pr = "GSLA"), "list")
  testthat::expect_type(head(pr_get_DataLocs("NRS"),5) %>% pr_match_MODIS(pr <- c("chl_gsm", "chl_oc3")), "list")
})
