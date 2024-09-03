testthat::test_that("Correct function output", {
  testthat::expect_s3_class(pr_get_DataLocs("NRS"), "data.frame")
  testthat::expect_s3_class(pr_get_DataLocs("CPR"), "data.frame")
  testthat::expect_s3_class(pr_get_DataLocs(), "data.frame")

  testthat::expect_s3_class(tail(pr_get_DataLocs("NRS"), 5) %>% pr_match_Altimetry(pr = "GSLA", res_spat = 1), "data.frame")
  testthat::expect_s3_class(tail(pr_get_DataLocs("NRS"), 5) %>% pr_match_Altimetry(pr = "GSLA", res_spat = 5), "data.frame")

  testthat::expect_s3_class(head(pr_get_DataLocs("NRS"), 5) %>% pr_match_MODIS(pr <- c("chl_gsm", "chl_oc3"), res_spat = 1), "data.frame")
  testthat::expect_s3_class(head(pr_get_DataLocs("NRS"), 5) %>% pr_match_MODIS(pr <- c("chl_gsm", "chl_oc3"), res_spat = 5), "data.frame")

  testthat::expect_s3_class(head(pr_get_DataLocs("NRS"), 5) %>% pr_match_GHRSST(pr = "sea_surface_temperature", res_spat = 1), "data.frame")
  testthat::expect_s3_class(head(pr_get_DataLocs("NRS"), 5) %>% pr_match_GHRSST(pr = "sea_surface_temperature", res_spat = 5), "data.frame")
})
