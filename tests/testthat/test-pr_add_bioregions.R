testthat::test_that("Correct function output", {

testthat::expect_type(pr_get_Raw("cpr_derived_indices_data") %>%
   pr_rename() %>%
  pr_add_Bioregions(near_dist_km = 250), "list")

})

