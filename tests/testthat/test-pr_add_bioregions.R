testthat::test_that("Correct function output", {


testthat::expect_type(data.frame(pr_get_Raw("cpr_derived_indices_data") %>%
   pr_rename() %>%
  pr_add_Bioregions("st_nearest_feature")))

})

