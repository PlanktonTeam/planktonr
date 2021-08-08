test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# test table
# n should be 1, replicates or duplicate samples will have values > 1
# test <- BGC %>%
#   dplyr::group_by(TripCode, SampleDepth_m) %>%
#   dplyr::summarise(n = dplyr::n(),
#                    .groups = "drop")

# df <- create_bgc()
# Check
# max(test$n)
