# ==============================================================================
# Integration tests for pr_get_trips() (network-dependent)
# ==============================================================================

testthat::test_that("pr_get_trips returns valid NRS trip data with temporal information", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_trips(Survey = "NRS")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check required columns
  expected_cols <- c("TripCode", "StationCode")
  testthat::expect_true(all(expected_cols %in% names(result)),
                        info = paste("Missing columns:", 
                                     paste(setdiff(expected_cols, names(result)), collapse = ", ")))
  
  # Check date/time columns if present
  date_cols <- grep("Date|Time|date|time", names(result), value = TRUE)
  if (length(date_cols) > 0) {
    for (col in date_cols) {
      if (inherits(result[[col]], c("POSIXct", "POSIXt", "Date"))) {
        # Check dates are reasonable (after program start, not in future)
        min_date <- as.Date("1990-01-01")
        max_date <- Sys.Date() + 30  # Allow 30 days for scheduled trips
        
        valid_dates <- result[[col]][!is.na(result[[col]])]
        if (length(valid_dates) > 0) {
          testthat::expect_true(all(as.Date(valid_dates) >= min_date),
                                info = paste("Dates before 1990 in", col))
          testthat::expect_true(all(as.Date(valid_dates) <= max_date),
                                info = paste("Dates too far in future in", col))
        }
      }
    }
  }
})

testthat::test_that("pr_get_trips returns valid CPR trip data with bioregions", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_trips(Survey = "CPR")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check required columns for CPR
  expected_cols <- c("TripCode", "BioRegion")
  testthat::expect_true(all(expected_cols %in% names(result)),
                        info = paste("Missing columns:", 
                                     paste(setdiff(expected_cols, names(result)), collapse = ", ")))
  
  # Check that BioRegion has reasonable values
  if ("BioRegion" %in% names(result)) {
    valid_bioregions <- c("North", "North-west", "South-east", "South-west", 
                          "Temperate East", "Coral Sea", "Southern Ocean Region",
                          "None", NA_character_)
    testthat::expect_true(all(result$BioRegion %in% valid_bioregions | is.na(result$BioRegion)),
                          info = "Unexpected BioRegion values found")
  }
})

testthat::test_that("pr_get_trips with CPR accepts near_dist_km argument", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # This should not error
  result <- pr_get_trips(Survey = "CPR", near_dist_km = 250)
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_trips with NRS warns about unused arguments", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Should produce a warning when passing arguments to NRS
  testthat::expect_warning(
    pr_get_trips(Survey = "NRS", near_dist_km = 250),
    "Additional arguments are ignored for NRS trips"
  )
})

# ==============================================================================
# Unit tests (no network dependency)
# ==============================================================================

testthat::test_that("pr_get_trips errors on invalid Survey parameter", {
  testthat::expect_error(
    pr_get_trips(Survey = "InvalidSurvey"),
    "'Survey' must be one of 'NRS' or 'CPR'"
  )
})

testthat::test_that("pr_get_trips errors on non-character Survey parameter", {
  testthat::expect_error(
    pr_get_trips(Survey = 123),
    "'Survey' must be a single character string"
  )
})

testthat::test_that("pr_get_trips errors on vector Survey parameter", {
  testthat::expect_error(
    pr_get_trips(Survey = c("NRS", "CPR")),
    "'Survey' must be a single character string"
  )
})
