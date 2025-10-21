# ==============================================================================
# Integration tests for SOTS utilities (network-dependent)
# ==============================================================================

testthat::test_that("pr_get_SOTSvariables returns Physical variables with expected structure", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSvariables(Type = "Physical")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(ncol(result) >= 1)
  
  # Check that we have variable names
  testthat::expect_true(all(!is.na(result[[1]])))
  testthat::expect_type(result[[1]], "character")
  
  # Check for expected physical variables
  all_vars <- paste(result[[1]], collapse = " ")
  testthat::expect_true(any(grepl("TEMP|PSAL|MLD", all_vars, ignore.case = TRUE)),
                        info = "Expected to find at least one physical variable (TEMP, PSAL, or MLD)")
})

testthat::test_that("pr_get_SOTSvariables returns Nutrients variables with expected structure", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSvariables(Type = "Nutrients")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(ncol(result) >= 1)
  
  # Check that we have variable names
  testthat::expect_true(all(!is.na(result[[1]])))
  testthat::expect_type(result[[1]], "character")
  
  # Check for expected nutrient variables
  all_vars <- paste(result[[1]], collapse = " ")
  testthat::expect_true(any(grepl("NTRI|PHOS|SLCA|pH|TCO2|TALK", all_vars, ignore.case = TRUE)),
                        info = "Expected to find at least one nutrient variable")
})

testthat::test_that("pr_get_SOTSMoorData returns Physical data with expected columns and structure", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Physical")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("StationName", "StationCode", "Month_Local", "SampleTime_Local", 
                     "Parameters", "SampleDepth_m", "Values", "Year_Local")
  testthat::expect_true(all(expected_cols %in% names(result)),
                        info = paste("Missing columns:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
  
  # Check StationCode is SOTS
  testthat::expect_true(all(result$StationCode == "SOTS"))
  
  # Check StationName
  testthat::expect_true(all(result$StationName == "Southern Ocean Time Series"))
  
  # Check data types
  testthat::expect_true(inherits(result$SampleTime_Local, c("POSIXct", "Date")))
  testthat::expect_type(result$StationCode, "character")
  testthat::expect_type(result$Parameters, "character")
  testthat::expect_true(is.numeric(result$Values))
  testthat::expect_true(is.numeric(result$SampleDepth_m))
  testthat::expect_true(is.numeric(result$Month_Local))
  testthat::expect_true(is.numeric(result$Year_Local))
  
  # Check reasonable depth values (should be in 0, 30, 50, 100, 200, 500)
  expected_depths <- c(0, 30, 50, 100, 200, 500)
  testthat::expect_true(all(result$SampleDepth_m %in% expected_depths),
                        info = paste("Unexpected depths found:", paste(unique(result$SampleDepth_m), collapse = ", ")))
  
  # Check for expected physical parameters
  expected_params <- c("Temperature_degC", "Salinity", "ChlF_mgm3", "MLD_m", "DissolvedOxygen_umolkg")
  testthat::expect_true(any(expected_params %in% unique(result$Parameters)),
                        info = paste("Expected at least one of:", paste(expected_params, collapse = ", ")))
  
  # Check month range
  testthat::expect_true(all(result$Month_Local >= 1 & result$Month_Local <= 12))
  
  # Check year range (should be reasonably recent)
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  testthat::expect_true(all(result$Year_Local >= 1997 & result$Year_Local <= current_year))
  
  # Check no NA values in critical columns
  testthat::expect_false(any(is.na(result$Parameters)))
  testthat::expect_false(any(is.na(result$Values)))
})

testthat::test_that("pr_get_SOTSMoorData returns Nutrients data with expected columns and structure", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Nutrients")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("StationName", "StationCode", "Month_Local", "SampleTime_Local", 
                     "Parameters", "SampleDepth_m", "Values", "Year_Local")
  testthat::expect_true(all(expected_cols %in% names(result)),
                        info = paste("Missing columns:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
  
  # Check StationCode is SOTS
  testthat::expect_true(all(result$StationCode == "SOTS"))
  
  # Check for expected nutrient parameters
  expected_params <- c("Nitrate_umolL", "Phosphate_umolL", "Silicate_umolL", 
                       "Alkalinity_umolkg", "pH", "DIC_umolkg")
  testthat::expect_true(any(expected_params %in% unique(result$Parameters)),
                        info = paste("Expected at least one of:", paste(expected_params, collapse = ", ")))
  
  # Check no NA values in critical columns
  testthat::expect_false(any(is.na(result$Parameters)))
  testthat::expect_false(any(is.na(result$Values)))
})

testthat::test_that("pr_get_SOTSMoorData filters out NA Parameters and Values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Physical")
  
  # The function should drop NA Parameters and Values
  testthat::expect_false(any(is.na(result$Parameters)))
  testthat::expect_false(any(is.na(result$Values)))
})

testthat::test_that("pr_get_SOTSMoorData rounds depths correctly", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Physical")
  
  # Depths should be rounded to nearest 10m and filtered to specific depths
  expected_depths <- c(0, 30, 50, 100, 200, 500)
  unique_depths <- unique(result$SampleDepth_m)
  
  testthat::expect_true(all(unique_depths %in% expected_depths),
                        info = paste("Found unexpected depths:", paste(setdiff(unique_depths, expected_depths), collapse = ", ")))
})

testthat::test_that("pr_get_SOTSMoorData has correct planktonr_dat attributes", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Physical")
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "SOTS")
})

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_SOTSvariables validates Type parameter", {
  testthat::expect_error(
    pr_get_SOTSvariables(Type = "Invalid"),
    regexp = "must be one of 'Physical' or 'Nutrients'"
  )
  
  testthat::expect_error(
    pr_get_SOTSvariables(Type = c("Physical", "Nutrients")),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_SOTSvariables(Type = 123),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_SOTSvariables(Type = NULL),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_SOTSMoorData validates Type parameter", {
  testthat::expect_error(
    pr_get_SOTSMoorData(Type = "Invalid"),
    regexp = "must be one of 'Physical' or 'Nutrients'"
  )
  
  testthat::expect_error(
    pr_get_SOTSMoorData(Type = c("Physical", "Nutrients")),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_SOTSMoorData(Type = 123),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_SOTSMoorData(Type = NULL),
    regexp = "must be a single character string"
  )
})

# ==============================================================================
# Unit tests (parameter validation)
# ==============================================================================

testthat::test_that("pr_get_SOTSvariables accepts valid Type values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Both valid values should work without error
  testthat::expect_no_error(pr_get_SOTSvariables(Type = "Physical"))
  testthat::expect_no_error(pr_get_SOTSvariables(Type = "Nutrients"))
})

testthat::test_that("pr_get_SOTSMoorData accepts valid Type values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Both valid values should work without error
  testthat::expect_no_error(pr_get_SOTSMoorData(Type = "Physical"))
  testthat::expect_no_error(pr_get_SOTSMoorData(Type = "Nutrients"))
})

testthat::test_that("pr_get_SOTSvariables returns distinct variables", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSvariables(Type = "Physical")
  
  # Should have no duplicate variable names
  testthat::expect_equal(nrow(result), nrow(dplyr::distinct(result)))
})

testthat::test_that("pr_get_SOTSMoorData parameter names are standardized", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Physical")
  
  # Check that parameter names follow expected patterns
  params <- unique(result$Parameters)
  
  # Physical parameters should not contain raw NetCDF variable names
  testthat::expect_false(any(grepl("^TEMP$|^PSAL$|^CPHL$|^DOX2$|^MLD$", params)),
                         info = "Parameters should be standardized, not raw NetCDF names")
  
  # Should contain standardized names
  standardized_pattern <- "Temperature_degC|Salinity|ChlF_mgm3|DissolvedOxygen_umolkg|MLD_m"
  testthat::expect_true(any(grepl(standardized_pattern, params)),
                        info = "Expected to find standardized parameter names")
})

testthat::test_that("pr_get_SOTSMoorData averages daily values correctly", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_SOTSMoorData(Type = "Physical")
  
  # SampleTime_Local should be floored to day
  # Check that time component is midnight (00:00:00)
  if (inherits(result$SampleTime_Local, "POSIXct")) {
    time_components <- format(result$SampleTime_Local, "%H:%M:%S")
    # All times should be 00:00:00 or the date should have no time component
    testthat::expect_true(all(time_components == "00:00:00" | is.na(time_components)))
  }
})
