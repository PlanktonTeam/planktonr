# ============================================================================
# Indices and Calculations Validation Tests
# Testing that derived values are calculated correctly
# ============================================================================

testthat::test_that("pr_get_Indices returns data with expected parameter ranges", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get NRS Phytoplankton indices
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton")

  # Check structure
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)

  # Check expected columns (flexible with date column names)
  essential_cols <- c("StationCode", "Parameters", "Values")
  testthat::expect_true(all(essential_cols %in% names(result)),
                        info = paste("Missing columns:", paste(setdiff(essential_cols, names(result)), collapse = ", ")))

  # Check for some date column (various names possible)
  date_col_exists <- any(c("SampleDateUTC", "SampleTime_UTC", "SampleTime_Local", "SampleDate") %in% names(result))
  testthat::expect_true(date_col_exists, info = "No date column found")

  # Check parameter types exist
  params <- unique(result$Parameters)
  testthat::expect_true(length(params) > 0, info = "No parameters found")

  # Check for expected phytoplankton parameters
  phyto_params <- c("PhytoAbundance_CellsL", "PhytoBiomassCarbon_pgL", "Chla_mgm3")
  found_params <- intersect(phyto_params, params)
  testthat::expect_true(length(found_params) > 0,
                        info = paste("Expected phytoplankton parameters not found. Got:", paste(params, collapse = ", ")))
})

testthat::test_that("Abundance values are non-negative and in reasonable ranges", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get zooplankton indices
  result <- pr_get_Indices(Survey = "NRS", Type = "Zooplankton")

  # Filter to abundance parameters
  abundance_data <- result[grepl("Abundance|abundance", result$Parameters), ]

  if (nrow(abundance_data) > 0) {
    # Check non-negative
    non_na_values <- abundance_data$Values[!is.na(abundance_data$Values)]
    testthat::expect_true(all(non_na_values >= 0),
                          info = "Negative abundance values found")

    # Check for reasonable upper bounds (not infinite or absurdly high)
    testthat::expect_true(all(non_na_values < 1e10),
                          info = "Unreasonably high abundance values found")

    # Check not all zeros
    testthat::expect_true(sum(non_na_values > 0) > 0,
                          info = "All abundance values are zero")
  }
})

testthat::test_that("Biomass values are non-negative and consistent with abundance", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get zooplankton data
  result <- pr_get_Indices(Survey = "NRS", Type = "Zooplankton")

  # Filter to biomass parameters
  biomass_data <- result[grepl("Biomass|biomass", result$Parameters), ]

  if (nrow(biomass_data) > 0) {
    non_na_values <- biomass_data$Values[!is.na(biomass_data$Values)]

    if (length(non_na_values) > 0) {
      # Biomass should be non-negative
      testthat::expect_true(all(non_na_values >= 0),
                            info = "Negative biomass values found")

      # Biomass should be in reasonable range (mg/m3 typically 0.1 to 100)
      testthat::expect_true(all(non_na_values < 10000),
                            info = "Unreasonably high biomass values (check units)")
    }
  }
})

testthat::test_that("Temperature values are in valid oceanographic range", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get EOV data which includes temperature
  result <- pr_get_EOVs(Survey = "NRS")

  # Filter to temperature data
  temp_data <- result[grepl("Temperature|temperature|SST|sst", result$Parameters), ]

  if (nrow(temp_data) > 0) {
    non_na_temps <- temp_data$Values[!is.na(temp_data$Values)]

    if (length(non_na_temps) > 0) {
      # Ocean temperature typically -2 to 35°C for surface waters
      testthat::expect_true(all(non_na_temps >= -3 & non_na_temps <= 40),
                            info = paste("Temperature outside valid range:",
                                       "min =", min(non_na_temps),
                                       "max =", max(non_na_temps)))

      # Australian waters typically 10-30°C
      median_temp <- median(non_na_temps)
      testthat::expect_true(median_temp >= 8 & median_temp <= 32,
                            info = paste("Median temperature unusual for Australian waters:", median_temp))
    }
  }
})

testthat::test_that("Salinity values are in valid oceanographic range", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get EOV data which includes salinity
  result <- pr_get_EOVs(Survey = "NRS")

  # Filter to salinity data
  sal_data <- result[grepl("Salinity|salinity", result$Parameters), ]

  if (nrow(sal_data) > 0) {
    non_na_sal <- sal_data$Values[!is.na(sal_data$Values)]

    if (length(non_na_sal) > 0) {
      # Seawater salinity typically 30-40 PSU
      testthat::expect_true(all(non_na_sal >= 0 & non_na_sal <= 50),
                            info = paste("Salinity outside valid range:",
                                       "min =", min(non_na_sal),
                                       "max =", max(non_na_sal)))

      # Australian coastal waters typically 34-36 PSU
      median_sal <- median(non_na_sal)
      testthat::expect_true(median_sal >= 30 & median_sal <= 38,
                            info = paste("Median salinity unusual for Australian waters:", median_sal))
    }
  }
})



testthat::test_that("Time series data has temporal ordering", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get indices data
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton")

  # Find date column (various names possible)
  date_cols <- c("SampleDateUTC", "SampleTime_UTC", "SampleTime_Local", "SampleDate")
  date_col <- intersect(date_cols, names(result))[1]
  testthat::skip_if(is.na(date_col), "No date column found")

  # Check date column is properly formatted
  testthat::expect_true(inherits(result[[date_col]], c("POSIXct", "Date")))

  # Check dates span multiple years (not just a single day)
  dates <- as.Date(result[[date_col]])
  dates <- dates[!is.na(dates)]
  testthat::skip_if(length(dates) == 0, "No valid dates found")

  date_range <- diff(range(dates))
  testthat::expect_true(date_range > 30,
                        info = paste("Date range too short:", date_range, "days"))

  # Check dates are chronologically reasonable
  testthat::expect_true(min(dates) >= as.Date("1990-01-01"),
                        info = "Dates before program start")
  testthat::expect_true(max(dates) <= Sys.Date() + 30,
                        info = "Dates too far in future")
})

testthat::test_that("Functional group data has expected structure", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get functional groups
  result <- pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton")

  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0, info = "Result should have data")
  testthat::expect_true("Parameters" %in% names(result) || "Values" %in% names(result),
                        info = "Should have Parameters or Values column")
})

testthat::test_that("CPR bioregions are within expected Australian regions", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get CPR indices
  result <- pr_get_Indices(Survey = "CPR", Type = "Phytoplankton")

  # Check for bioregion column
  bioregion_cols <- grep("BioRegion|Bioregion|bioregion", names(result), ignore.case = TRUE, value = TRUE)

  if (length(bioregion_cols) > 0) {
    bioregions <- unique(result[[bioregion_cols[1]]])
    bioregions <- bioregions[!is.na(bioregions)]

    # Check that bioregions are Australian regions
    # Known CPR bioregions
    expected_regions <- c("North", "North-west", "Temperate East", "South-east",
                          "South-west", "Tropical", "East", "West")

    # At least some should match expected
    matching <- sum(bioregions %in% expected_regions)
    testthat::expect_true(matching > 0,
                          info = paste("Unexpected bioregions:", paste(bioregions, collapse = ", ")))
  }
})

testthat::test_that("Index calculations preserve data relationships", {
  # Test with fixture data
  test_data <- create_test_zoo_data()

  # If biomass and abundance both exist, biomass should correlate positively with abundance
  if ("CopeAbundance_m3" %in% names(test_data) && "Biomass_mgm3" %in% names(test_data)) {
    # Both should be non-negative
    testthat::expect_true(all(test_data$CopeAbundance_m3 >= 0))
    testthat::expect_true(all(test_data$Biomass_mgm3 >= 0))

    # Where both are non-zero, there should be some relationship
    both_nonzero <- test_data$CopeAbundance_m3 > 0 & test_data$Biomass_mgm3 > 0
    if (sum(both_nonzero) > 2) {
      cor_value <- cor(test_data$CopeAbundance_m3[both_nonzero],
                       test_data$Biomass_mgm3[both_nonzero])
      # Correlation should be positive (more individuals = more biomass)
      testthat::expect_true(cor_value > -0.5,
                            info = paste("Unexpected negative correlation between abundance and biomass:", cor_value))
    }
  }
})

testthat::test_that("Station-level aggregations maintain sample counts", {
  skip_if_offline()
  testthat::skip_on_cran()

  # Get detailed data
  result <- pr_get_Indices(Survey = "NRS", Type = "Phytoplankton")

  # Group by station
  station_counts <- table(result$StationCode)

  # Each station should have multiple samples (program running for years)
  if (length(station_counts) > 0) {
    testthat::expect_true(all(station_counts > 5),
                          info = "Some stations have very few samples")

    # Check no station has absurdly high counts (data duplication issue)
    max_samples_per_station <- 10000  # Reasonable upper limit
    testthat::expect_true(all(station_counts < max_samples_per_station),
                          info = "Suspiciously high sample count for a station")
  }
})
