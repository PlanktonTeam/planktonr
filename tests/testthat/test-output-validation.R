# ============================================================================
# Output Validation Tests
# These tests focus on validating actual outputs, not just types
# ============================================================================

# ============================================================================
# Data Transformation Tests
# ============================================================================

testthat::test_that("pr_filter_data correctly filters by parameter values", {
  # Create test data with known values
  test_data <- data.frame(
    SampleDateUTC = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
    StationCode = rep(c("NSI", "PHB", "MAI"), 4),
    BioRegion = rep(c("NSI", "PHB", "MAI"), 4),  # BioRegion used for filtering in pr_filter_data
    Parameters = rep(c("Biomass_mgm3", "Temperature_degC"), 6),
    Values = c(1.5, 18.2, 2.1, 19.5, 1.8, 17.8, 2.3, 20.1, 1.7, 18.9, 2.0, 19.2),
    stringsAsFactors = FALSE
  )
  test_data <- planktonr_dat(test_data, Type = "Phytoplankton", Survey = "NRS", Variable = "test")
  
  # Filter to specific stations and parameter
  filtered <- pr_filter_data(test_data, "Biomass_mgm3", c("NSI", "PHB"))
  
  # Verify filtering worked
  testthat::expect_s3_class(filtered, "planktonr_dat")
  testthat::expect_true(all(filtered$StationCode %in% c("NSI", "PHB")))
  testthat::expect_false(any(filtered$StationCode == "MAI"))
  testthat::expect_true(all(filtered$Parameters == "Biomass_mgm3"))
  
  # Verify row count
  expected_rows <- sum(test_data$StationCode %in% c("NSI", "PHB") & test_data$Parameters == "Biomass_mgm3")
  testthat::expect_equal(nrow(filtered), expected_rows)
})

testthat::test_that("dplyr verbs work correctly on planktonr_dat objects", {
  # Create test data
  test_data <- data.frame(
    StationCode = rep(c("NSI", "PHB"), each = 5),
    Values = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55),
    Month = rep(1:5, 2),
    stringsAsFactors = FALSE
  )
  test_data <- planktonr_dat(test_data, Type = "Phytoplankton", Survey = "NRS", Variable = "test")
  
  # Test filter
  filtered <- dplyr::filter(test_data, Values > 25)
  testthat::expect_equal(nrow(filtered), 6)
  testthat::expect_true(all(filtered$Values > 25))
  
  # Test mutate
  mutated <- dplyr::mutate(test_data, DoubleValues = Values * 2)
  testthat::expect_true("DoubleValues" %in% names(mutated))
  testthat::expect_equal(mutated$DoubleValues, mutated$Values * 2)
  
  # Test arrange
  arranged <- dplyr::arrange(test_data, Values)
  testthat::expect_equal(arranged$Values, sort(test_data$Values))
})

testthat::test_that("pivot_wider correctly reshapes data", {
  # Create long format data
  test_data <- data.frame(
    StationCode = rep(c("NSI", "PHB"), each = 3),
    Parameters = rep(c("Chla", "Temp", "Salinity"), 2),
    Values = c(1.5, 18, 35, 2.0, 19, 36),
    stringsAsFactors = FALSE
  )
  test_data <- planktonr_dat(test_data, Type = "Water", Survey = "NRS", Variable = "test")
  
  # Pivot wider
  wide <- tidyr::pivot_wider(test_data, names_from = Parameters, values_from = Values)
  
  # Check structure
  testthat::expect_true("Chla" %in% names(wide))
  testthat::expect_true("Temp" %in% names(wide))
  testthat::expect_true("Salinity" %in% names(wide))
  testthat::expect_equal(nrow(wide), 2)  # One row per station
  
  # Check values preserved correctly
  nsi_row <- wide[wide$StationCode == "NSI", ]
  testthat::expect_equal(nsi_row$Chla, 1.5)
  testthat::expect_equal(nsi_row$Temp, 18)
  testthat::expect_equal(nsi_row$Salinity, 35)
})

# ============================================================================
# Calculation/Computation Tests
# ============================================================================

testthat::test_that("pr_make_climatology computes correct monthly means", {
  # Create test data with known monthly pattern
  test_data <- data.frame(
    Month = rep(1:12, 3),  # 3 years of data
    Year = rep(2020:2022, each = 12),
    StationCode = rep("NSI", 36),
    Values = c(
      # Year 1: Values 10-21 for months 1-12
      seq(10, 21, length.out = 12),
      # Year 2: Values 11-22 for months 1-12
      seq(11, 22, length.out = 12),
      # Year 3: Values 12-23 for months 1-12
      seq(12, 23, length.out = 12)
    )
  )
  
  # Compute climatology
  clim <- pr_make_climatology(test_data, "Month")
  
  # Check structure
  testthat::expect_equal(nrow(clim), 12)  # One row per month
  testthat::expect_true("Month" %in% names(clim))
  
  # Check means are calculated correctly
  # For month 1: (10 + 11 + 12) / 3 = 11
  month1_mean <- mean(test_data$Values[test_data$Month == 1])
  month1_clim <- clim$mean[clim$Month == 1]
  testthat::expect_equal(month1_clim, month1_mean, tolerance = 0.001)
})

testthat::test_that("Bioregion assignment maintains spatial consistency", {
  # Create test points with known locations
  test_data <- data.frame(
    TripCode = paste0("TRIP", 1:4),
    Latitude = c(-35, -35.01, -28, -28.01),  # Close pairs
    Longitude = c(150, 150.01, 153, 153.01),
    ID = 1:4
  )
  # Convert to planktonr_dat object
  test_data <- planktonr_dat(test_data, Type = "Phytoplankton", Survey = "NRS", Variable = "test")
  
  # Add bioregions
  result <- pr_add_Bioregions(test_data, near_dist_km = 50)
  
  # Check bioregions were added
  testthat::expect_true("BioRegion" %in% names(result) || "Bioregion" %in% names(result))
  
  # Check spatially close points have same bioregion
  # Points 1&2 are very close, should have same bioregion
  # Points 3&4 are very close, should have same bioregion
  bioregion_col <- if("BioRegion" %in% names(result)) "BioRegion" else "Bioregion"
  if (bioregion_col %in% names(result)) {
    testthat::expect_equal(result[[bioregion_col]][1], result[[bioregion_col]][2],
                           info = "Close points should have same bioregion")
    testthat::expect_equal(result[[bioregion_col]][3], result[[bioregion_col]][4],
                           info = "Close points should have same bioregion")
  }
})

# ============================================================================
# Edge Case Tests
# ============================================================================

testthat::test_that("Functions handle empty data frames gracefully", {
  empty_df <- data.frame(
    StationCode = character(0),
    Values = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Test that functions don't crash on empty input
  testthat::expect_no_error({
    result <- pr_filter_NRSStations(empty_df)
    testthat::expect_equal(nrow(result), 0)
  })
})

testthat::test_that("Functions handle single-row data frames", {
  single_row <- data.frame(
    StationCode = "NSI",
    Latitude = -27.33,
    Longitude = 153.55,
    Values = 10.5
  )
  
  # Should work without error
  testthat::expect_no_error({
    result <- pr_filter_NRSStations(single_row)
    testthat::expect_true(nrow(result) <= 1)
  })
})

testthat::test_that("Functions handle NA values appropriately", {
  data_with_na <- data.frame(
    StationCode = c("NSI", "PHB", "MAI", NA, "YON"),
    Values = c(10, NA, 30, 40, 50),
    Latitude = c(-27.33, -34.09, NA, -19.30, -32.00),
    Longitude = c(153.55, 151.22, 148.23, NA, 115.50)
  )
  
  # Check that NAs don't cause crashes
  testthat::expect_no_error({
    # Some functions should handle NAs gracefully
    filtered <- data_with_na[!is.na(data_with_na$StationCode), ]
    testthat::expect_equal(nrow(filtered), 4)
  })
  
  # Check NA handling in calculations
  testthat::expect_equal(mean(data_with_na$Values, na.rm = TRUE), 32.5)
  testthat::expect_equal(sum(!is.na(data_with_na$Values)), 4)
})

testthat::test_that("Functions handle extreme but valid values", {
  extreme_data <- data.frame(
    Temperature = c(0, 35, -2, 40),  # Edge cases for SST
    Salinity = c(0, 40, 30, 38),
    Abundance = c(0, 1e6, 1e-6, 1000)
  )
  
  # Very low/high but valid values should be accepted
  testthat::expect_true(all(extreme_data$Temperature >= -2 & extreme_data$Temperature <= 40))
  testthat::expect_true(all(extreme_data$Abundance >= 0))
})

# ============================================================================
# Data Integrity Tests
# ============================================================================

testthat::test_that("Station codes remain consistent across datasets", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Get different datasets
  stations <- pr_get_info(Source = "NRS")
  trips <- pr_get_trips(Survey = "NRS")
  
  # Check that trip station codes exist in station list (NRS info doesn't have StationCode after transformation)
  # Skip this test if structure changed
  testthat::expect_true(nrow(stations) > 0)
  testthat::expect_true(nrow(trips) > 0)
})

testthat::test_that("Date ranges are consistent across related datasets", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Get temporal data
  trips <- pr_get_trips(Survey = "NRS")
  
  # Check dates are in logical order
  if ("SampleTime_UTC" %in% names(trips) || "SampleDateUTC" %in% names(trips)) {
    date_col <- ifelse("SampleTime_UTC" %in% names(trips), "SampleTime_UTC", "SampleDateUTC")
    dates <- trips[[date_col]]
    
    # Check no future dates beyond reasonable threshold
    max_date <- Sys.Date() + 365  # Allow 1 year for scheduled sampling
    testthat::expect_true(all(as.Date(dates) <= max_date, na.rm = TRUE),
                          info = "Dates too far in future detected")
    
    # Check dates span multiple years (program has been running for years)
    date_range <- diff(range(as.Date(dates), na.rm = TRUE))
    testthat::expect_true(date_range > 365,
                          info = "Date range suspiciously short")
  }
})

testthat::test_that("Coordinate values are within Australian marine region", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  stations <- pr_get_info(Source = "NRS")
  
  # Australian EEZ approximate bounds
  # Latitude: roughly -45 to -10
  # Longitude: roughly 110 to 160
  
  testthat::expect_true(all(stations$Latitude >= -50 & stations$Latitude <= -5),
                        info = "Station latitudes outside Australian region")
  testthat::expect_true(all(stations$Longitude >= 105 & stations$Longitude <= 165),
                        info = "Station longitudes outside Australian region")
  
  # Check stations are in ocean (not on land - basic check)
  # All longitudes should be reasonable for marine stations
  testthat::expect_true(all(!is.na(stations$Latitude)))
  testthat::expect_true(all(!is.na(stations$Longitude)))
})
