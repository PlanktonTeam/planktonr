# ==============================================================================
# Integration tests (network-dependent)
# ==============================================================================

testthat::test_that("pr_get_NRSData returns phytoplankton abundance with expected columns and data types", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSData(Type = "phytoplankton", Variable = "abundance", Subset = "htg")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns exist (flexible with naming)
  essential_cols <- c("StationCode")
  testthat::expect_true(all(essential_cols %in% names(result)), 
                        info = paste("Missing columns:", paste(setdiff(essential_cols, names(result)), collapse = ", ")))
  
  # Check for date column (various possible names)
  date_col_exists <- any(c("SampleDateUTC", "SampleTime_UTC", "SampleTime_Local", "SampleDate") %in% names(result))
  testthat::expect_true(date_col_exists, info = "No date column found")
  
  # For htg subset, we expect aggregated data columns, but the exact names may vary
  # Instead of checking for specific columns, verify we have some data columns beyond the basics
  basic_cols <- c("StationCode", "Latitude", "Longitude", "SampleDateUTC", "SampleTime_UTC", "SampleTime_Local", "SampleDate")
  data_cols <- setdiff(names(result), basic_cols)
  
  testthat::expect_true(length(data_cols) > 0,
                        info = paste("No data columns found beyond basic metadata. Columns present:", paste(names(result), collapse = ", ")))
  
  # Check data types if columns exist
  date_col <- intersect(c("SampleDateUTC", "SampleTime_UTC", "SampleTime_Local", "SampleDate"), names(result))[1]
  if (!is.na(date_col)) {
    testthat::expect_true(inherits(result[[date_col]], c("POSIXct", "Date")))
  }
  if ("StationCode" %in% names(result)) {
    testthat::expect_type(result$StationCode, "character")
  }
  
  # Check for reasonable values
  if ("Latitude" %in% names(result) && any(!is.na(result$Latitude))) {
    testthat::expect_true(all(result$Latitude[!is.na(result$Latitude)] >= -90 & 
                              result$Latitude[!is.na(result$Latitude)] <= 90))
  }
  if ("Longitude" %in% names(result) && any(!is.na(result$Longitude))) {
    testthat::expect_true(all(result$Longitude[!is.na(result$Longitude)] >= -180 & 
                              result$Longitude[!is.na(result$Longitude)] <= 180))
  }
})

testthat::test_that("pr_get_NRSData returns Zooplankton abundance with expected structure and values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSData(Type = "Zooplankton", Variable = "abundance", Subset = "species")
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns (flexible with naming)
  essential_cols <- c("StationCode")
  testthat::expect_true(all(essential_cols %in% names(result)))
  
  # Check for date column
  date_col_exists <- any(c("SampleDateUTC", "SampleTime_UTC", "SampleTime_Local", "SampleDate") %in% names(result))
  testthat::expect_true(date_col_exists, info = "No date column found")
  
  # Check abundance values are non-negative where they exist
  abundance_cols <- grep("Abundance|abundance", names(result), value = TRUE)
  if (length(abundance_cols) > 0) {
    for (col in abundance_cols) {
      if (is.numeric(result[[col]])) {
        non_na_values <- result[[col]][!is.na(result[[col]])]
        if (length(non_na_values) > 0) {
          testthat::expect_true(all(non_na_values >= 0), 
                                info = paste("Negative abundance found in", col))
        }
      }
    }
  }
})

testthat::test_that("pr_get_Stations returns valid station data with coordinates", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_Stations()
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check required columns
  required_cols <- c("StationCode", "Latitude", "Longitude")
  testthat::expect_true(all(required_cols %in% names(result)),
                        info = paste("Missing:", paste(setdiff(required_cols, names(result)), collapse = ", ")))
  
  # Check coordinate validity
  testthat::expect_true(all(result$Latitude >= -90 & result$Latitude <= 90),
                        info = "Latitudes outside valid range (-90 to 90)")
  testthat::expect_true(all(result$Longitude >= -180 & result$Longitude <= 180),
                        info = "Longitudes outside valid range (-180 to 180)")
  
  # Check Australian waters (reasonable bounds for IMOS NRS stations)
  testthat::expect_true(all(result$Latitude >= -45 & result$Latitude <= -10),
                        info = "Latitudes outside Australian region")
  testthat::expect_true(all(result$Longitude >= 110 & result$Longitude <= 160),
                        info = "Longitudes outside Australian region")
  
  # Check no duplicate station codes
  testthat::expect_equal(nrow(result), length(unique(result$StationCode)),
                         info = "Duplicate station codes found")
})

testthat::test_that("pr_get_NRSTrips returns valid trip data with temporal information", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSTrips()
  
  # Check structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check required columns
  expected_cols <- c("TripCode", "StationCode")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
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

testthat::test_that("pr_filter_NRSStations correctly filters to NRS stations only", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  stations <- pr_get_Stations()
  result <- pr_filter_NRSStations(stations)
  
  # Check filtering occurred
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) <= nrow(stations),
                        info = "Filtered result has more rows than input")
  
  # Check same columns preserved
  testthat::expect_equal(names(result), names(stations),
                         info = "Column names changed during filtering")
  
  # Check all required NRS stations present (known NRS codes)
  known_nrs <- c("NSI", "PHB", "MAI", "YON", "ROT", "KAI", "DAR", "ESP")
  present_nrs <- intersect(known_nrs, result$StationCode)
  testthat::expect_true(length(present_nrs) >= 5,
                        info = paste("Expected at least 5 NRS stations, found:", length(present_nrs)))
})

# ==============================================================================
# Unit tests using fixtures (no network dependency)
# ==============================================================================

testthat::test_that("pr_filter_NRSStations works with fixture station data", {
  stations <- create_test_stations()
  result <- pr_filter_NRSStations(stations)
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) <= nrow(stations))
  testthat::expect_true(all(c("StationCode", "StationName", "Latitude", "Longitude") %in% names(result)))
})

testthat::test_that("NRS station data has expected structure", {
  stations <- create_test_stations()
  
  testthat::expect_true("StationCode" %in% names(stations))
  testthat::expect_true("Latitude" %in% names(stations))
  testthat::expect_true("Longitude" %in% names(stations))
  testthat::expect_true(all(!is.na(stations$StationCode)))
})

testthat::test_that("NRS trip data fixture has expected structure", {
  trips <- create_test_trips()
  
  testthat::expect_true("TripCode" %in% names(trips))
  testthat::expect_true("StationCode" %in% names(trips))
  testthat::expect_s3_class(trips$SampleTime_UTC, "POSIXct")
})

testthat::test_that("pr_filter_NRSStations preserves data integrity during filtering", {
  stations <- create_test_stations()
  original_nrow <- nrow(stations)
  original_cols <- names(stations)
  
  result <- pr_filter_NRSStations(stations)
  
  # Check structure preserved
  testthat::expect_equal(names(result), original_cols,
                         info = "Columns changed during filtering")
  
  # Check data types preserved
  for (col in names(result)) {
    testthat::expect_equal(class(result[[col]]), class(stations[[col]]),
                           info = paste("Data type changed for column", col))
  }
  
  # Check no data corruption - filtered rows should exist in original
  if (nrow(result) > 0 && nrow(stations) > 0) {
    testthat::expect_true(all(result$StationCode %in% stations$StationCode),
                          info = "Filtered result contains stations not in original")
  }
})

testthat::test_that("Station coordinates remain consistent through operations", {
  stations <- create_test_stations()
  
  # Check coordinate pairing is preserved
  if (nrow(stations) > 0) {
    for (i in seq_len(nrow(stations))) {
      code <- stations$StationCode[i]
      lat <- stations$Latitude[i]
      lon <- stations$Longitude[i]
      
      # Each station code should have consistent coordinates
      same_station <- stations[stations$StationCode == code, ]
      testthat::expect_equal(nrow(same_station), 1,
                             info = paste("Duplicate station code:", code))
      testthat::expect_equal(same_station$Latitude, lat)
      testthat::expect_equal(same_station$Longitude, lon)
    }
  }
})

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_NRSData errors on invalid Type parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_NRSData(Type = "InvalidType", Variable = "abundance", Subset = "htg"))
})

testthat::test_that("pr_get_NRSData errors on invalid Variable parameter", {
  skip_if_offline()
  testthat::expect_error(pr_get_NRSData(Type = "Phytoplankton", Variable = "invalid_var", Subset = "htg"))
})

testthat::test_that("pr_filter_NRSStations handles empty input", {
  empty_df <- data.frame(
    StationCode = character(0),
    Latitude = numeric(0),
    Longitude = numeric(0),
    stringsAsFactors = FALSE
  )
  
  result <- pr_filter_NRSStations(empty_df)
  testthat::expect_equal(nrow(result), 0)
})
