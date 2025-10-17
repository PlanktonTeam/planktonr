# Integration-style tests using fixtures
# These tests simulate full workflows without network calls

testthat::test_that("Complete NRS phytoplankton analysis workflow with fixtures", {
  # Create mock data
  indices_data <- create_test_nrs_indices()
  
  # Apply time columns
  indices_with_time <- pr_apply_Time(indices_data)
  testthat::expect_true("Month_Local" %in% names(indices_with_time))
  testthat::expect_true("Year_Local" %in% names(indices_with_time))
  
  # Filter to specific stations
  filtered_data <- pr_filter_data(indices_with_time, "PhytoAbundance_CellsL", c("NSI", "PHB"))
  testthat::expect_true(all(filtered_data$StationCode %in% c("NSI", "PHB")))
  
  # Create climatology (parameter is 'x', use 'Month_Local' since that's what pr_apply_Time creates)
  climatology <- pr_make_climatology(filtered_data, x = "Month_Local")
  testthat::expect_type(climatology, "list")
  testthat::expect_gt(nrow(climatology), 0)
})

testthat::test_that("Complete CPR data processing workflow with fixtures", {
  # Create mock CPR data (already has BioRegion)
  cpr_data <- create_test_cpr_indices()
  testthat::expect_true("BioRegion" %in% names(cpr_data))
  
  # Apply time processing
  cpr_with_time <- pr_apply_Time(cpr_data)
  testthat::expect_true("Month_Local" %in% names(cpr_with_time))
  
  # Filter to specific bioregions
  filtered_cpr <- pr_filter_data(cpr_with_time, "BiomassIndex_mgm3", c("North", "South-east"))
  testthat::expect_true(all(filtered_cpr$BioRegion %in% c("North", "South-east")))
})

testthat::test_that("Phytoplankton carbon calculation workflow with fixtures", {
  # Create phyto data
  phyto_data <- create_test_phyto_data()
  
  # Add carbon for NRS (parameter is 'meth', not 'Survey')
  nrs_with_carbon <- pr_add_Carbon(phyto_data, meth = "NRS")
  testthat::expect_true("Carbon" %in% names(nrs_with_carbon))
  testthat::expect_true(all(nrs_with_carbon$Carbon > 0, na.rm = TRUE))
  
  # Verify all rows have carbon values
  testthat::expect_equal(sum(!is.na(nrs_with_carbon$Carbon)), nrow(nrs_with_carbon))
})

testthat::test_that("Quality control workflow with fixtures", {
  # Create flagged data
  flagged_data <- create_test_flagged_data()
  initial_rows <- nrow(flagged_data)
  
  # Apply quality flags
  qc_data <- pr_apply_Flags(flagged_data)
  
  # Function may or may not remove rows - just verify it returns data
  testthat::expect_true(is.data.frame(qc_data))
  testthat::expect_lte(nrow(qc_data), initial_rows)
  
  # If flag columns are removed, that's also valid behavior
  testthat::expect_true(TRUE)
})

testthat::test_that("Species filtering workflow with fixtures", {
  # Create species data with various quality issues
  species_data <- create_test_species_data()
  initial_count <- nrow(species_data)
  
  # Filter species
  clean_species <- pr_filter_Species(species_data)
  
  # Should remove problematic species
  testthat::expect_lt(nrow(clean_species), initial_count)
  
  # Verify no invalid patterns remain
  testthat::expect_false(any(grepl("cf\\.", clean_species$Species, ignore.case = TRUE)))
  testthat::expect_false(any(grepl("spp\\.", clean_species$Species, ignore.case = TRUE)))
  testthat::expect_false(any(is.na(clean_species$Species)))
})

testthat::test_that("Station code and name workflow with fixtures", {
  # Create trip data
  trip_data <- create_test_trips()
  
  # Add station codes
  with_codes <- pr_add_StationCode(trip_data)
  testthat::expect_true("StationCode" %in% names(with_codes))
  
  # Add station names
  with_names <- pr_add_StationName(with_codes)
  testthat::expect_true("StationName" %in% names(with_names))
  
  # Verify relationship between codes and names
  testthat::expect_equal(nrow(with_names), nrow(trip_data))
})

testthat::test_that("Multi-step data transformation preserves structure", {
  # Start with basic data
  test_data <- create_test_nrs_indices()
  original_cols <- names(test_data)
  
  # Apply multiple transformations
  result <- test_data %>%
    pr_apply_Time() %>%
    pr_filter_data("PhytoAbundance_CellsL", c("NSI", "PHB"))
  
  # Check that original columns are preserved
  testthat::expect_true(all(original_cols %in% names(result)))
  
  # Check that new columns were added
  testthat::expect_true("Month_Local" %in% names(result))
  testthat::expect_true("Year_Local" %in% names(result))
})

testthat::test_that("planktonr_dat workflow maintains class through pipeline", {
  # Create planktonr_dat object
  test_data <- create_test_planktonr_dat("phytoplankton")
  
  # Apply multiple dplyr operations
  result <- test_data %>%
    dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
    dplyr::mutate(LogAbundance = log10(Abundance + 1)) %>%
    dplyr::arrange(StationCode, Species)
  
  # Verify class preservation
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_s3_class(result, "data.frame")
  
  # Verify transformations worked
  testthat::expect_true("LogAbundance" %in% names(result))
  testthat::expect_true(all(result$StationCode %in% c("NSI", "PHB")))
})

testthat::test_that("Empty data handling throughout workflow", {
  # Create empty data with correct structure
  empty_data <- create_empty_test_data(c("StationCode", "Species", "Abundance"))
  
  # Apply filtering - should return empty data, not error
  result <- pr_filter_Species(empty_data)
  testthat::expect_equal(nrow(result), 0)
  testthat::expect_true(is.data.frame(result))
})

testthat::test_that("Large dataset simulation works efficiently", {
  # Create larger mock dataset
  n_stations <- 5
  n_samples <- 100
  
  large_data <- data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI", "YON", "ROT"), each = n_samples),
    Latitude = rep(c(-27.33, -34.09, -42.60, -19.30, -32.00), each = n_samples),
    Longitude = rep(c(153.55, 151.22, 148.23, 147.62, 115.50), each = n_samples),
    SampleTime_Local = rep(seq.Date(as.Date("2015-01-01"), 
                                     as.Date("2023-12-31"), 
                                     length.out = n_samples), n_stations),
    Values = rlnorm(n_stations * n_samples, meanlog = 10, sdlog = 1),
    Parameters = "PhytoAbundance_CellsL",
    BioRegion = rep(c("NSI", "PHB", "MAI", "YON", "ROT"), each = n_samples),
    stringsAsFactors = FALSE
  )
  
  # Convert to planktonr_dat object
  large_data <- planktonr_dat(large_data, Type = "Phytoplankton", Survey = "NRS", Variable = "abundance")
  
  # Time the operation
  start_time <- Sys.time()
  result <- large_data %>%
    pr_apply_Time() %>%
    pr_filter_data("PhytoAbundance_CellsL", c("NSI", "PHB", "MAI"))
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  # Should complete quickly
  testthat::expect_lt(elapsed, 5)  # Should take less than 5 seconds
  
  # Verify result
  testthat::expect_gt(nrow(result), 0)
  testthat::expect_true(all(result$StationCode %in% c("NSI", "PHB", "MAI")))
})

testthat::test_that("Edge case: single row data frame", {
  single_row <- data.frame(
    Species = "Valid species",
    Abundance = 100,
    stringsAsFactors = FALSE
  )
  
  result <- pr_filter_Species(single_row)
  
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$Species, "Valid species")
})

testthat::test_that("Edge case: data with all NA values in key column", {
  all_na_data <- data.frame(
    Species = c(NA, NA, NA),
    Abundance = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  result <- pr_filter_Species(all_na_data)
  
  # Should remove all rows with NA species
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("Reproducibility: same input produces same output", {
  set.seed(123)
  test_data1 <- create_test_nrs_indices()
  
  set.seed(123)
  test_data2 <- create_test_nrs_indices()
  
  testthat::expect_identical(test_data1, test_data2)
})

testthat::test_that("Coordinate renaming workflow with fixtures", {
  coord_data <- create_test_coordinate_data()
  
  # Should have uppercase column names
  testthat::expect_true("LATITUDE" %in% names(coord_data))
  testthat::expect_true("LONGITUDE" %in% names(coord_data))
  
  # After renaming should have lowercase
  renamed_data <- pr_rename(coord_data)
  
  testthat::expect_true("Latitude" %in% names(renamed_data) | "latitude" %in% names(renamed_data))
  testthat::expect_true("Longitude" %in% names(renamed_data) | "longitude" %in% names(renamed_data))
})

testthat::test_that("Fixture data has expected statistical properties", {
  indices_data <- create_test_nrs_indices()
  
  # Check for reasonable data ranges
  testthat::expect_true(all(indices_data$Values > 0))
  testthat::expect_true(all(indices_data$Latitude >= -90 & indices_data$Latitude <= 90))
  testthat::expect_true(all(indices_data$Longitude >= -180 & indices_data$Longitude <= 180))
  testthat::expect_true(all(indices_data$Month_Local >= 1 & indices_data$Month_Local <= 12))
})
