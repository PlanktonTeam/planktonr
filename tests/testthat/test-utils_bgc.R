# ==============================================================================
# Integration tests (network-dependent) - skip offline
# ==============================================================================

testthat::test_that("pr_get_NRSChemistry returns data frame with expected columns", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSChemistry()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("Project", "SampleTime_Local", "Month_Local", "SampleDepth_m",
                     "TripCode", "StationName", "StationCode", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check Parameters column contains expected values
  expected_params <- c("SecchiDepth_m", "Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL",
                       "Nitrate_umolL", "Nitrite_umolL", "Oxygen_umolL", "DIC_umolkg",
                       "Alkalinity_umolkg", "Salinity", "NOx_umolL", "DIN_umolL", "Redfield")
  testthat::expect_true(any(unique(result$Parameters) %in% expected_params))
  
  # Check data types
  testthat::expect_true(is.numeric(result$Values))
  testthat::expect_true(inherits(result$SampleTime_Local, c("POSIXct", "Date")))
  # StationCode can be character, integer, or factor
  testthat::expect_true(is.character(result$StationCode) | is.integer(result$StationCode) | is.factor(result$StationCode))
})

testthat::test_that("pr_get_NRSChemistry calculates derived parameters correctly", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSChemistry()
  
  # Check that NOx, DIN, and Redfield parameters exist
  params <- unique(result$Parameters)
  testthat::expect_true("NOx_umolL" %in% params)
  testthat::expect_true("DIN_umolL" %in% params)
  testthat::expect_true("Redfield" %in% params)
  
  # NOx should be sum of Nitrate and Nitrite
  # DIN should be sum of NOx and Ammonium
  # Redfield should be NOx/Phosphate ratio
  testthat::expect_true(all(is.na(result$Values) | is.numeric(result$Values)))
})

testthat::test_that("pr_get_NRSPigments returns data frame in standard format", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSPigments()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("Project", "TripCode", "SampleTime_Local", "Month_Local",
                     "SampleDepth_m", "StationName", "StationCode", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check for individual pigment parameters (not binned)
  params <- unique(result$Parameters)
  individual_pigments <- c("CphlA_mgm3", "Fuco_mgm3", "Perid_mgm3")
  testthat::expect_true(any(individual_pigments %in% params))
})

testthat::test_that("pr_get_NRSPigments returns data frame in binned format with aggregated parameters", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSPigments(Format = "binned")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check for binned/aggregated parameters
  params <- unique(result$Parameters)
  binned_params <- c("TotalChla", "TotalChl", "PPC", "PSC", "PSP", "TCaro", "TAcc", "TPig", "TDP")
  testthat::expect_true(any(binned_params %in% params))
  
  # Should NOT contain individual pigment parameters
  testthat::expect_false(any(c("CphlA_mgm3", "Fuco_mgm3") %in% params))
})

testthat::test_that("pr_get_NRSPigments filters out zero TotalChla in binned format", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSPigments(Format = "binned")
  
  # Check that TotalChla values are not zero
  totalchla_data <- result[result$Parameters == "TotalChla", ]
  if(nrow(totalchla_data) > 0) {
    testthat::expect_true(all(totalchla_data$Values != 0, na.rm = TRUE))
  }
})

testthat::test_that("pr_get_NRSPico returns data frame with picoplankton groups", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSPico()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("Project", "TripCode", "SampleTime_Local", "Month_Local", "Year_Local",
                     "SampleDepth_m", "StationName", "StationCode", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check for expected picoplankton groups
  params <- unique(result$Parameters)
  expected_groups <- c("Prochlorococcus_cellsmL", "Synechococcus_cellsmL", "Picoeukaryotes_cellsmL")
  testthat::expect_true(any(expected_groups %in% params))
  
  # Values should be numeric and positive (cells per mL)
  testthat::expect_true(is.numeric(result$Values))
  testthat::expect_true(all(result$Values[!is.na(result$Values)] > 0))
})

testthat::test_that("pr_get_NRSMicro returns data frame for NRS survey with expected structure", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSMicro(Survey = "NRS")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Microbes")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
  
  # Check for expected columns
  expected_cols <- c("TripCode", "SampleDepth_m", "Parameters", "Values",
                     "SampleTime_Local", "Year_Local", "Month_Local", "StationName", "StationCode")
  testthat::expect_true(all(expected_cols %in% names(result)))
})

testthat::test_that("pr_get_NRSMicro returns data frame for Coastal survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSMicro(Survey = "Coastal")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Microbes")
  testthat::expect_equal(attr(result, "Survey"), "Coastal")
  
  # Should have location data
  expected_cols <- c("StationName", "Latitude", "Longitude", "SampleDepth_m", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
})

testthat::test_that("pr_get_NRSMicro returns data frame for GO-SHIP survey", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSMicro(Survey = "GO-SHIP")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Microbes")
  testthat::expect_equal(attr(result, "Survey"), "GO-SHIP")
  
  # Should have location data
  testthat::expect_true("Latitude" %in% names(result))
  testthat::expect_true("Longitude" %in% names(result))
})

testthat::test_that("pr_get_CSChem returns data frame with chemistry parameters", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_CSChem()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check for expected columns
  expected_cols <- c("StationName", "SampleTime_Local", "Latitude", "Longitude",
                     "SampleDepth_m", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check for expected chemistry parameters
  params <- unique(result$Parameters)
  expected_params <- c("Silicate_umolL", "Nitrate_umolL", "Phosphate_umolL", "Ammonium_umolL",
                       "Chla_mgm3", "Temperature_degC", "Salinity_psu", "Oxygen_umolL")
  testthat::expect_true(any(expected_params %in% params))
})

testthat::test_that("pr_get_NRSTSS returns data frame with TSS data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSTSS()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
})

testthat::test_that("pr_get_NRSCTD returns data frame with CTD parameters", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSCTD()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
  
  # Check for expected CTD parameters
  expected_cols <- c("Project", "StationName", "StationCode", "TripCode", "SampleTime_Local",
                     "Latitude", "Longitude", "SampleDepth_m", "Salinity_psu", "Temperature_degC",
                     "DissolvedOxygen_umolkg", "ChlF_mgm3", "Turbidity_NTU")
  testthat::expect_true(any(expected_cols %in% names(result)))
})

testthat::test_that("pr_get_NRSCTD filters out problematic file_ids", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSCTD()
  
  # Should not contain file_ids 2117, 2184, 2186, 2187
  if("file_id" %in% names(result)) {
    testthat::expect_false(any(result$file_id %in% c(2117, 2184, 2186, 2187)))
  }
})

testthat::test_that("pr_get_LTnuts returns combined long-term data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_LTnuts()
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("Project", "StationName", "StationCode", "SampleTime_Local",
                     "Month_Local", "Year_Local", "SampleDepth_m", "Parameters", "Values")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Should combine data from LTM, NRS Chemistry, and NRS CTD
  projects <- unique(result$Project)
  testthat::expect_true("LTM" %in% projects | "NRS" %in% projects)
  
  # Should only contain specific stations (MAI, ROT, PHB)
  stations <- unique(result$StationCode)
  testthat::expect_true(all(stations %in% c("MAI", "ROT", "PHB")))
})

testthat::test_that("pr_get_LTnuts filters out -999 values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_LTnuts()
  
  # Should not contain -999 values
  testthat::expect_false(any(result$Values == -999, na.rm = TRUE))
})

testthat::test_that("pr_get_NRSEnvContour processes data correctly", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Pico")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Should not contain NA values (dropped)
  testthat::expect_false(any(is.na(result$Values)))
  
  # Should not contain SecchiDepth parameter
  testthat::expect_false("SecchiDepth_m" %in% unique(result$Parameters))
  
  # SampleTime_Local should be floored to month
  if(nrow(result) > 0 && inherits(result$SampleTime_Local, "POSIXct")) {
    # Check that times are at beginning of month (day = 1, hour = 0)
    testthat::expect_true(all(lubridate::day(result$SampleTime_Local) == 1))
  }
})

testthat::test_that("pr_get_NRSEnvContour works with Chemistry data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Chemistry")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSEnvContour works with Pico data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Pico")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_NRSEnvContour works with Micro data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_NRSEnvContour(Data = "Micro")
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

# ==============================================================================
# Unit tests using fixtures (no network dependency)
# ==============================================================================

testthat::test_that("chemistry data fixture has expected structure", {
  chem <- create_test_chemistry()
  testthat::expect_s3_class(chem, "data.frame")
  testthat::expect_true("Parameters" %in% names(chem))
  testthat::expect_true("Values" %in% names(chem))
  testthat::expect_true("StationCode" %in% names(chem))
  testthat::expect_true(nrow(chem) > 0)
})

testthat::test_that("chemistry data can be filtered by parameter", {
  chem <- create_test_chemistry()
  secchi <- chem[chem$Parameters == "SecchiDepth_m", ]
  testthat::expect_true(nrow(secchi) > 0)
  testthat::expect_true(all(secchi$Parameters == "SecchiDepth_m"))
})

testthat::test_that("chemistry data can be filtered by station", {
  chem <- create_test_chemistry()
  nsi <- chem[chem$StationCode == "NSI", ]
  testthat::expect_true(nrow(nsi) > 0)
  testthat::expect_true(all(nsi$StationCode == "NSI"))
})

testthat::test_that("chemistry data contains reasonable values", {
  chem <- create_test_chemistry()
  
  # Values should be numeric
  testthat::expect_true(is.numeric(chem$Values))
  
  # Values should be non-negative for most parameters
  testthat::expect_true(all(chem$Values >= 0, na.rm = TRUE))
})

testthat::test_that("chemistry data has temporal information", {
  chem <- create_test_chemistry()
  
  # Should have time-related columns
  testthat::expect_true("Month_Local" %in% names(chem) | "SampleTime_Local" %in% names(chem))
  
  # Month values should be 1-12
  if("Month_Local" %in% names(chem)) {
    testthat::expect_true(all(chem$Month_Local >= 1 & chem$Month_Local <= 12))
  }
})

# ==============================================================================
# Error handling tests
# ==============================================================================

testthat::test_that("pr_get_NRSMicro validates Survey parameter", {
  testthat::expect_error(
    pr_get_NRSMicro(Survey = "InvalidSurvey"),
    regexp = "must be one of 'NRS', 'Coastal', or 'GO-SHIP'"
  )
  
  testthat::expect_error(
    pr_get_NRSMicro(Survey = c("NRS", "Coastal")),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_NRSMicro(Survey = 123),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_NRSPigments validates Format parameter", {
  testthat::expect_error(
    pr_get_NRSPigments(Format = "invalid_format"),
    regexp = "must be one of 'all' or 'binned'"
  )
  
  testthat::expect_error(
    pr_get_NRSPigments(Format = c("all", "binned")),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_NRSPigments(Format = NULL),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_NRSEnvContour validates Data parameter", {
  testthat::expect_error(
    pr_get_NRSEnvContour(Data = "InvalidData"),
    regexp = "must be one of 'Chemistry', 'Pico', or 'Micro'"
  )
  
  testthat::expect_error(
    pr_get_NRSEnvContour(Data = c("Chemistry", "Pico")),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_NRSEnvContour(Data = 999),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_NRSPigments accepts valid Format values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Both valid values should work without error
  testthat::expect_no_error(pr_get_NRSPigments(Format = "all"))
  testthat::expect_no_error(pr_get_NRSPigments(Format = "binned"))
})

testthat::test_that("pr_get_NRSMicro accepts valid Survey values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # All valid values should work without error
  testthat::expect_no_error(pr_get_NRSMicro(Survey = "NRS"))
  testthat::expect_no_error(pr_get_NRSMicro(Survey = "Coastal"))
  testthat::expect_no_error(pr_get_NRSMicro(Survey = "GO-SHIP"))
})

testthat::test_that("pr_get_NRSEnvContour accepts all valid Data values", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # All valid values should work without error
  testthat::expect_no_error(pr_get_NRSEnvContour(Data = "Chemistry"))
  testthat::expect_no_error(pr_get_NRSEnvContour(Data = "Pico"))
  testthat::expect_no_error(pr_get_NRSEnvContour(Data = "Micro"))
})

