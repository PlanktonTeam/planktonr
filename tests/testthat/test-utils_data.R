# ==============================================================================
# Tests for pr_get_data() - unified data retrieval function
# ==============================================================================

# ==============================================================================
# Unit tests - Input validation (no network dependency)
# ==============================================================================

testthat::test_that("pr_get_data validates Survey parameter", {
  testthat::expect_error(
    pr_get_data(Survey = "InvalidSurvey", Type = "Phytoplankton", Variable = "abundance", Subset = "raw"),
    regexp = "Survey.*must be one of"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = c("NRS", "CPR"), Type = "Phytoplankton", Variable = "abundance", Subset = "raw"),
    regexp = "must be a single character string"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = 123, Type = "Phytoplankton", Variable = "abundance", Subset = "raw"),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_data validates Type parameter", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "InvalidType", Variable = "abundance", Subset = "raw"),
    regexp = "Type.*is not available"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = c("Phytoplankton", "Zooplankton"), Variable = "abundance", Subset = "raw"),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_data validates Survey + Type combinations", {
  # CPR only has Phytoplankton and Zooplankton
  testthat::expect_error(
    pr_get_data(Survey = "CPR", Type = "Chemistry"),
    regexp = "Type.*is not available for Survey.*CPR"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "CPR", Type = "CTD"),
    regexp = "Type.*is not available for Survey.*CPR"
  )
  
  # Coastal only has Micro
  testthat::expect_error(
    pr_get_data(Survey = "Coastal", Type = "Phytoplankton", Variable = "abundance", Subset = "raw"),
    regexp = "Type.*is not available for Survey.*Coastal"
  )
  
  # GO-SHIP only has Micro
  testthat::expect_error(
    pr_get_data(Survey = "GO-SHIP", Type = "Chemistry"),
    regexp = "Type.*is not available for Survey.*GO-SHIP"
  )
})

testthat::test_that("pr_get_data requires Variable for plankton data", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Phytoplankton", Subset = "raw"),
    regexp = "Variable.*is required"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Zooplankton", Subset = "raw"),
    regexp = "Variable.*is required"
  )
})

testthat::test_that("pr_get_data requires Subset for plankton data", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "abundance"),
    regexp = "Subset.*is required"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Zooplankton", Variable = "abundance"),
    regexp = "Subset.*is required"
  )
})

testthat::test_that("pr_get_data validates Variable parameter", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "invalid", Subset = "raw"),
    regexp = "Variable.*must be one of"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = c("abundance", "biovolume"), Subset = "raw"),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_data validates Subset parameter", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "abundance", Subset = "invalid"),
    regexp = "Subset.*must be one of"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "abundance", Subset = c("raw", "htg")),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_data validates biovolume only for Phytoplankton", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Zooplankton", Variable = "biovolume", Subset = "raw"),
    regexp = "biovolume.*only available for Phytoplankton"
  )
})

testthat::test_that("pr_get_data validates Format parameter for Pigments", {
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Pigments", Format = "invalid"),
    regexp = "Format.*must be one of"
  )
  
  testthat::expect_error(
    pr_get_data(Survey = "NRS", Type = "Pigments", Format = c("all", "binned")),
    regexp = "must be a single character string"
  )
})

testthat::test_that("pr_get_data warns about unused parameters", {
  # Variable is ignored for Chemistry
  testthat::expect_warning(
    suppressMessages(pr_get_data(Survey = "NRS", Type = "Chemistry", Variable = "abundance")),
    regexp = "Variable.*is ignored"
  )
  
  # Subset is ignored for Chemistry
  testthat::expect_warning(
    suppressMessages(pr_get_data(Survey = "NRS", Type = "Chemistry", Subset = "raw")),
    regexp = "Subset.*is ignored"
  )
  
  # Format is ignored for non-Pigments
  testthat::expect_warning(
    suppressMessages(pr_get_data(Survey = "NRS", Type = "Chemistry", Format = "binned")),
    regexp = "Format.*is ignored"
  )
})

# ==============================================================================
# Integration tests (network-dependent) - skip offline
# ==============================================================================

# --- NRS Phytoplankton ---

testthat::test_that("pr_get_data returns NRS Phytoplankton abundance data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Check for expected metadata columns
  expected_cols <- c("StationCode", "StationName", "TripCode", "SampleTime_Local")
  testthat::expect_true(all(expected_cols %in% names(result)))
})

testthat::test_that("pr_get_data returns NRS Phytoplankton biovolume data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "biovolume", Subset = "genus")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_data returns NRS Phytoplankton htg data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Phytoplankton", Variable = "abundance", Subset = "htg")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

# --- NRS Zooplankton ---

testthat::test_that("pr_get_data returns NRS Zooplankton abundance data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Zooplankton", Variable = "abundance", Subset = "raw")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_data returns NRS Zooplankton species data (copepods + non-copepods)", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Zooplankton", Variable = "abundance", Subset = "species")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_data returns NRS Zooplankton copepods data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Zooplankton", Variable = "abundance", Subset = "copepods")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

# --- CPR Phytoplankton ---

testthat::test_that("pr_get_data returns CPR Phytoplankton abundance data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "CPR", Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Survey"), "CPR")
})

testthat::test_that("pr_get_data returns CPR Phytoplankton biovolume data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "CPR", Type = "Phytoplankton", Variable = "biovolume", Subset = "htg")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

# --- CPR Zooplankton ---

testthat::test_that("pr_get_data returns CPR Zooplankton abundance data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "CPR", Type = "Zooplankton", Variable = "abundance", Subset = "raw")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

testthat::test_that("pr_get_data returns CPR Zooplankton species data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "CPR", Type = "Zooplankton", Variable = "abundance", Subset = "species")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
})

# --- NRS Chemistry ---

testthat::test_that("pr_get_data returns NRS Chemistry data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Chemistry")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check expected columns (long format)
  expected_cols <- c("Parameters", "Values", "StationCode", "StationName")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check Parameters column contains expected values
  expected_params <- c("Silicate_umolL", "Phosphate_umolL", "Nitrate_umolL", "NOx_umolL", "DIN_umolL")
  testthat::expect_true(any(unique(result$Parameters) %in% expected_params))
})

# --- NRS Pigments ---

testthat::test_that("pr_get_data returns NRS Pigments data (all format)", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Pigments", Format = "all")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Should contain individual pigment names
  params <- unique(result$Parameters)
  individual_pigments <- c("CphlA_mgm3", "Fuco_mgm3", "Perid_mgm3")
  testthat::expect_true(any(individual_pigments %in% params))
})

testthat::test_that("pr_get_data returns NRS Pigments data (binned format)", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Pigments", Format = "binned")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(nrow(result) > 0)
  
  # Should contain binned pigment classes
  params <- unique(result$Parameters)
  binned_params <- c("TotalChla", "TotalChl", "PPC", "PSC")
  testthat::expect_true(any(binned_params %in% params))
  
  # Should NOT contain individual pigment names
  testthat::expect_false(any(c("CphlA_mgm3", "Fuco_mgm3") %in% params))
})

# --- NRS Pico ---

testthat::test_that("pr_get_data returns NRS Pico data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Pico")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check for expected picoplankton groups
  params <- unique(result$Parameters)
  expected_groups <- c("Prochlorococcus_cellsmL", "Synechococcus_cellsmL", "Picoeukaryotes_cellsmL")
  testthat::expect_true(any(expected_groups %in% params))
})

# --- Micro data (NRS, Coastal, GO-SHIP) ---

testthat::test_that("pr_get_data returns NRS Micro data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "Micro")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Microbes")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
})

testthat::test_that("pr_get_data returns Coastal Micro data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "Coastal", Type = "Micro")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Microbes")
  testthat::expect_equal(attr(result, "Survey"), "Coastal")
})

testthat::test_that("pr_get_data returns GO-SHIP Micro data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "GO-SHIP", Type = "Micro")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Microbes")
  testthat::expect_equal(attr(result, "Survey"), "GO-SHIP")
})

# --- NRS TSS ---

testthat::test_that("pr_get_data returns NRS TSS data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "TSS")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
})

# --- NRS CTD ---

testthat::test_that("pr_get_data returns NRS CTD data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "CTD")
  
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "planktonr_dat")
  testthat::expect_true(nrow(result) > 0)
  
  # Check planktonr_dat attributes
  testthat::expect_equal(attr(result, "Type"), "Water")
  testthat::expect_equal(attr(result, "Survey"), "NRS")
  
  # Check for expected CTD columns (wide format)
  expected_cols <- c("StationCode", "StationName", "SampleDepth_m", "Temperature_degC", "Salinity_psu")
  testthat::expect_true(any(expected_cols %in% names(result)))
})

testthat::test_that("pr_get_data CTD filters problematic file_ids", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- pr_get_data(Survey = "NRS", Type = "CTD")
  
  # Should not contain file_ids 2117, 2184, 2186, 2187
  if ("file_id" %in% names(result)) {
    testthat::expect_false(any(result$file_id %in% c(2117, 2184, 2186, 2187)))
  }
})
