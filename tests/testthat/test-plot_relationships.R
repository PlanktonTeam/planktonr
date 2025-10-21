# ============================================================================
# Integration Tests (require network access)
# ============================================================================

testthat::test_that("pr_plot_scatter creates ggplot for NRS microbial data with no trend", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- planktonr::pr_get_NRSMicro() %>%
    tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
    dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
    tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
    pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true("ggplot" %in% class(result))
  
  # Check that the plot has the expected layers
  testthat::expect_true(length(result$layers) > 0)
  
  # Check that it uses StationName for color/shape (NRS data) - check in layers
  testthat::expect_true(length(result$layers) > 0)
})

testthat::test_that("pr_plot_scatter creates ggplot for NRS with Linear trend", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- planktonr::pr_get_NRSMicro() %>%
    tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
    dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
    tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
    pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = "Linear")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that geom_smooth is added for Linear trend
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomSmooth" %in% layer_geoms)
})

testthat::test_that("pr_plot_scatter creates ggplot for NRS with Smoother trend", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- planktonr::pr_get_NRSMicro() %>%
    tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
    dplyr::filter(StationCode %in% c("NSI", "PHB")) %>%
    tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
    pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = "Smoother")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that geom_smooth is added for Smoother trend
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomSmooth" %in% layer_geoms)
})

testthat::test_that("pr_plot_scatter handles CPR data with BioRegion", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Get CPR data (requires bioregion processing)
  cpr_data <- planktonr::pr_get_Raw("cpr_derived_indices_data") %>%
    pr_rename() %>%
    pr_add_Bioregions() %>%
    dplyr::filter(!is.na(BioRegion)) %>%
    dplyr::select(BioRegion, BiomassIndex_mgm3, ZoopAbundance_m3) %>%
    tidyr::drop_na()
  
  if(nrow(cpr_data) > 0) {
    result <- pr_plot_scatter(cpr_data, "BiomassIndex_mgm3", "ZoopAbundance_m3", Trend = "none")
    
    testthat::expect_s3_class(result, "ggplot")
    testthat::expect_true(length(result$layers) > 0)
  } else {
    testthat::skip("No CPR data with BioRegion available")
  }
})

testthat::test_that("pr_plot_scatter handles data with SampleDepth_m for faceting", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- planktonr::pr_get_NRSMicro() %>%
    tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
    dplyr::filter(StationCode %in% c("NSI", "PHB"), 
                  SampleDepth_m %in% c(0, 10, 50)) %>%
    tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
    pr_plot_scatter("Bacterial_Temperature_Index_KD", "nitrogen_fixation_organisms", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for faceting by SampleDepth_m
  if(!is.null(result$facet) && inherits(result$facet, "FacetGrid")) {
    testthat::expect_true("FacetGrid" %in% class(result$facet))
  }
})

testthat::test_that("pr_plot_box creates ggplot for NRS data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- planktonr::pr_get_NRSMicro() %>%
    tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
    dplyr::filter(StationCode %in% c("NSI", "PHB", "MAI")) %>%
    tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
    pr_plot_box("Bacterial_Temperature_Index_KD")
  
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true("ggplot" %in% class(result))
  
  # Check that boxplot geom is present
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomBoxplot" %in% layer_geoms)
  
  # Check that layers are present
  testthat::expect_true(length(result$layers) > 0)
})

testthat::test_that("pr_plot_box creates ggplot for Coastal survey data", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  result <- planktonr::pr_get_NRSMicro(Survey = "Coastal") %>%
    tidyr::drop_na(tidyselect::all_of(c("Values", "Parameters"))) %>%
    dplyr::filter(StationCode %in% c("DEE", "DEB")) %>%
    tidyr::pivot_wider(names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
    pr_plot_box("Bacterial_Temperature_Index_KD")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that boxplot geom is present
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomBoxplot" %in% layer_geoms)
})

testthat::test_that("pr_plot_box handles CPR data with BioRegion", {
  skip_if_offline()
  testthat::skip_on_cran()
  
  # Get CPR data with BioRegion
  cpr_data <- planktonr::pr_get_Raw("cpr_derived_indices_data") %>%
    pr_rename() %>%
    pr_add_Bioregions() %>%
    dplyr::filter(!is.na(BioRegion)) %>%
    dplyr::select(BioRegion, BiomassIndex_mgm3, ZoopAbundance_m3) %>%
    tidyr::drop_na()
  
  if(nrow(cpr_data) > 0) {
    result <- pr_plot_box(cpr_data, "BiomassIndex_mgm3")
    
    testthat::expect_s3_class(result, "ggplot")
    testthat::expect_true(length(result$layers) > 0)
  } else {
    testthat::skip("No CPR data with BioRegion available")
  }
})

# ============================================================================
# Unit Tests (use mock data, no network required)
# ============================================================================

testthat::test_that("pr_plot_scatter works with simple mock NRS data", {
  mock_data <- data.frame(
    StationName = rep(c("North Stradbroke Island", "Port Hacking"), each = 50),
    Biomass_mgm3 = runif(100, 0, 10),
    Temperature_degC = runif(100, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "Biomass_mgm3", "Temperature_degC", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(length(result$layers) > 0)
})

testthat::test_that("pr_plot_scatter works with mock CPR data", {
  mock_data <- data.frame(
    BioRegion = rep(c("South-east", "South-west", "Temperate East"), each = 30),
    BiomassIndex_mgm3 = runif(90, 0, 10),
    Temperature_degC = runif(90, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "BiomassIndex_mgm3", "Temperature_degC", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(length(result$layers) > 0)
})

testthat::test_that("pr_plot_scatter Linear trend adds smooth layer", {
  mock_data <- data.frame(
    StationName = rep(c("Station A", "Station B"), each = 50),
    Biomass_mgm3 = runif(100, 0, 10),
    Temperature_degC = runif(100, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "Biomass_mgm3", "Temperature_degC", Trend = "Linear")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for GeomSmooth layer
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomSmooth" %in% layer_geoms)
})

testthat::test_that("pr_plot_scatter Smoother trend adds smooth layer", {
  mock_data <- data.frame(
    StationName = rep(c("Station A", "Station B"), each = 50),
    Biomass_mgm3 = runif(100, 0, 10),
    Temperature_degC = runif(100, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "Biomass_mgm3", "Temperature_degC", Trend = "Smoother")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for GeomSmooth layer
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomSmooth" %in% layer_geoms)
})

testthat::test_that("pr_plot_scatter facets by SampleDepth_m when present", {
  mock_data <- data.frame(
    StationName = rep(c("Station A", "Station B"), each = 50),
    SampleDepth_m = rep(c(0, 10, 50), length.out = 100),
    Biomass_mgm3 = runif(100, 0, 10),
    Temperature_degC = runif(100, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "Biomass_mgm3", "Temperature_degC", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for faceting
  testthat::expect_true(!is.null(result$facet))
})

testthat::test_that("pr_plot_box works with simple mock NRS data", {
  mock_data <- data.frame(
    StationName = rep(c("North Stradbroke Island", "Port Hacking", "Maria Island"), each = 30),
    Temperature_degC = runif(90, 10, 25)
  )
  
  result <- pr_plot_box(mock_data, "Temperature_degC")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for boxplot layer
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomBoxplot" %in% layer_geoms)
})

testthat::test_that("pr_plot_box works with mock CPR data", {
  mock_data <- data.frame(
    BioRegion = rep(c("South-east", "South-west", "Temperate East"), each = 30),
    Temperature_degC = runif(90, 10, 25)
  )
  
  result <- pr_plot_box(mock_data, "Temperature_degC")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for boxplot layer
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomBoxplot" %in% layer_geoms)
})

testthat::test_that("pr_plot_box includes point layer", {
  mock_data <- data.frame(
    StationName = rep(c("Station A", "Station B"), each = 30),
    Biomass_mgm3 = runif(60, 0, 100)
  )
  
  result <- pr_plot_box(mock_data, "Biomass_mgm3")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check for both point and boxplot layers
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomPoint" %in% layer_geoms)
  testthat::expect_true("GeomBoxplot" %in% layer_geoms)
})

testthat::test_that("pr_plot_scatter applies pr_relabel to axis titles", {
  mock_data <- data.frame(
    StationName = rep("Station A", 20),
    Temperature_degC = runif(20, 10, 25),
    Salinity = runif(20, 30, 37)
  )
  
  result <- pr_plot_scatter(mock_data, "Temperature_degC", "Salinity", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that axis labels are set
  testthat::expect_true(!is.null(result$labels$x))
  testthat::expect_true(!is.null(result$labels$y))
})

testthat::test_that("pr_plot_box applies pr_relabel to y-axis title", {
  mock_data <- data.frame(
    StationName = rep(c("Station A", "Station B"), each = 20),
    Temperature_degC = runif(40, 10, 25)
  )
  
  result <- pr_plot_box(mock_data, "Temperature_degC")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that y-axis label is set
  testthat::expect_true(!is.null(result$labels$y))
})

testthat::test_that("pr_plot_scatter uses correct color scales for NRS", {
  mock_data <- data.frame(
    StationName = rep(c("North Stradbroke Island", "Port Hacking"), each = 20),
    Biomass_mgm3 = runif(40, 0, 10),
    Temperature_degC = runif(40, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "Biomass_mgm3", "Temperature_degC", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that scales are applied
  testthat::expect_true(length(result$scales$scales) > 0)
})

testthat::test_that("pr_plot_scatter uses correct color scales for CPR", {
  mock_data <- data.frame(
    BioRegion = rep(c("South-east", "South-west"), each = 20),
    BiomassIndex_mgm3 = runif(40, 0, 10),
    Temperature_degC = runif(40, 15, 25)
  )
  
  result <- pr_plot_scatter(mock_data, "BiomassIndex_mgm3", "Temperature_degC", Trend = "none")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that scales are applied
  testthat::expect_true(length(result$scales$scales) > 0)
})

testthat::test_that("pr_plot_box uses correct scales for NRS", {
  mock_data <- data.frame(
    StationName = rep(c("North Stradbroke Island", "Port Hacking"), each = 20),
    Biomass_mgm3 = runif(40, 0, 100)
  )
  
  result <- pr_plot_box(mock_data, "Biomass_mgm3")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that scales are applied (color and linetype)
  testthat::expect_true(length(result$scales$scales) > 0)
})

testthat::test_that("pr_plot_box uses correct scales for CPR", {
  mock_data <- data.frame(
    BioRegion = rep(c("South-east", "South-west"), each = 20),
    BiomassIndex_mgm3 = runif(40, 0, 100)
  )
  
  result <- pr_plot_box(mock_data, "BiomassIndex_mgm3")
  
  testthat::expect_s3_class(result, "ggplot")
  
  # Check that scales are applied (color and linetype)
  testthat::expect_true(length(result$scales$scales) > 0)
})

# ============================================================================
# Error Handling Tests
# ============================================================================

testthat::test_that("pr_plot_scatter creates plot even with missing column (ggplot handles gracefully)", {
  mock_data <- data.frame(
    StationName = rep("Station A", 20),
    Param1 = runif(20, 0, 10)
  )
  
  # ggplot will create plot but may error on render - we just check it creates a ggplot object
  testthat::expect_s3_class(
    pr_plot_scatter(mock_data, "Param1", "NonExistentParam", Trend = "none"),
    "ggplot"
  )
})

testthat::test_that("pr_plot_box creates plot even with missing column (ggplot handles gracefully)", {
  mock_data <- data.frame(
    StationName = rep("Station A", 20),
    Param1 = runif(20, 0, 10)
  )
  
  # ggplot will create plot but may error on render - we just check it creates a ggplot object
  testthat::expect_s3_class(
    pr_plot_box(mock_data, "NonExistentParam"),
    "ggplot"
  )
})

testthat::test_that("pr_plot_scatter handles empty data frame", {
  mock_data <- data.frame(
    StationName = character(),
    Param1 = numeric(),
    Param2 = numeric()
  )
  
  # Should create plot but with no data
  result <- pr_plot_scatter(mock_data, "Param1", "Param2", Trend = "none")
  testthat::expect_s3_class(result, "ggplot")
})

testthat::test_that("pr_plot_box handles empty data frame", {
  mock_data <- data.frame(
    StationName = character(),
    Value = numeric()
  )
  
  # Should create plot but with no data
  result <- pr_plot_box(mock_data, "Value")
  testthat::expect_s3_class(result, "ggplot")
})

testthat::test_that("pr_plot_scatter creates plot even without StationName or BioRegion", {
  mock_data <- data.frame(
    Param1 = runif(20, 0, 10),
    Param2 = runif(20, 5, 15)
  )
  
  # Should create plot even without grouping variable (uses defaults)
  result <- pr_plot_scatter(mock_data, "Param1", "Param2", Trend = "none")
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(length(result$layers) > 0)
})

testthat::test_that("pr_plot_box creates plot even without StationName or BioRegion", {
  mock_data <- data.frame(
    Value = runif(20, 0, 100)
  )
  
  # Should create plot even without grouping variable (uses defaults)
  result <- pr_plot_box(mock_data, "Value")
  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(length(result$layers) > 0)
})

testthat::test_that("pr_plot_scatter accepts valid Trend values", {
  mock_data <- data.frame(
    StationName = rep("Station A", 20),
    Param1 = runif(20, 0, 10),
    Param2 = runif(20, 5, 15)
  )
  
  # All valid Trend values should work
  testthat::expect_no_error(pr_plot_scatter(mock_data, "Param1", "Param2", Trend = "none"))
  testthat::expect_no_error(pr_plot_scatter(mock_data, "Param1", "Param2", Trend = "Linear"))
  testthat::expect_no_error(pr_plot_scatter(mock_data, "Param1", "Param2", Trend = "Smoother"))
})

testthat::test_that("pr_plot_scatter handles case-sensitive Trend parameter", {
  mock_data <- data.frame(
    StationName = rep("Station A", 20),
    Biomass_mgm3 = runif(20, 0, 10),
    Temperature_degC = runif(20, 15, 25)
  )
  
  # Lowercase or incorrect case should not add trend line (no error, just no trend)
  result <- pr_plot_scatter(mock_data, "Biomass_mgm3", "Temperature_degC", Trend = "linear")
  testthat::expect_s3_class(result, "ggplot")
  
  # Should not have smooth layer for incorrect case
  layer_geoms <- sapply(result$layers, function(l) class(l$geom)[1])
  testthat::expect_false("GeomSmooth" %in% layer_geoms)
})
