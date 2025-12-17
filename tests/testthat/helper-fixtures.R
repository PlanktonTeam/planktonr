# Test Fixtures for planktonr
# This file creates mock/sample data to avoid network dependencies

# Create sample station data
create_test_stations <- function() {
  data.frame(
    StationCode = c("NSI", "PHB", "MAI", "YON", "ROT"),
    StationName = c("North Stradbroke Island", "Port Hacking", "Maria Island", "Yongala", "Rottnest Island"),
    Latitude = c(-27.33, -34.09, -42.60, -19.30, -32.00),
    Longitude = c(153.55, 151.22, 148.23, 147.62, 115.50),
    stringsAsFactors = FALSE
  )
}

# Create sample trip data
create_test_trips <- function() {
  data.frame(
    TripCode = c("NSI20220101", "PHB20220115", "MAI20220201"),
    StationCode = c("NSI", "PHB", "MAI"),
    SampleTime_UTC = as.POSIXct(c("2022-01-01 00:00:00", "2022-01-15 00:00:00", "2022-02-01 00:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
}

# Create sample phytoplankton data
create_test_phyto_data <- function() {
  data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI"), each = 3),
    TaxonGroup = rep(c("Dinoflagellate", "Cyanobacteria", "Diatom"), 3),
    Species = rep(c("Dinophysis acuta", "Synechococcus sp.", "Pseudo-nitzschia spp."), 3),
    Abundance = runif(9, 10, 1000),
    BioVolume_um3m3 = runif(9, 100, 10000),
    PhytoAbund_m3 = runif(9, 5, 500),
    Biovolume_um3L = runif(9, 50, 5000),
    Cells_L = runif(9, 100, 10000),
    stringsAsFactors = FALSE
  )
}

# Create sample zooplankton data
create_test_zoo_data <- function() {
  data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI"), each = 3),
    TaxonGroup = rep(c("Copepod", "Chaetognath", "Appendicularian"), 3),
    Species = rep(c("Acartia danae", "Sagitta enflata", "Oikopleura dioica"), 3),
    CopeAbundance_m3 = runif(9, 10, 1000),
    Biomass_mgm3 = runif(9, 0.1, 10),
    AshFreeBiomass_mgm3 = runif(9, 0.05, 8),
    stringsAsFactors = FALSE
  )
}

# Create sample CPR data
create_test_cpr_data <- function() {
  data.frame(
    BioRegion = rep(c("North", "South-east", "South-west", "Temperate East"), each = 2),
    TaxonGroup = rep(c("Dinoflagellate", "Diatom"), 4),
    Species = rep(c("Ceratium spp.", "Rhizosolenia spp."), 4),
    PhytoAbund_m3 = runif(8, 5, 500),
    BioVolume_um3m3 = runif(8, 100, 10000),
    BiomassIndex_mgm3 = runif(8, 0.5, 20),
    Latitude = runif(8, -45, -10),
    Longitude = runif(8, 110, 160),
    stringsAsFactors = FALSE
  )
}

# Create sample indices data for NRS
create_test_nrs_indices <- function() {
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
  n_samples <- length(dates)

  df <- data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI"), each = n_samples),
    SampleTime_Local = rep(dates, 3),
    Year_Local = rep(as.integer(format(dates, "%Y")), 3),
    Month_Local = rep(as.integer(format(dates, "%m")), 3),
    Parameters = "PhytoAbundance_CellsL",
    Values = rlnorm(n_samples * 3, meanlog = 10, sdlog = 1),
    Latitude = rep(c(-27.33, -34.09, -42.60), each = n_samples),
    Longitude = rep(c(153.55, 151.22, 148.23), each = n_samples),
    BioRegion = rep(c("NSI", "PHB", "MAI"), each = n_samples),
    stringsAsFactors = FALSE
  )

  planktonr_dat(df, Type = "Phytoplankton", Survey = "NRS", Variable = "abundance")
}

# Create sample indices data for CPR
create_test_cpr_indices <- function() {
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
  n_samples <- length(dates)

  df <- data.frame(
    BioRegion = rep(c("North", "South-east", "South-west", "Temperate East"), each = n_samples),
    SampleTime_Local = rep(dates, 4),
    Year_Local = rep(as.integer(format(dates, "%Y")), 4),
    Month_Local = rep(as.integer(format(dates, "%m")), 4),
    Parameters = "BiomassIndex_mgm3",
    Values = rlnorm(n_samples * 4, meanlog = 2, sdlog = 0.5),
    Latitude = runif(n_samples * 4, -45, -10),
    Longitude = runif(n_samples * 4, 110, 160),
    stringsAsFactors = FALSE
  )

  planktonr_dat(df, Type = "Zooplankton", Survey = "CPR", Variable = "biomass")
}

# Create sample chemistry data
create_test_chemistry <- function() {
  data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI"), each = 10),
    Parameters = rep(c("SecchiDepth_m", "Nitrate_umolL", "Phosphate_umolL",
                       "Silicate_umolL", "Ammonium_umolL"), 6),
    Values = c(runif(6, 10, 30), runif(6, 0.1, 5), runif(6, 0.01, 1),
               runif(6, 0.5, 10), runif(6, 0.05, 2)),
    SampleDepth_m = rep(c(0, 10, 20), 10),
    SampleTime_Local = rep(seq.Date(as.Date("2022-01-01"), as.Date("2022-10-01"), by = "month"), each = 3),
    stringsAsFactors = FALSE
  )
}

# Create sample EOV data
create_test_eov_data <- function() {
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
  n_samples <- length(dates)

  data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI"), each = n_samples * 3),
    Parameters = rep(rep(c("Biomass_mgm3", "Temperature_degC", "Salinity_psu"), each = n_samples), 3),
    Values = c(
      rlnorm(n_samples, meanlog = 0, sdlog = 0.8),  # Biomass
      rnorm(n_samples, mean = 20, sd = 3),           # Temperature
      rnorm(n_samples, mean = 35, sd = 1),           # Salinity
      rlnorm(n_samples, meanlog = 0, sdlog = 0.8),
      rnorm(n_samples, mean = 18, sd = 3),
      rnorm(n_samples, mean = 35.5, sd = 1),
      rlnorm(n_samples, meanlog = 0, sdlog = 0.8),
      rnorm(n_samples, mean = 16, sd = 3),
      rnorm(n_samples, mean = 34.5, sd = 1)
    ),
    SampleTime_Local = rep(dates, 9),
    Year_Local = rep(as.integer(format(dates, "%Y")), 9),
    Month_Local = rep(as.integer(format(dates, "%m")), 9),
    stringsAsFactors = FALSE
  )
}

# Create sample functional groups data
create_test_funcgroups <- function() {
  dates <- seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month")
  n_samples <- length(dates)

  groups <- c("Diatoms", "Dinoflagellates", "Cryptophytes", "Cyanobacteria", "Other")

  data.frame(
    StationCode = rep(c("NSI", "PHB"), each = n_samples * length(groups)),
    FunctionalGroup = rep(rep(groups, each = n_samples), 2),
    Abundance = rlnorm(n_samples * length(groups) * 2, meanlog = 8, sdlog = 1.5),
    SampleTime_Local = rep(dates, length(groups) * 2),
    Year_Local = rep(as.integer(format(dates, "%Y")), length(groups) * 2),
    Month_Local = rep(as.integer(format(dates, "%m")), length(groups) * 2),
    stringsAsFactors = FALSE
  )
}

# Create sample data with quality flags
create_test_flagged_data <- function() {
  data.frame(
    SST = c(15.5, 20.3, 999, 18.7, -99, 22.1, 25.4, 19.8),
    SST_Flag = c(1, 1, 4, 1, 4, 1, 1, 2),
    Salinity = c(35.2, 35.5, 35.1, 0, 35.8, 35.3, 35.6, 35.4),
    Salinity_Flag = c(1, 1, 1, 4, 1, 1, 1, 1),
    StationCode = rep(c("NSI", "PHB"), each = 4),
    stringsAsFactors = FALSE
  )
}

# Create sample coordinate data for renaming
create_test_coordinate_data <- function() {
  data.frame(
    LATITUDE = c(-27.33, -34.09, -42.60),
    LONGITUDE = c(153.55, 151.22, 148.23),
    Station = c("NSI", "PHB", "MAI"),
    stringsAsFactors = FALSE
  )
}

# Create sample species data for filtering
create_test_species_data <- function() {
  data.frame(
    Species = c(
      "Valid species",
      "Another valid species",
      "Invalid cf.",
      "Bad spp.",
      NA,
      "Good Species",
      "Wrong/badname",
      "Acceptable sp",
      "Problem cf",
      "Another bad spp"
    ),
    Abundance = runif(10, 1, 100),
    stringsAsFactors = FALSE
  )
}

# Create sample larval fish data
create_test_larvalfish <- function() {
  structure(
    data.frame(
      SpeciesName = rep(c("Acanthuridae_37437900", "Carangidae_37437901"), each = 5),
      Latitude = runif(10, -45, -10),
      Longitude = runif(10, 110, 160),
      Abundance = runif(10, 1, 50),
      SampleTime_UTC = seq.POSIXt(
        as.POSIXct("2020-01-01", tz = "UTC"),
        as.POSIXct("2020-05-01", tz = "UTC"),
        length.out = 10
      ),
      stringsAsFactors = FALSE
    ),
    class = c("planktonr_dat", "data.frame")
  )
}

# Create sample progress map data
create_test_progress_data <- function() {
  data.frame(
    Survey = rep(c("NRS", "CPR"), each = 50),
    Latitude = c(runif(50, -45, -10), runif(50, -45, -10)),
    Longitude = c(runif(50, 110, 160), runif(50, 110, 160)),
    Year = rep(2015:2024, 10),
    TripCode = paste0(rep(c("NSI", "CPR"), each = 50), 20150101 + 1:50),
    stringsAsFactors = FALSE
  )
}

# Create sample data for satellite matching
create_test_satellite_locs <- function() {
  data.frame(
    Latitude = c(-27.33, -34.09, -42.60, -19.30, -32.00),
    Longitude = c(153.55, 151.22, 148.23, 147.62, 115.50),
    SampleTime_UTC = seq.POSIXt(
      as.POSIXct("2022-01-01", tz = "UTC"),
      as.POSIXct("2022-01-05", tz = "UTC"),
      length.out = 5
    ),
    StationCode = c("NSI", "PHB", "MAI", "YON", "ROT"),
    stringsAsFactors = FALSE
  )
}

# Create planktonr_dat object
create_test_planktonr_dat <- function(Type = "phytoplankton") {
  if (Type == "phytoplankton") {
    data <- create_test_phyto_data()
  } else if (Type == "zooplankton") {
    data <- create_test_zoo_data()
  } else {
    data <- create_test_stations()
  }

  structure(
    data,
    class = c("planktonr_dat", "data.frame"),
    Survey = "NRS",
    Type = Type,
    variable = "abundance"
  )
}

# Helper to create empty data frame with expected structure
create_empty_test_data <- function(columns) {
  df <- as.data.frame(matrix(nrow = 0, ncol = length(columns)))
  names(df) <- columns
  df
}
