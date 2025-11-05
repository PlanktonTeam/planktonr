#' Load National Reference Station chemistry data
#'
#' Load water chemistry measurements from NRS stations, including nutrients 
#' (nitrogen, phosphorus, silicate), dissolved oxygen, carbon chemistry, and 
#' physical properties. Data are quality controlled using IMOS flags.
#' 
#' @details
#' The function retrieves and processes the following parameters:
#' 
#' **Nutrients:**
#' * `Nitrate_umolL` - Nitrate concentration (µmol/L)
#' * `Nitrite_umolL` - Nitrite concentration (µmol/L)
#' * `Ammonium_umolL` - Ammonium concentration (µmol/L)
#' * `Phosphate_umolL` - Phosphate concentration (µmol/L)
#' * `Silicate_umolL` - Silicate concentration (µmol/L)
#' * `NOx_umolL` - Nitrogen oxides (nitrate + nitrite, µmol/L)
#' * `DIN_umolL` - Dissolved inorganic nitrogen (NOx + ammonium, µmol/L)
#' 
#' **Carbon Chemistry:**
#' * `DIC_umolkg` - Dissolved inorganic carbon (µmol/kg)
#' * `Alkalinity_umolkg` - Total alkalinity (µmol/kg)
#' 
#' **Other:**
#' * `Oxygen_umolL` - Dissolved oxygen (µmol/L)
#' * `Salinity` - Salinity (unitless, PSU)
#' * `SecchiDepth_m` - Secchi depth, measure of water clarity (m)
#' * `Redfield` - N:P ratio (calculated as NOx/Phosphate)
#' 
#' Quality control flags are applied using [pr_apply_Flags()]. Samples are 
#' filtered to include only NRS stations.
#' 
#' NOx, DIN, and Redfield ratio are calculated from the measured nutrient values.
#'
#' @return A dataframe in long format with chemistry data including:
#'   * `StationCode`, `StationName` - Station identifiers
#'   * `SampleTime_Local` - Local sampling date and time
#'   * `SampleDepth_m` - Sampling depth
#'   * `Parameters` - Chemistry parameter name
#'   * `Values` - Measured or calculated value
#' 
#' @seealso [pr_get_NRSPigments()] for photosynthetic pigment data,
#'   [pr_get_NRSCTD()] for CTD profile data,
#'   [pr_get_NRSEnvContour()] to format data for contour plots
#' 
#' @export
#'
#' @examples
#' df <- pr_get_NRSChemistry()
#' 
#' # Examine available parameters
#' unique(df$Parameters)
#' 
#' # Filter for surface nitrate at specific stations
#' df_nitrate <- df %>%
#'   dplyr::filter(Parameters == "Nitrate_umolL", 
#'                 SampleDepth_m == 0,
#'                 StationCode %in% c("MAI", "PHB"))
#' 
#' @importFrom rlang .data
pr_get_NRSChemistry <- function(){

  var_names <- c("SecchiDepth_m", "Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "DIC_umolkg", "Alkalinity_umolkg", "Salinity",
                 "NOx_umolL", "DIN_umolL", "Redfield") # TODO Check if it is Alk or Total Alk. Our code used to say Total Alk.

  file <- "bgc_chemistry_data"

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags() %>%
    pr_add_StationCode() %>%
    pr_filter_NRSStations() %>%
    dplyr::mutate_all(~ replace(., is.na(.), NA)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Month_Local = lubridate::month(.data$SampleTime_Local),
                  NOx_umolL = sum(.data$Nitrate_umolL, .data$Nitrite_umolL, na.rm = TRUE),
                  DIN_umolL = sum(.data$NOx_umolL, .data$Ammonium_umolL, na.rm = TRUE),
                  Redfield = mean(.data$NOx_umolL, na.rm = TRUE)/mean(.data$Phosphate_umolL, na.rm = TRUE),
                  Redfield = ifelse(is.infinite(.data$Redfield), NA, .data$Redfield)) %>%
    dplyr::select("Project", "SampleTime_Local", "Month_Local", "SampleDepth_m", "TripCode",
                  "StationName", "StationCode", tidyselect::all_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    pr_reorder()

  return(dat)

}

#' Get photosynthetic pigments data from NRS stations
#' 
#' Load High-Performance Liquid Chromatography (HPLC) pigment data from NRS stations. 
#' Pigments can be retrieved as individual compounds or grouped into functional classes.
#' 
#' @param Format Output format:
#'   * `"all"` - All individual pigment concentrations (default)
#'   * `"binned"` - Pigments grouped into functional classes (e.g., total chlorophyll, 
#'     photoprotective carotenoids)
#'
#' @details
#' ## Individual Pigments (Format = "all")
#' Returns concentrations (mg/m³) of individual pigments including:
#' * Chlorophylls: Cphl A, B, C1, C2, C3, chlorophyllide A
#' * Carotenoids: fucoxanthin, peridinin, alloxanthin, zeaxanthin, etc.
#' * Degradation products: phaeophytin A & B, phaeophorbide A, pyrophaeophorbide A
#' 
#' ## Binned Pigments (Format = "binned")
#' Returns pigments grouped into functional classes:
#' * `TotalChla` - Total chlorophyll a (including degradation products)
#' * `TotalChl` - All chlorophyll pigments combined
#' * `PPC` - Photoprotective carotenoids (protect from high light)
#' * `PSC` - Photosynthetic carotenoids (harvest light energy)
#' * `PSP` - Total photosynthetic pigments (PSC + Total Chl)
#' * `TCaro` - Total carotenoids
#' * `TAcc` - Total accessory pigments (everything except Chl a)
#' * `TPig` - Total pigments
#' * `TDP` - Total diagnostic pigments (used for phytoplankton community analysis)
#' 
#' Quality control flags are applied and samples with zero total chlorophyll a 
#' are removed. Water column (WC) samples are excluded as they represent 
#' integrated samples rather than discrete depths.
#'
#' @return A dataframe in long format with pigment data including:
#'   * `StationCode`, `StationName` - Station identifiers
#'   * `SampleTime_Local` - Local sampling date and time
#'   * `SampleDepth_m` - Sampling depth (numeric)
#'   * `Parameters` - Pigment name or pigment class
#'   * `Values` - Concentration (mg/m³)
#' 
#' @seealso [pr_get_NRSChemistry()] for nutrient data,
#'   [pr_plot_Enviro()] for visualising pigment depth profiles,
#'   [pr_remove_outliers()] for outlier removal
#' 
#' @export
#'
#' @examples
#' # Get all individual pigments
#' df <- pr_get_NRSPigments()
#' 
#' # Get binned pigment classes
#' df <- pr_get_NRSPigments(Format = "binned")
#' 
#' # Filter for total chlorophyll a at Maria Island
#' df_chla <- df %>%
#'   dplyr::filter(Parameters == "TotalChla", StationCode == "MAI")
pr_get_NRSPigments <- function(Format = "all"){

  # Input validation
  assertthat::assert_that(
    is.character(Format) && length(Format) == 1,
    msg = "'Format' must be a single character string. Valid options are 'all' or 'binned'."
  )

  assertthat::assert_that(
    Format %in% c("all", "binned"),
    msg = "'Format' must be one of 'all' or 'binned'."
  )

  file <- "bgc_pigments_data"
  if(Format == "binned") {
    var_names <- c("TotalChla", "TotalChl", "PPC", "PSC", "PSP", "TCaro", "TAcc", "TPig", "TDP")
  } else {
    var_names <- c("Allo_mgm3", "AlphaBetaCar_mgm3", "Anth_mgm3", "Asta_mgm3", "BetaBetaCar_mgm3", "BetaEpiCar_mgm3", "Butfuco_mgm3",
                   "Cantha_mgm3", "CphlA_mgm3", "CphlB_mgm3", "CphlC1_mgm3", "CphlC2_mgm3", "CphlC3_mgm3", "CphlC1C2_mgm3",
                   "CphlideA_mgm3", "Diadchr_mgm3", "Diadino_mgm3",  "Fuco_mgm3", "Gyro_mgm3", "Hexfuco_mgm3", "Ketohexfuco_mgm3", "Lut_mgm3",
                   "Lyco_mgm3", "MgDvp_mgm3", "Neo_mgm3", "Perid_mgm3", "PhideA_mgm3", "PhytinA_mgm3", "PhytinB_mgm3", "Pras_mgm3",
                   "PyrophideA_mgm3", "PyrophytinA_mgm3", "Viola_mgm3", "Zea_mgm3", "Pigments_flag" )
  }

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("Pigments_flag") %>%
    dplyr::select(-"Pigments_flag") %>%
    dplyr::filter(.data$Project == "NRS", .data$SampleDepth_m != "WC") %>%
    pr_add_StationCode() %>%
    dplyr::rowwise() %>%
    pr_apply_Time()

  if(Format == "binned") {
    dat <- dat %>%
      dplyr::mutate(TotalChla = sum(.data$CphlideA_mgm3, .data$DvCphlA_mgm3, .data$CphlA_mgm3, na.rm = TRUE),
                    TotalChl = sum(.data$TotalChla, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3, .data$CphlC2_mgm3, .data$CphlC1_mgm3, na.rm = TRUE),
                    PPC = sum(.data$Allo_mgm3, .data$Diadchr_mgm3, .data$Diadino_mgm3, .data$Diato_mgm3, .data$Zea_mgm3,  na.rm = TRUE), # CARO, #Photoprotective Carotenoids
                    PSC = sum(.data$Butfuco_mgm3, .data$Hexfuco_mgm3, .data$Perid_mgm3,  na.rm = TRUE), # Photosynthetic Carotenoids
                    PSP = sum(.data$PSC, .data$TotalChl, na.rm = TRUE), # Photosynthetic pigments
                    TCaro = sum(.data$PSC, .data$PSP, na.rm = TRUE), # Total Carotenoids
                    TAcc = sum(.data$TCaro, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3, .data$CphlC2_mgm3, .data$CphlC1_mgm3,  na.rm = TRUE), # Total Accessory pigments
                    TPig = sum(.data$TAcc, .data$TotalChla,  na.rm = TRUE), # Total pigments
                    TDP = sum(.data$PSC, .data$Allo_mgm3, .data$Zea_mgm3, .data$DvCphlB_mgm3, .data$CphlB_mgm3,  na.rm = TRUE), # Total Diagnostic pigments
                    StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                    SampleDepth_m = as.numeric(.data$SampleDepth_m)) %>%
      dplyr::filter(.data$TotalChla != 0)
  }

  dat <- dat %>%
    dplyr::select("Project", "TripCode", "SampleTime_Local", "Month_Local", "SampleDepth_m", "StationName", "StationCode",
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    pr_reorder()

}


#' Load picophytoplankton data from NRS stations
#'
#' Load picophytoplankton abundance data measured by flow cytometry. Picophytoplankton 
#' are tiny photosynthetic cells (0.2-2 µm) that are important primary producers in 
#' oligotrophic (nutrient-poor) waters.
#' 
#' @details
#' The function retrieves abundance data for three groups of picophytoplankton:
#' 
#' * `Prochlorococcus_cellsmL` - Prochlorococcus abundance (cells/mL). 
#'   A tiny cyanobacterium found in warm, nutrient-poor tropical and subtropical waters. 
#'   Typically absent or in very low numbers in temperate Australian waters.
#' 
#' * `Synechococcus_cellsmL` - Synechococcus abundance (cells/mL). 
#'   A cyanobacterium found throughout Australian waters, more tolerant of cooler 
#'   temperatures and higher nutrients than Prochlorococcus.
#' 
#' * `Picoeukaryotes_cellsmL` - Picoeukaryotic algae abundance (cells/mL). 
#'   Small eukaryotic phytoplankton including prasinophytes and small diatoms.
#' 
#' Quality control flags are applied using [pr_apply_Flags()]. Water column (WC) 
#' samples are excluded. A small constant is added to zero values to allow log 
#' transformations in plotting.
#'
#' @return A dataframe in long format with picophytoplankton data including:
#'   * `StationCode`, `StationName` - Station identifiers
#'   * `SampleTime_Local` - Local sampling date and time
#'   * `SampleDepth_m` - Sampling depth (numeric, in metres)
#'   * `Parameters` - Cell type (Prochlorococcus, Synechococcus, or Picoeukaryotes)
#'   * `Values` - Cell abundance (cells/mL)
#' 
#' @seealso [pr_get_NRSMicro()] for broader microbial community data,
#'   [pr_get_NRSEnvContour()] to format data for contour plots,
#'   [pr_plot_NRSEnvContour()] for visualisation
#' 
#' @export
#'
#' @examples
#' df <- pr_get_NRSPico()
#' 
#' # Examine which stations have picophytoplankton data
#' table(df$StationCode, df$Parameters)
#' 
#' # Filter for Synechococcus at North Stradbroke Island
#' df_syn <- df %>%
#'   dplyr::filter(Parameters == "Synechococcus_cellsmL", StationCode == "NSI")
#' 
#' @importFrom rlang .data
pr_get_NRSPico <- function(){

  file <- "bgc_picoplankton_data"

  var_names <- c("Prochlorococcus_cellsmL", "Synechococcus_cellsmL", "Picoeukaryotes_cellsmL")
  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    dplyr::filter(.data$SampleDepth_m != "WC") %>%
    pr_apply_Flags() %>%
    pr_add_StationCode() %>%
    pr_apply_Time() %>%
    dplyr::mutate(SampleDepth_m = as.numeric(.data$SampleDepth_m)) %>%
    dplyr::select("Project", "TripCode", "SampleTime_Local", "Month_Local", "Year_Local",
                  "SampleDepth_m", "StationName", "StationCode",
                  tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    pr_reorder()

  # dat <- planktonr_dat(dat, type = NULL, survey = "NRS", variable = NULL)

  return(dat)
}

#' Prepare biogeochemical data for depth-time contour plots
#'
#' @param Data Use Chemistry, Pico or Micro, the depth stratified samples
#'
#' @return A dataframe to be used with pr_plot_NRSEnvContour
#' @export
#'
#' @examples
#' df <- pr_get_NRSEnvContour(Data = "Micro")
pr_get_NRSEnvContour <- function(Data = "Chemistry") {

  # Input validation
  assertthat::assert_that(
    is.character(Data) && length(Data) == 1,
    msg = "'Data' must be a single character string. Valid options are 'Chemistry', 'Pico' or 'Micro'."
  )

  assertthat::assert_that(
    Data %in% c("Chemistry", "Pico", "Micro"),
    msg = "'Data' must be one of 'Chemistry', 'Pico', or 'Micro'."
  )

  file <- parse(text = paste("planktonr::pr_get_NRS", Data, "()", sep = ""))

  dat <- eval(file) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) %>%
    dplyr::filter(.data$Parameters != 'SecchiDepth_m') %>%
    pr_remove_outliers(2) %>%
    droplevels() %>%
    pr_reorder()

}


#' Load microbial abundance data from flow cytometry
#' @param Survey NRS, Coastal or GO-SHIP stations
#'
#' @return A dataframe with NRS microbial data
#' @export
#'
#' @examples
#' df <- pr_get_NRSMicro(Survey = "GO-SHIP")
#' @importFrom rlang .data
#'
pr_get_NRSMicro <- function(Survey = "NRS"){

  # Input validation
  assertthat::assert_that(
    is.character(Survey) && length(Survey) == 1,
    msg = "'Survey' must be a single character string. Valid options are 'NRS', 'Coastal', or 'GO-SHIP'."
  )

  assertthat::assert_that(
    Survey %in% c("NRS", "Coastal", "GO-SHIP"),
    msg = "'Survey' must be one of 'NRS', 'Coastal', or 'GO-SHIP'."
  )

  var_names <- c("Prochlorococcus_cellsmL", "Synechococcus_cellsmL", "Picoeukaryotes_cellsmL",
                 "NifH_genes_per_mil_reads", "RuBisCo_genes_per_mil_reads", "fish_parasites",
                 "nitrogen_fixation_organisms",
                 "Bacterial_Salinity_Index_KD", "Bacterial_Salinity_Index_mean", "Bacterial_Salinity_Range",
                 "Bacterial_Salinity_Proportion", "Bacterial_Salinity_Bias", "Bacterial_Salinity_Diversity",
                 "Archaeal_Salinity_Index_KD", "Archaeal_Salinity_Index_mean", "Archaeal_Salinity_Range",
                 "Archaeal_Salinity_Proportion", "Archaeal_Salinity_Bias", "Archaeal_Salinity_Diversity",
                 "Eukaryote_Salinity_Index_KD", "Eukaryote_Salinity_Index_mean", "Eukaryote_Salinity_Range",
                 "Eukaryote_Salinity_Proportion", "Eukaryote_Salinity_Bias", "Eukaryote_Salinity_Diversity",
                 "Bacterial_Nitrogen_Index_KD", "Bacterial_Nitrogen_Index_mean", "Bacterial_Nitrogen_Range",
                 "Bacterial_Nitrogen_Proportion", "Bacterial_Nitrogen_Bias", "Bacterial_Nitrogen_Diversity",
                 "Archaeal_Nitrogen_Index_KD", "Archaeal_Nitrogen_Index_mean", "Archaeal_Nitrogen_Range",
                 "Archaeal_Nitrogen_Proportion", "Archaeal_Nitrogen_Bias", "Archaeal_Nitrogen_Diversity",
                 "Eukaryote_Nitrogen_Index_KD", "Eukaryote_Nitrogen_Index_mean", "Eukaryote_Nitrogen_Range",
                 "Eukaryote_Nitrogen_Proportion", "Eukaryote_Nitrogen_Bias", "Eukaryote_Nitrogen_Diversity",
                 "Bacterial_Phosphate_Index_KD", "Bacterial_Phosphate_Index_mean", "Bacterial_Phosphate_Range",
                 "Bacterial_Phosphate_Proportion", "Bacterial_Phosphate_Bias", "Bacterial_Phosphate_Diversity",
                 "Archaeal_Phosphate_Index_KD", "Archaeal_Phosphate_Index_mean", "Archaeal_Phosphate_Range",
                 "Archaeal_Phosphate_Proportion", "Archaeal_Phosphate_Bias", "Archaeal_Phosphate_Diversity",
                 "Eukaryote_Phosphate_Index_KD", "Eukaryote_Phosphate_Index_mean", "Eukaryote_Phosphate_Range",
                 "Eukaryote_Phosphate_Proportion", "Eukaryote_Phosphate_Bias", "Eukaryote_Phosphate_Diversity",
                 "Bacterial_Silicate_Index_KD", "Bacterial_Silicate_Index_mean", "Bacterial_Silicate_Range",
                 "Bacterial_Silicate_Proportion", "Bacterial_Silicate_Bias", "Bacterial_Silicate_Diversity",
                 "Archaeal_Silicate_Index_KD", "Archaeal_Silicate_Index_mean", "Archaeal_Silicate_Range",
                 "Archaeal_Silicate_Proportion", "Archaeal_Silicate_Bias", "Archaeal_Silicate_Diversity",
                 "Eukaryote_Silicate_Index_KD", "Eukaryote_Silicate_Index_mean", "Eukaryote_Silicate_Range",
                 "Eukaryote_Silicate_Proportion", "Eukaryote_Silicate_Bias", "Eukaryote_Silicate_Diversity",
                 "Bacterial_Oxygen_Index_KD", "Bacterial_Oxygen_Index_mean", "Bacterial_Oxygen_Range",
                 "Bacterial_Oxygen_Proportion", "Bacterial_Oxygen_Bias", "Bacterial_Oxygen_Diversity",
                 "Archaeal_Oxygen_Index_KD", "Archaeal_Oxygen_Index_mean", "Archaeal_Oxygen_Range",
                 "Archaeal_Oxygen_Proportion", "Archaeal_Oxygen_Bias", "Archaeal_Oxygen_Diversity",
                 "Eukaryote_Oxygen_Index_KD", "Eukaryote_Oxygen_Index_mean", "Eukaryote_Oxygen_Range",
                 "Eukaryote_Oxygen_Proportion", "Eukaryote_Oxygen_Bias", "Eukaryote_Oxygen_Diversity",
                 "Bacterial_Temperature_Index_KD", "Bacterial_Temperature_Index_mean",
                 "Bacterial_Temperature_Range", "Bacterial_Temperature_Proportion", "Bacterial_Temperature_Bias",
                 "Bacterial_Temperature_Diversity", "Archaeal_Temperature_Index_KD",
                 "Archaeal_Temperature_Index_mean", "Archaeal_Temperature_Range",
                 "Archaeal_Temperature_Proportion", "Archaeal_Temperature_Bias",
                 "Archaeal_Temperature_Diversity", "Eukaryote_Temperature_Index_KD",
                 "Eukaryote_Temperature_Index_mean", "Eukaryote_Temperature_Range",
                 "Eukaryote_Temperature_Proportion", "Eukaryote_Temperature_Bias",
                 "Eukaryote_Temperature_Diversity", "Bacteria_unique_ASVs", "Bacteria_shannon_index",
                 "Bacteria_simpsons_index", "Bacteria_invsimpson_index", "Bacteria_total_observations",
                 "Archaea_unique_ASVs", "Archaea_shannon_index", "Archaea_simpsons_index",
                 "Archaea_invsimpson_index", "Archaea_total_observations", "Eukaryote_unique_ASVs",
                 "Eukaryote_shannon_index", "Eukaryote_simpsons_index", "Eukaryote_invsimpson_index",
                 "Eukaryote_total_observations")

  if(Survey == "Coastal"){

    dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                           show_col_types = FALSE) %>%
      tidyr::drop_na(tidyselect::any_of(c("StationCode", "SampleDateUTC", "Time_24hr", "Year", "Month", "Day", "depth_m"))) %>%
      pr_rename() %>%
      dplyr::filter(is.na(.data$TripCode), .data$StationName %in% CSCodes$StationName) %>%
      dplyr::select("StationName", "SampleDateUTC", "Latitude", "Longitude", SampleDepth_m = "depth_m", tidyselect::any_of(var_names)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric),
                    tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE))

    times <- purrr::map2_vec(dat$SampleDateUTC, dat$tz, function(x,y) lubridate::with_tz(x, tzone = y))

    dat <- dat %>%
      dplyr::bind_cols(SampleTime_Local = times) %>%
      planktonr::pr_apply_Time() %>%
      dplyr::inner_join(CSCodes, by = "StationName") %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::arrange(.data$State, .data$Latitude) %>%
      dplyr::select(-c("SampleDateUTC", "tz"))

  } else if (Survey == "GO-SHIP"){

    GOSHIP <- c(seq(34369, 34678, 1), seq(36916, 37650, 1))

    dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                           show_col_types = FALSE) %>%
      pr_rename() %>%
      dplyr::mutate(sample = as.numeric(stringr::str_sub(.data$code, -5, -1)),
                    StationName = ifelse(grepl("GO", .data$StationName), stringr::str_sub(.data$code, -3, -1), .data$StationName)) %>%
      dplyr::filter(sample %in% GOSHIP) %>%
      dplyr::select("StationName", "SampleDateUTC", "Latitude", "Longitude", SampleDepth_m = "depth_m", tidyselect::any_of(var_names)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric),
                    tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE))

    times <- purrr::map2_vec(dat$SampleDateUTC, dat$tz, function(x,y) lubridate::with_tz(x, tzone = y))

    dat <- dat %>%
      dplyr::bind_cols(SampleTime_Local = times) %>%
      planktonr::pr_apply_Time() %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::arrange(.data$Latitude) %>%
      dplyr::select(-c("SampleDateUTC", "tz"))

  } else if (Survey == "NRS"){

    NRS <- pr_get_NRSTrips() %>%
      dplyr::select("TripCode", "SampleTime_Local", "Year_Local", "Month_Local", "StationName", "StationCode")

    dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                           show_col_types = FALSE) %>%
      planktonr_dat(Type = "Microbes", Survey = Survey) %>%
      pr_rename() %>%
      dplyr::select("TripCode", SampleDepth_m = "depth_m", tidyselect::any_of(var_names)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric))

    dat <- dat %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::left_join(NRS, by = "TripCode") %>%
      pr_reorder()
  }

  dat <- planktonr_dat(dat, Type = "Microbes", Survey = Survey, Variable = NULL)

  return(dat)

}

#' Load Coastal Seas chemistry and nutrient data
#'
#' @return A dataframe with NRS chemistry data
#' @export
#'
#' @examples
#' df <- pr_get_CSChem()
#' @importFrom rlang .data
pr_get_CSChem <- function(){

  var_names <- c("Silicate_umolL", "Nitrate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Chla_mgm3", "Temperature_degC",
                 "Salinity_psu", "Oxygen_umolL", "Turbidity_NTU", "Density_kgm3")

  dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                         show_col_types = FALSE) %>%
    tidyr::drop_na(tidyselect::any_of(c("StationCode", "SampleDateUTC", "Time_24hr", "Year", "Month", "Day", "depth_m"))) %>% # TODO these should fixed soon
    pr_rename() %>%
    dplyr::filter(is.na(.data$TripCode), .data$StationName %in% CSCodes$StationName) %>%
    dplyr::select("StationName", "SampleDateUTC", "Latitude", "Longitude", SampleDepth_m = "depth_m", tidyselect::any_of(var_names)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric),
                  tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE))

  times <- purrr::map2_vec(dat$SampleDateUTC, dat$tz, function(x,y) lubridate::with_tz(x, tzone = y))

  dat <- dat %>%
    dplyr::bind_cols(SampleTime_Local = times) %>%
    planktonr::pr_apply_Time() %>%
    dplyr::inner_join(CSCodes, by = "StationName") %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
    dplyr::arrange(.data$State, .data$Latitude) %>%
    dplyr::select(-c("SampleDateUTC", "tz"))

  # dat <- planktonr_dat(dat, type = NULL, survey = "NRS", variable = NULL)

  return(dat)

}


#' Load Total Suspended Solids (TSS) data
#'
#' @return A dataframe with NRS TSS data
#' @export
#'
#' @examples
#' df <- pr_get_NRSTSS()
pr_get_NRSTSS <- function(){
  file <- "bgc_tss_data"

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("TSSall_flag") %>%
    planktonr_dat(Type = "Water", Survey = "NRS")

  return(dat)
}


#' Load NRS CTD profile data (temperature, salinity, fluorescence)
#'
#' @return A dataframe with NRS CTD data
#' @export
#'
#' @examples
#' df <- pr_get_NRSCTD()
#' @importFrom rlang .data
pr_get_NRSCTD <- function(){

  file = "nrs_depth_binned_ctd_data"

  ctd_vars <- c("Project", "file_id", "StationName", "StationCode", "TripCode",
                "SampleTime_Local", "SampleTime_UTC", "Latitude", "Longitude", "SampleDepth_m",
                "Salinity_psu", "Temperature_degC", "DissolvedOxygen_umolkg", "ChlF_mgm3",
                "Turbidity_NTU", "Conductivity_Sm", "WaterDensity_kgm3")

  dat <- pr_get_Raw(file) %>%
    planktonr_dat(Type = "Water", Survey = "NRS") %>%
    pr_rename() %>%
    pr_add_StationCode() %>%
    dplyr::rename(ChlF_mgm3 = "Chla_mgm3") %>%
    #pr_apply_Flags() %>%  # TODO flags are small f, make function work with both
    dplyr::filter(!.data$file_id %in% c(2117, 2184, 2186, 2187)) %>% #TODO Check with Claire
    dplyr::select(tidyselect::all_of(ctd_vars)) %>%
    pr_apply_Time()

  return(dat)
}


#' Get NRS long term nutrient timeseries data
#'
#' @return dataframe for plotting long term nutrient time series info
#' @export
#'
#' @examples
#' df <- pr_get_LTnuts()
pr_get_LTnuts <-  function(){

  var_names <- c("Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Nitrate_umolL", "Nitrite_umolL",
                 "Oxygen_umolL", "Salinity", "Temperature_degC")

  NutsLT <- readr::read_csv(system.file("extdata", "LongTermMonitoringData.csv", package = "planktonr", mustWork = TRUE),
                            show_col_types = FALSE,
                            na = c("NA", ""),
                            col_types = readr::cols(NITRITE_VALUE = readr::col_number(),
                                                    NITRITE_QC_FLAG = readr::col_number(),
                                                    AMMONIA_VALUE = readr::col_number(),
                                                    AMMONIA_QC_FLAG = readr::col_number())) %>%
    planktonr_dat(Type = "Water", Survey = "LTM") %>%
    dplyr::mutate(StationCode = gsub(".*[-]([^.]+)[-].*", "\\1", .data$SURVEY_NAME),
                  Project = 'LTM',
                  Month_Local = lubridate::month(.data$START_TIME),
                  Year_Local = lubridate::year(.data$START_TIME)) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_Local = "START_TIME",
                  SampleDepth_m = "PRESSURE") %>%
    dplyr::select("StationCode", "Project", "SampleTime_Local", "Month_Local", "Year_Local",
                  "SampleDepth_m", tidyselect::all_of(var_names), tidyselect::contains("_Flag"),
                  -tidyselect::contains("ROSETTE_POSITION")) %>%
    pr_apply_Flags() %>%
    dplyr::select(-dplyr::contains("Flag")) %>%
    pr_add_StationName() %>%
    tidyr::pivot_longer(tidyselect::all_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    dplyr::filter(.data$Values != -999) %>%
    dplyr::relocate(c("Project", "StationName", "StationCode", tidyselect::everything())) %>%
    pr_reorder()

  Nuts <- pr_get_NRSChemistry() %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB"),
                  .data$Parameters != 'SecchiDepth_m') %>%
    dplyr::mutate(Year_Local = lubridate::year(.data$SampleTime_Local)) %>%
    dplyr::select(tidyselect::all_of(colnames(NutsLT))) # Ensure columns are in the same order

  CTD <- pr_get_NRSCTD() %>%
    dplyr::select("Project", "StationCode", "StationName", "Month_Local", "Year_Local",
                  "SampleTime_Local", "SampleDepth_m", "Temperature_degC") %>%
    dplyr::filter(.data$StationCode %in% c("MAI", "ROT", "PHB")) %>%
    tidyr::pivot_longer(-c("Project":"SampleDepth_m"), values_to = "Values", names_to = "Parameters") %>%
    dplyr::select(tidyselect::all_of(colnames(NutsLT)))

  dat <- dplyr::bind_rows(NutsLT, Nuts, CTD)

  return(dat)
}
