#' Get plankton and biogeochemical data from IMOS surveys
#'
#' A unified interface for retrieving plankton abundance, biovolume, and
#' biogeochemical data from IMOS marine monitoring programs. This function
#' consolidates multiple survey-specific functions into a single entry point.
#'
#' @param Survey Survey program to retrieve data from:
#'   * `"NRS"` - National Reference Stations (default). Coastal monitoring stations
#'     around Australia with monthly sampling.
#'   * `"CPR"` - Continuous Plankton Recorder. Ship-towed sampler providing
#'     broad-scale spatial coverage along shipping routes.
#'   * `"SOTS"` - Southern Ocean Time Series. Deep-water mooring site south of
#'     Tasmania for monitoring Southern Ocean ecosystems.
#'   * `"Coastal"` - Coastal Seas microbial monitoring sites.
#'   * `"GO-SHIP"` - Global Ocean Ship-based Hydrographic Investigations Program.
#'
#' @param Type Data type to retrieve (case-insensitive for Phytoplankton/Zooplankton):
#'   * `"Phytoplankton"` - Phytoplankton abundance or biovolume (NRS, CPR, SOTS)
#'   * `"Zooplankton"` - Zooplankton abundance (NRS, CPR, SOTS)
#'   * `"Chemistry"` - Water chemistry including nutrients (NRS, Coastal)
#'   * `"Pigments"` - HPLC photosynthetic pigments (NRS only)
#'   * `"Pico"` - Picophytoplankton from flow cytometry (NRS only)
#'   * `"Micro"` - Microbial community data (NRS, Coastal, GO-SHIP)
#'   * `"TSS"` - Total Suspended Solids (NRS only)
#'   * `"CTD"` - CTD profiles: temperature, salinity, fluorescence (NRS only)
#'
#' @param Variable Variable to retrieve (required for Phytoplankton/Zooplankton):
#'   * `"abundance"` - Cell or individual counts
#'   * `"biovolume"` - Cell biovolume in cubic micrometres (Phytoplankton only)
#'
#' @param Subset Data aggregation level (required for Phytoplankton/Zooplankton):
#'   * `"raw"` - Raw taxonomic data as identified by analysts
#'   * `"htg"` - Higher taxonomic groups (e.g., diatoms, dinoflagellates)
#'   * `"genus"` - Aggregated to genus level
#'   * `"species"` - Aggregated to species level
#'   * `"copepods"` - Copepod data only (Zooplankton only)
#'
#' @param Format Output format for Pigments data:
#'   * `"all"` - All individual pigment concentrations (default)
#'   * `"binned"` - Pigments grouped into functional classes
#'
#' @details
#' ## Available Data by Survey
#'
#' | Survey | Available Types |
#' |--------|-----------------|
#' | NRS | Phytoplankton, Zooplankton, Chemistry, Pigments, Pico, Micro, TSS, CTD |
#' | CPR | Phytoplankton, Zooplankton |
#' | SOTS | Phytoplankton, Zooplankton |
#' | Coastal | Micro, Chemistry |
#' | GO-SHIP | Micro |
#'
#' ## Return Format
#'
#' The structure of the returned dataframe varies by data type:
#'
#' **Wide format** (taxa/variables as columns):
#' * Phytoplankton, Zooplankton: Taxa names as columns, samples as rows
#' * CTD: Measurement variables as columns, depth bins as rows
#'
#' **Long format** (Parameters and Values columns):
#' * Chemistry, Pigments, Pico, Micro, TSS: Standardised long format with
#'   `Parameters` column containing variable names and `Values` column
#'   containing measurements
#'
#' ## Parameter Requirements
#'
#' * `Variable` and `Subset` are required for Phytoplankton and Zooplankton
#' * `Format` is only used for Pigments (defaults to "all")
#' * Unused parameters will generate a warning
#'
#' @return A dataframe containing the requested data. Structure varies by
#'   data type (see Details).
#'
#' @seealso
#'   [pr_get_trips()] for sampling trip metadata,
#'   [pr_get_Indices()] for derived ecological indices,
#'   [pr_get_info()] for station and taxonomic information
#'
#' @export
#'
#' @examples
#' # Get NRS phytoplankton abundance data at genus level
#' dat <- pr_get_data(Survey = "NRS", Type = "Phytoplankton",
#'                    Variable = "abundance", Subset = "genus")
#'
#' # Get CPR zooplankton data
#' dat <- pr_get_data(Survey = "CPR", Type = "Zooplankton",
#'                    Variable = "abundance", Subset = "htg")
#'
#' # Get NRS water chemistry
#' dat <- pr_get_data(Survey = "NRS", Type = "Chemistry")
#'
#' # Get NRS pigments in binned format
#' dat <- pr_get_data(Survey = "NRS", Type = "Pigments", Format = "binned")
#'
#' # Get NRS CTD profiles
#' dat <- pr_get_data(Survey = "NRS", Type = "CTD")
#'
#' # Get GO-SHIP microbial data
#' dat <- pr_get_data(Survey = "GO-SHIP", Type = "Micro")
#'
#' # Get Coastal Seas chemistry data
#' dat <- pr_get_data(Survey = "Coastal", Type = "Chemistry")
#'
#' @importFrom rlang .data
pr_get_data <- function(Survey = "NRS",
                        Type = "Phytoplankton",
                        Variable = NULL,
                        Subset = NULL,
                        Format = "all") {

  # Normalize Type for case-insensitive matching of plankton types
  Type <- pr_check_type(Type)

  # Define valid combinations

  valid_types <- list(
    "NRS" = c("Phytoplankton", "Zooplankton", "Chemistry", "Pigments", "Pico", "Micro", "TSS", "CTD"),
    "CPR" = c("Phytoplankton", "Zooplankton"),
    "SOTS" = c("Phytoplankton", "Zooplankton"),
    "Coastal" = c("Micro", "Chemistry"),
    "GO-SHIP" = c("Micro")
  )

  # Types that require Variable and Subset
  types_requiring_variable <- c("Phytoplankton", "Zooplankton")

  # Types that use Format
  types_using_format <- c("Pigments")

  # --- Input Validation ---

  # Validate Survey
  assertthat::assert_that(
    is.character(Survey) && length(Survey) == 1,
    msg = "'Survey' must be a single character string. Valid options are 'NRS', 'CPR', 'SOTS', 'Coastal', or 'GO-SHIP'."
  )

  assertthat::assert_that(
    Survey %in% names(valid_types),
    msg = paste0("'Survey' must be one of: ", paste(names(valid_types), collapse = ", "), ".")
  )

  # Validate Type
  assertthat::assert_that(
    is.character(Type) && length(Type) == 1,
    msg = "'Type' must be a single character string."
  )

  # Check Type is valid for the selected Survey
  assertthat::assert_that(
    Type %in% valid_types[[Survey]],
    msg = paste0("'Type' = '", Type, "' is not available for Survey = '", Survey, "'. ",
                 "Valid types for ", Survey, " are: ", paste(valid_types[[Survey]], collapse = ", "), ".")
  )

  # --- Check for required parameters ---

  if (Type %in% types_requiring_variable) {
    # Variable is required
    assertthat::assert_that(
      !is.null(Variable),
      msg = paste0("'Variable' is required when Type = '", Type, "'. ",
                   "Valid options are 'abundance' or 'biovolume'.")
    )

    assertthat::assert_that(
      is.character(Variable) && length(Variable) == 1,
      msg = "'Variable' must be a single character string. Valid options are 'abundance' or 'biovolume'."
    )

    assertthat::assert_that(
      Variable %in% c("abundance", "biovolume"),
      msg = "'Variable' must be one of 'abundance' or 'biovolume'."
    )

    # Subset is required
    assertthat::assert_that(
      !is.null(Subset),
      msg = paste0("'Subset' is required when Type = '", Type, "'. ",
                   "Valid options are 'raw', 'htg', 'genus', 'species', or 'copepods'.")
    )

    assertthat::assert_that(
      is.character(Subset) && length(Subset) == 1,
      msg = "'Subset' must be a single character string."
    )

    assertthat::assert_that(
      Subset %in% c("raw", "htg", "genus", "species", "copepods"),
      msg = "'Subset' must be one of 'raw', 'htg', 'genus', 'species', or 'copepods'."
    )

    # Biovolume only available for Phytoplankton
    assertthat::assert_that(
      !(Variable == "biovolume" && Type == "Zooplankton"),
      msg = "'biovolume' is only available for Phytoplankton data. Please use 'abundance' for Zooplankton."
    )
  }

  # Validate Format for Pigments
  if (Type == "Pigments") {
    assertthat::assert_that(
      is.character(Format) && length(Format) == 1,
      msg = "'Format' must be a single character string. Valid options are 'all' or 'binned'."
    )

    assertthat::assert_that(
      Format %in% c("all", "binned"),
      msg = "'Format' must be one of 'all' or 'binned'."
    )
  }

  # --- Warn about unused parameters ---

  if (!Type %in% types_requiring_variable) {
    if (!is.null(Variable)) {
      warning("'Variable' is ignored for Type = '", Type, "'. ",
              "This parameter is only used for Phytoplankton and Zooplankton data.",
              call. = FALSE)
    }
    if (!is.null(Subset)) {
      warning("'Subset' is ignored for Type = '", Type, "'. ",
              "This parameter is only used for Phytoplankton and Zooplankton data.",
              call. = FALSE)
    }
  }

  if (!Type %in% types_using_format && Format != "all") {
    warning("'Format' is ignored for Type = '", Type, "'. ",
            "This parameter is only used for Pigments data.",
            call. = FALSE)
  }

  # --- Route to appropriate data retrieval ---

  if (Type %in% c("Phytoplankton", "Zooplankton")) {
    # Plankton data - use internal functions directly to avoid deprecation warnings
    dat <- .get_plankton_data(Survey = Survey, Type = Type, Variable = Variable, Subset = Subset)

  } else if (Type == "Chemistry") {
    if (Survey == "Coastal") {
      dat <- .get_coastal_chemistry_data()
    } else {
      dat <- .get_chemistry_data()
    }

  } else if (Type == "Pigments") {
    dat <- .get_pigments_data(Format = Format)

  } else if (Type == "Pico") {
    dat <- .get_pico_data()

  } else if (Type == "Micro") {
    dat <- .get_micro_data(Survey = Survey)

  } else if (Type == "TSS") {
    dat <- .get_tss_data()

  } else if (Type == "CTD") {
    dat <- .get_ctd_data()
  }

  return(dat)
}


# =============================================================================
# Internal helper functions - these contain the actual data retrieval logic
# =============================================================================

#' @noRd
.get_plankton_data <- function(Survey, Type, Variable, Subset) {

  Type <- pr_check_type(Type)

  if (Survey %in% c("NRS", "SOTS")) {
    if (Type == "Zooplankton" & Subset == "species") {
      # Specific case for NRS zooplankton species
      str <- pr_get_NonTaxaColumns(Survey = "NRS", Type = "Zooplankton")

      datc <- pr_get_Raw("bgc_zooplankton_abundance_copepods_data") %>%
        pr_rename()

      datnc <- pr_get_Raw("bgc_zooplankton_abundance_non_copepods_data") %>%
        pr_rename() %>%
        dplyr::select(-tidyselect::all_of(str[!str %in% "TripCode"]))

      dat <- dplyr::left_join(datc, datnc, by = "TripCode")

    } else {
      file <- paste("bgc", tolower(Type), Variable, Subset, "data", sep = "_")
      dat <- pr_get_Raw(file) %>%
        dplyr::mutate(StationCode = ifelse(grepl("SOTS", .data$StationCode), "SOTS", .data$StationCode),
                      StationName = ifelse(grepl("Remote Access Sampler", .data$StationName), "Southern Ocean Time Series", .data$StationName)) %>%
        pr_rename()
    }

    # Filter for SOTS if requested
    if (Survey == "SOTS") {
      dat <- dat %>%
        dplyr::filter(.data$StationCode == "SOTS")
    }

  } else if (Survey == "CPR") {
    if (Type == "Zooplankton" & Subset == "species") {
      # Specific case for CPR zooplankton species
      datc <- pr_get_Raw("cpr_zooplankton_abundance_copepods_data") %>%
        pr_rename() %>%
        dplyr::arrange(.data$TripCode, .data$SampleTime_Local)

      datnc <- pr_get_Raw("cpr_zooplankton_abundance_non_copepods_data") %>%
        pr_rename() %>%
        dplyr::arrange(.data$TripCode, .data$SampleTime_Local) %>%
        dplyr::select(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = "Zooplankton")))

      dat <- dplyr::bind_cols(datc, datnc)

    } else {
      file <- paste("cpr", tolower(Type), Variable, Subset, "data", sep = "_")

      dat <- readr::read_csv(stringr::str_replace(pr_get_Site(), "LAYER_NAME", file),
                             na = "",
                             show_col_types = FALSE,
                             comment = "#") %>%
        pr_rename() %>%
        planktonr_dat(Type = Type, Survey = "CPR")
    }
  }

  return(dat)
}


#' @noRd
.get_chemistry_data <- function() {

  var_names <- c("SecchiDepth_m", "Silicate_umolL", "Phosphate_umolL", "Ammonium_umolL",
                 "Nitrate_umolL", "Nitrite_umolL", "Oxygen_umolL", "DIC_umolkg",
                 "Alkalinity_umolkg", "Salinity", "NOx_umolL", "DIN_umolL", "Redfield")

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
    pr_reorder() %>%
    planktonr_dat(Type = "Water", Survey = "NRS")

  return(dat)
}


#' @noRd
.get_coastal_chemistry_data <- function() {

  var_names <- c("Silicate_umolL", "Nitrate_umolL", "Phosphate_umolL", "Ammonium_umolL", "Chla_mgm3", "Temperature_degC",
                 "Salinity_psu", "Oxygen_umolL", "Turbidity_NTU", "Density_kgm3")

 dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                         show_col_types = FALSE) %>%
    tidyr::drop_na(tidyselect::any_of(c("StationCode", "SampleDateUTC", "Time_24hr", "Year", "Month", "Day", "depth_m"))) %>%
    pr_rename() %>%
    dplyr::filter(is.na(.data$TripCode), .data$StationName %in% CSCodes$StationName) %>%
    dplyr::select("StationName", "SampleDateUTC", "Latitude", "Longitude", SampleDepth_m = "depth_m", tidyselect::any_of(var_names)) %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(var_names), as.numeric),
                  tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE))

  times <- purrr::map2_vec(dat$SampleDateUTC, dat$tz, function(x,y) lubridate::with_tz(x, tzone = y))

  dat <- dat %>%
    dplyr::bind_cols(SampleTime_Local = times) %>%
    pr_apply_Time() %>%
    dplyr::inner_join(CSCodes, by = "StationName") %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
    dplyr::arrange(.data$State, .data$Latitude) %>%
    dplyr::select(-c("SampleDateUTC", "tz")) %>%
    planktonr_dat(Type = "Chemistry", Survey = "Coastal")

  return(dat)
}


#' @noRd
.get_pigments_data <- function(Format) {

  file <- "bgc_pigments_data"

  if (Format == "binned") {
    var_names <- c("TotalChla", "TotalChl", "PPC", "PSC", "PSP", "TCaro", "TAcc", "TPig", "TDP")
  } else {
    var_names <- c("Allo_mgm3", "AlphaBetaCar_mgm3", "Anth_mgm3", "Asta_mgm3", "BetaBetaCar_mgm3",
                   "BetaEpiCar_mgm3", "Butfuco_mgm3", "Cantha_mgm3", "CphlA_mgm3", "CphlB_mgm3",
                   "CphlC1_mgm3", "CphlC2_mgm3", "CphlC3_mgm3", "CphlC1C2_mgm3", "CphlideA_mgm3",
                   "Diadchr_mgm3", "Diadino_mgm3", "Fuco_mgm3", "Gyro_mgm3", "Hexfuco_mgm3",
                   "Ketohexfuco_mgm3", "Lut_mgm3", "Lyco_mgm3", "MgDvp_mgm3", "Neo_mgm3",
                   "Perid_mgm3", "PhideA_mgm3", "PhytinA_mgm3", "PhytinB_mgm3", "Pras_mgm3",
                   "PyrophideA_mgm3", "PyrophytinA_mgm3", "Viola_mgm3", "Zea_mgm3", "Pigments_flag")
  }

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("Pigments_flag") %>%
    dplyr::select(-"Pigments_flag") %>%
    dplyr::filter(.data$Project == "NRS", .data$SampleDepth_m != "WC") %>%
    pr_add_StationCode() %>%
    dplyr::rowwise() %>%
    pr_apply_Time()

  if (Format == "binned") {
    dat <- dat %>%
      dplyr::mutate(TotalChla = sum(.data$CphlideA_mgm3, .data$DvCphlA_mgm3, .data$CphlA_mgm3, na.rm = TRUE),
                    TotalChl = sum(.data$TotalChla, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3,
                                   .data$CphlC2_mgm3, .data$CphlC1_mgm3, na.rm = TRUE),
                    PPC = sum(.data$Allo_mgm3, .data$Diadchr_mgm3, .data$Diadino_mgm3, .data$Diato_mgm3,
                              .data$Zea_mgm3, na.rm = TRUE),
                    PSC = sum(.data$Butfuco_mgm3, .data$Hexfuco_mgm3, .data$Perid_mgm3, na.rm = TRUE),
                    PSP = sum(.data$PSC, .data$TotalChl, na.rm = TRUE),
                    TCaro = sum(.data$PSC, .data$PSP, na.rm = TRUE),
                    TAcc = sum(.data$TCaro, .data$DvCphlB_mgm3, .data$CphlB_mgm3, .data$CphlC3_mgm3,
                               .data$CphlC2_mgm3, .data$CphlC1_mgm3, na.rm = TRUE),
                    TPig = sum(.data$TAcc, .data$TotalChla, na.rm = TRUE),
                    TDP = sum(.data$PSC, .data$Allo_mgm3, .data$Zea_mgm3, .data$DvCphlB_mgm3,
                              .data$CphlB_mgm3, na.rm = TRUE),
                    StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                    SampleDepth_m = as.numeric(.data$SampleDepth_m)) %>%
      dplyr::filter(.data$TotalChla != 0)
  }

  dat <- dat %>%
    dplyr::select("Project", "TripCode", "SampleTime_Local", "Month_Local", "SampleDepth_m",
                  "StationName", "StationCode", tidyselect::any_of(var_names)) %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = 'Parameters') %>%
    pr_reorder() %>%
    planktonr_dat(Type = "Pigments", Survey = "NRS")

  return(dat)
}


#' @noRd
.get_pico_data <- function() {

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
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values > 0], na.rm = TRUE)) %>%
    pr_reorder() %>%
    planktonr_dat(Type = "Pico", Survey = "NRS")

  return(dat)
}


#' @noRd
.get_micro_data <- function(Survey) {

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

  if (Survey == "Coastal") {

    dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                           show_col_types = FALSE) %>%
      tidyr::drop_na(tidyselect::any_of(c("StationCode", "SampleDateUTC", "Time_24hr", "Year", "Month", "Day", "depth_m"))) %>%
      pr_rename() %>%
      dplyr::filter(is.na(.data$TripCode), .data$StationName %in% CSCodes$StationName) %>%
      dplyr::select("StationName", "SampleDateUTC", "Latitude", "Longitude", SampleDepth_m = "depth_m",
                    tidyselect::any_of(var_names)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric),
                    tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE))

    times <- purrr::map2_vec(dat$SampleDateUTC, dat$tz, function(x, y) lubridate::with_tz(x, tzone = y))

    dat <- dat %>%
      dplyr::bind_cols(SampleTime_Local = times) %>%
      pr_apply_Time() %>%
      dplyr::inner_join(CSCodes, by = "StationName") %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::arrange(.data$State, .data$Latitude) %>%
      dplyr::select(-c("SampleDateUTC", "tz"))

  } else if (Survey == "GO-SHIP") {

    GOSHIP <- c(seq(34369, 34678, 1), seq(36916, 37650, 1))

    dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                           show_col_types = FALSE) %>%
      pr_rename() %>%
      dplyr::mutate(sample = as.numeric(stringr::str_sub(.data$code, -5, -1)),
                    StationName = ifelse(grepl("GO", .data$StationName), stringr::str_sub(.data$code, -3, -1), .data$StationName)) %>%
      dplyr::filter(sample %in% GOSHIP) %>%
      dplyr::select("StationName", "SampleDateUTC", "Latitude", "Longitude", SampleDepth_m = "depth_m",
                    tidyselect::any_of(var_names)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric),
                    tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE))

    times <- purrr::map2_vec(dat$SampleDateUTC, dat$tz, function(x, y) lubridate::with_tz(x, tzone = y))

    dat <- dat %>%
      dplyr::bind_cols(SampleTime_Local = times) %>%
      pr_apply_Time() %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::arrange(.data$Latitude) %>%
      dplyr::select(-c("SampleDateUTC", "tz"))

  } else if (Survey == "NRS") {

    NRS <- pr_get_trips(Survey = "NRS") %>%
      dplyr::select("TripCode", "SampleTime_Local", "Year_Local", "Month_Local", "StationName", "StationCode")

    dat <- readr::read_csv("https://raw.githubusercontent.com/AusMicrobiome/microbial_ocean_atlas/main/data/oceanViz_AM_data.csv",
                           show_col_types = FALSE) %>%
      pr_rename() %>%
      dplyr::select("TripCode", SampleDepth_m = "depth_m", tidyselect::any_of(var_names)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(var_names), as.numeric))

    dat <- dat %>%
      tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters") %>%
      dplyr::left_join(NRS, by = "TripCode")
  }

  dat <- planktonr_dat(dat, Type = "Microbes", Survey = Survey, Variable = NULL) %>%
    pr_reorder()

  return(dat)
}


#' @noRd
.get_tss_data <- function() {
  file <- "bgc_tss_data"

  dat <- pr_get_Raw(file) %>%
    pr_rename() %>%
    pr_apply_Flags("TSSall_flag") %>%
    planktonr_dat(Type = "Water", Survey = "NRS")

  return(dat)
}


#' @noRd
.get_ctd_data <- function() {

  file <- "nrs_depth_binned_ctd_data"

  ctd_vars <- c("Project", "file_id", "StationName", "StationCode", "TripCode",
                "SampleTime_Local", "SampleTime_UTC", "Latitude", "Longitude", "SampleDepth_m",
                "Salinity_psu", "Temperature_degC", "DissolvedOxygen_umolkg", "ChlF_mgm3",
                "Turbidity_NTU", "Conductivity_Sm", "WaterDensity_kgm3")

  dat <- pr_get_Raw(file) %>%
    planktonr_dat(Type = "Water", Survey = "NRS") %>%
    pr_rename() %>%
    pr_add_StationCode() %>%
    dplyr::rename(ChlF_mgm3 = "Chla_mgm3") %>%
    dplyr::filter(!.data$file_id %in% c(2117, 2184, 2186, 2187)) %>%
    dplyr::select(tidyselect::all_of(ctd_vars)) %>%
    pr_apply_Time()

  return(dat)
}
