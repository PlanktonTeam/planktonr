#' Get NRS larval fish abundance data
#'
#' Load and format larval fish count data from NRS biogeochemistry sampling. 
#' Data includes counts and abundance (standardised to 1000 m³) of fish larvae 
#' identified to family or species level.
#'
#' @details
#' ## Data Collection
#' Larval fish samples are collected at NRS stations using:
#' * Vertical net tows through the water column
#' * Various mesh sizes (typically 100-500 µm)
#' * Sample volumes calculated from net dimensions and tow depth
#' 
#' Fish larvae are identified to the lowest possible taxonomic level, typically 
#' family but sometimes genus or species for distinctive forms.
#' 
#' ## Data Format
#' The function returns data in long format with:
#' * One row per species per sample
#' * Raw counts and standardised abundance (per 1000 m³)
#' * Environmental data (temperature, salinity)
#' * Sampling metadata (mesh size, volume filtered, tow type)
#' 
#' ## Species Names
#' The `Species` column contains coded names (e.g., "Acanthuridae_37437900") 
#' combining family name and WoRMS taxonomic ID. The `Species2` column provides 
#' formatted names for display purposes.
#' 
#' ## Taxonomic Codes
#' Family names in the data typically end with "idae" (e.g., Acanthuridae for 
#' surgeonfishes, Carangidae for trevallies). Codes include:
#' * Family name
#' * WoRMS AphiaID (taxonomic identifier)
#' * Life stage code (3 = larval stage)
#' 
#' ## Data Availability
#' Larval fish sampling is conducted opportunistically during NRS voyages. Not 
#' all stations or months have larval fish data.
#' 
#' ## Quality Control
#' The `QC_flag` column indicates data quality:
#' * `1` = Good data
#' * `2` = Probably good data
#' * `3` = Probably bad data (use with caution)
#' * `4` = Bad data (do not use)
#'
#' @return A dataframe with columns:
#'   * **StationName**: NRS station name
#'   * **Latitude**, **Longitude**: Sample location
#'   * **TripCode**: Unique voyage identifier
#'   * **SampleTime_UTC**, **SampleTime_Local**: Sampling time
#'   * **Year_Local**, **Month_Local**, **Day_Local**: Date components
#'   * **SampleDepth_m**: Maximum depth of tow
#'   * **Temperature_degC**, **Salinity_psu**: Surface environmental data
#'   * **Volume_m3**: Volume of water filtered (m³)
#'   * **Vessel**: Research vessel name
#'   * **TowType**: Vertical or oblique
#'   * **GearMesh_um**: Net mesh size (micrometres)
#'   * **Bathymetry_m**: Bottom depth at station
#'   * **Species**: Coded species name with WoRMS ID
#'   * **Species2**: Formatted species name for display
#'   * **Count**: Number of larvae in sample
#'   * **Abundance_1000m3**: Larvae per 1000 m³
#'   * **QC_flag**: Data quality flag (1-4)
#' 
#' @seealso 
#' * [pr_get_NRSData()] for plankton data from the same stations
#' 
#' @export
#'
#' @examples
#' # Load larval fish data
#' dat <- pr_get_LFData()
#'
#' # Check which families are most common
#' dat %>%
#'   dplyr::group_by(Species) %>%
#'   dplyr::summarise(TotalCount = sum(Count, na.rm = TRUE)) %>%
#'   dplyr::arrange(dplyr::desc(TotalCount))
#'
#' # Abundance at Maria Island
#' dat %>%
#'   dplyr::filter(StationName == "Maria Island",
#'                 QC_flag == 1) %>%
#'   dplyr::group_by(Year_Local, Species2) %>%
#'   dplyr::summarise(MeanAbundance = mean(Abundance_1000m3, na.rm = TRUE),
#'                    .groups = "drop")
pr_get_LFData <- function(){
  dat <- planktonr::pr_get_Raw("bgc_larval_fish_count_raw_data") %>%
    pr_rename() %>%
    planktonr_dat(Survey = "NRS", Type = "Fish") %>%
    tidyr::pivot_longer(cols = -c("Project", "StationName", "Latitude", "Longitude", "TripCode",
                                  "SampleTime_UTC", "SampleTime_Local", "Year_Local", "Month_Local",
                                  "Day_Local", "Time_Local24hr", "SampleDepth_m", "Temperature_degC",
                                  "Salinity_psu", "Volume_m3", "Vessel", "TowType", "GearMesh_um",
                                  "Bathymetry_m", "QC_flag"), names_to = "Species", values_to = "Count") %>%
    dplyr::mutate(Species2 = stringr::str_replace(.data$Species, "_3", " (3"),
                  Species2 = stringr::str_replace(.data$Species2, "_", ": "),
                  Species2 = stringr::str_replace(.data$Species2, stringr::coll("."), " "),
                  Species2 = paste0(.data$Species2, ")")) %>%
    dplyr::mutate(Abundance_1000m3 = (.data$Count/.data$Volume_m3)*1e3)

}

