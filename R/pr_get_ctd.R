#' Load CTD data
#'
#' @return A dataframe with NRS CTD data
#' @export
#'
#' @examples
#' df <- pr_get_CTD()
#' @importFrom magrittr "%>%"
pr_get_CTD <- function(){

  rawCTD <- readr::read_csv(paste0(pr_get_site(), "IMOS_-_Australian_National_Mooring_Network_(ANMN)_-_CTD_Profiles.csv"), na = "", skip = 29,
                            col_types = readr::cols(CHLU = readr::col_double(), # columns start with nulls so tidyverse annoyingly assigns col_logical()
                                                    CHLU_quality_control = readr::col_double(),
                                                    CPHL = readr::col_double(),
                                                    CPHL_quality_control = readr::col_double(),
                                                    cruise_id = readr::col_skip())) %>%
    pr_rename() %>%
    dplyr::rename(SampleDepth_m = DEPTH, Salinity_psu = PSAL, Salinity_flag = PSAL_quality_control,
                  SampleDateUTC = time_coverage_start, StationCode = site_code) %>% # Can't rename this in pr_rename due to replicate name
    dplyr::filter(grepl("NRS", StationCode)) %>% # Subset to NRS only
    dplyr::mutate(TripCode = ifelse(StationCode == 'NRSDAR', paste0(substr(StationCode,4,6), format(SampleDateUTC, "%Y%m%d_%H:%M")),
                                    paste0(substr(StationCode,4,6), format(SampleDateUTC, "%Y%m%d"))),
                  Chla_mgm3 = ifelse(!is.na(Chla_mgm3), Chla_mgm3, CHLF)) %>%
    pr_get_StationName() %>%
    dplyr::select(file_id, StationName, TripCode, SampleDateUTC, Latitude, Longitude,
                  SampleDepth_m, Salinity_psu, Salinity_flag, Temperature_degC, Temperature_flag,
                  DissolvedOxygen_umolkg, DissolvedOxygen_flag, Chla_mgm3, Chla_flag,
                  Turbidity_NTU, Turbidity_flag, Pressure_dbar, Conductivity_Sm,
                  Conductivity_flag, WaterDensity_kgm3, WaterDensity_flag) %>%
    dplyr::mutate(tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast", warn = FALSE),
                  SampleDateLocal = dplyr::case_when(
                    tz == "Australia/Darwin" ~ format(SampleDateUTC, tz = "Australia/Darwin"),
                    tz == "Australia/Brisbane" ~ format(SampleDateUTC, tz = "Australia/Brisbane"),
                    tz == "Australia/Adelaide" ~ format(SampleDateUTC, tz = "Australia/Adelaide"),
                    tz == "Australia/Hobart" ~ format(SampleDateUTC, tz = "Australia/Hobart"),
                    tz == "Australia/Sydney" ~ format(SampleDateUTC, tz = "Australia/Sydney"),
                    tz == "Australia/Perth" ~ format(SampleDateUTC, tz = "Australia/Perth"))) %>%
    dplyr::filter(!file_id %in% c(2117, 2184, 2186, 2187))

  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::filter(stringr::str_detect(TripCode, "PH4", negate = TRUE))

  Stations <- rawCTD %>%
    dplyr::select(TripCode) %>%
    dplyr::mutate(stations = as.factor(substr(TripCode, 1, 3))) %>%
    dplyr::select(stations) %>%
    dplyr::distinct()

  df <- data.frame(file_id = NA, TripCode = NA)

  for (y in 1:nlevels(Stations$stations)){
    station <- levels(Stations$stations)[[y]]
    rawCTDCast <- rawCTD %>%
      dplyr::select(file_id, SampleDateUTC, TripCode) %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::distinct()

    CastTimes <- rawCTDCast$SampleDateUTC

    Samps <- NRSSamp %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::select(SampleDateUTC, TripCode) %>%
      dplyr::distinct()

    dateSelect <- function(x){
      which.min(abs(x - CastTimes))
    }

    DateMatch <- sapply(Samps$SampleDateUTC, dateSelect)
    Samps$SampLevel <- DateMatch
    Samps$SampleDateUTC <- Samps$SampleDateUTC

    for (i in 1:nrow(Samps)){
      j <- Samps$SampLevel[[i]]
      Samps$SampleDateUTC[i] <- CastTimes[[j]]
    }

    Samps <- Samps %>%
      dplyr::mutate(DateDiff = abs(SampleDateUTC - SampleDateUTC) / 3600,
                    DateDiff = ifelse(DateDiff > 3 & station != "NSI", NA,
                                      ifelse(DateDiff > 15 & station %in% c("NSI", "KAI"), NA, DateDiff)))

    SampsMatch <- rawCTDCast %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::select(SampleDateUTC, file_id) %>%
      dplyr::distinct()

    CastMatch <- Samps %>%
      tidyr::drop_na(DateDiff) %>%
      dplyr::inner_join(SampsMatch, by = "SampleDateUTC") %>%
      dplyr::select(file_id, TripCode)

    df <- df %>%
      dplyr::bind_rows(CastMatch) %>%
      tidyr::drop_na()
  }

  rawCTD <- rawCTD %>%
    dplyr::select(-TripCode) %>%
    dplyr::left_join(df, by = "file_id")

}
