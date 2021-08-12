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
    dplyr::filter(grepl("NRS", site_code)) %>%
    dplyr::mutate(TripCode = ifelse(site_code == 'NRSDAR', paste0(substr(site_code,4,6), format(time_coverage_start, "%Y%m%d_%H:%M")),
                                    paste0(substr(site_code,4,6), format(time_coverage_start, "%Y%m%d"))),
                  StationName = dplyr::case_when(
                    site_code == "NRSDAR" ~ "Darwin",
                    site_code == "NRSYON" ~ "Yongala",
                    site_code == "NRSNSI" ~ "North Stradbroke Island",
                    site_code == "NRSPHB" ~ "Port Hacking",
                    site_code == "NRSMAI" ~ "Maria Island",
                    site_code == "NRSKAI" ~ "Kangaroo Island",
                    site_code == "NRSESP" ~ "Esperance",
                    site_code == "NRSROT" ~ "Rottnest Island",
                    site_code == "NRSNIN" ~ "Ningaloo"),
                  CPHL = ifelse(!is.na(CPHL), CPHL, CHLF)) %>%
    # dplyr::rename(CastTime_UTC = time_coverage_start, Latitude = LATITUDE, Longitude = LONGITUDE, SampleDepth_m = DEPTH, Salinity_psu = PSAL,
    #               Salinity_flag = PSAL_quality_control, Temperature_degC = TEMP, Temperature_flag = TEMP_quality_control, DissolvedOxygen_umolkg = DOX2,
    #               DissolvedOxygen_flag = DOX2_quality_control, Chla_mgm3 = CPHL, Chla_flag = CPHL_quality_control, Turbidity_NTU = TURB,
    #               Turbidity_flag = TURB_quality_control, Pressure_dbar = PRES_REL, Conductivity_Sm = CNDC, Conductivity_flag = CNDC_quality_control,
    #               WaterDensity_kgm3 = DENS, WaterDensity_flag = DENS_quality_control) %>%
    pr_rename() %>%
    dplyr::select(file_id, StationName, TripCode, CastTime_UTC, Latitude, Longitude, SampleDepth_m, Salinity_psu, Salinity_flag, Temperature_degC, Temperature_flag,
                  DissolvedOxygen_umolkg, DissolvedOxygen_flag, Chla_mgm3, Chla_flag, Turbidity_NTU, Turbidity_flag, Pressure_dbar, Conductivity_Sm,
                  Conductivity_flag, WaterDensity_kgm3, WaterDensity_flag) %>%
    dplyr::mutate(tz = lutz::tz_lookup_coords(Latitude, Longitude, method = "fast"),
                  CastTime_Local = dplyr::case_when(
                    tz == "Australia/Darwin" ~ format(CastTime_UTC, tz = "Australia/Darwin"),
                    tz == "Australia/Brisbane" ~ format(CastTime_UTC, tz = "Australia/Brisbane"),
                    tz == "Australia/Adelaide" ~ format(CastTime_UTC, tz = "Australia/Adelaide"),
                    tz == "Australia/Hobart" ~ format(CastTime_UTC, tz = "Australia/Hobart"),
                    tz == "Australia/Sydney" ~ format(CastTime_UTC, tz = "Australia/Sydney"),
                    tz == "Australia/Perth" ~ format(CastTime_UTC, tz = "Australia/Perth"))) %>%
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
      dplyr::select(file_id, CastTime_UTC, TripCode) %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::distinct()

    CastTimes <- rawCTDCast$CastTime_UTC

    Samps <- NRSSamp %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::select(SampleDateUTC, TripCode) %>%
      dplyr::distinct()

    dateSelect <- function(x){
      which.min(abs(x - CastTimes))
    }

    DateMatch <- sapply(Samps$SampleDateUTC, dateSelect)
    Samps$SampLevel <- DateMatch
    Samps$CastTime_UTC <- Samps$SampleDateUTC

    for (i in 1:nrow(Samps)){
      j <- Samps$SampLevel[[i]]
      Samps$CastTime_UTC[i] <- CastTimes[[j]]
    }

    Samps <- Samps %>%
      dplyr::mutate(DateDiff = abs(CastTime_UTC - SampleDateUTC) / 3600,
                    DateDiff = ifelse(DateDiff > 3 & station != "NSI", NA,
                                      ifelse(DateDiff > 15 & station %in% c("NSI", "KAI"), NA, DateDiff)))

    SampsMatch <- rawCTDCast %>%
      dplyr::filter(substr(TripCode, 1, 3) == station) %>%
      dplyr::select(CastTime_UTC, file_id) %>%
      dplyr::distinct()

    CastMatch <- Samps %>%
      tidyr::drop_na(DateDiff) %>%
      dplyr::inner_join(SampsMatch, by = "CastTime_UTC") %>%
      dplyr::select(file_id, TripCode)

    df <- df %>%
      dplyr::bind_rows(CastMatch) %>%
      tidyr::drop_na()
  }

  rawCTD <- rawCTD %>%
    dplyr::select(-TripCode) %>%
    dplyr::left_join(df, by = "file_id")

}
