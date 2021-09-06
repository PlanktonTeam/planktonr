#' Load CTD data
#'
#' @return A dataframe with NRS CTD data
#' @export
#'
#' @examples
#' df <- pr_get_CTD()
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_get_CTD <- function(){

  rawCTD <- readr::read_csv(paste0(pr_get_site(), "IMOS_-_Australian_National_Mooring_Network_(ANMN)_-_CTD_Profiles.csv"),
                            show_col_types = FALSE,
                            na = "",
                            comment = "#",
                            col_types = readr::cols(CHLU = readr::col_double(), # columns start with nulls so tidyverse annoyingly assigns col_logical()
                                                    CHLU_quality_control = readr::col_double(),
                                                    CPHL = readr::col_double(),
                                                    CPHL_quality_control = readr::col_double(),
                                                    cruise_id = readr::col_skip())) %>%
    pr_rename() %>%
    rename(SampleDepth_m = .data$DEPTH, Salinity_psu = .data$PSAL, Salinity_flag = .data$PSAL_quality_control,
           SampleDateUTC = .data$time_coverage_start, StationCode = .data$site_code, Temperature_degC = .data$TEMP,
           ChlF_mgm3 = .data$Chla_mgm3) %>% # Can't rename this in pr_rename due to replicate name
    filter(grepl("NRS", .data$StationCode)) %>% # Subset to NRS only
    mutate(TripCode = if_else(.data$StationCode == 'NRSDAR', paste0(substr(.data$StationCode,4,6), format(.data$SampleDateUTC, "%Y%m%d_%H:%M")),
                             paste0(substr(.data$StationCode,4,6), format(.data$SampleDateUTC, "%Y%m%d"))),
           ChlF_mgm3 = if_else(!is.na(.data$ChlF_mgm3), .data$ChlF_mgm3, .data$CHLF)) %>%
    pr_get_StationName() %>%
    select(.data$file_id, .data$StationName, .data$TripCode, .data$SampleDateUTC, .data$Latitude, .data$Longitude,
           .data$SampleDepth_m, .data$Salinity_psu, .data$Salinity_flag, .data$Temperature_degC, .data$Temperature_flag,
           .data$DissolvedOxygen_umolkg, .data$DissolvedOxygen_flag, .data$ChlF_mgm3, .data$Chla_flag,
           .data$Turbidity_NTU, .data$Turbidity_flag, .data$Pressure_dbar, .data$Conductivity_Sm,
           .data$Conductivity_flag, .data$WaterDensity_kgm3, .data$WaterDensity_flag) %>%
    mutate(tz = lutz::tz_lookup_coords(.data$Latitude, .data$Longitude, method = "fast", warn = FALSE),
           SampleDateLocal = case_when(
             .data$tz == "Australia/Darwin" ~ format(.data$SampleDateUTC, tz = "Australia/Darwin"),
             .data$tz == "Australia/Brisbane" ~ format(.data$SampleDateUTC, tz = "Australia/Brisbane"),
             .data$tz == "Australia/Adelaide" ~ format(.data$SampleDateUTC, tz = "Australia/Adelaide"),
             .data$tz == "Australia/Hobart" ~ format(.data$SampleDateUTC, tz = "Australia/Hobart"),
             .data$tz == "Australia/Sydney" ~ format(.data$SampleDateUTC, tz = "Australia/Sydney"),
             .data$tz == "Australia/Perth" ~ format(.data$SampleDateUTC, tz = "Australia/Perth"))) %>%
    filter(!.data$file_id %in% c(2117, 2184, 2186, 2187))

  NRSSamp <- pr_get_NRSTrips() %>%
    filter(stringr::str_detect(.data$TripCode, "PH4", negate = TRUE))

  Stations <- rawCTD %>%
    select(.data$TripCode) %>%
    mutate(stations = as.factor(substr(.data$TripCode, 1, 3))) %>%
    select(.data$stations) %>%
    distinct()

  df <- data.frame(file_id = NA, TripCode = NA)

  for (y in 1:nlevels(Stations$stations)){

    station <- levels(Stations$stations)[[y]]

    rawCTDCast <- rawCTD %>%
      select(.data$file_id, .data$SampleDateUTC, .data$TripCode) %>%
      filter(substr(.data$TripCode, 1, 3) == station) %>%
      distinct()

    CastTimes <- rawCTDCast$SampleDateUTC

    Samps <- NRSSamp %>%
      filter(substr(.data$TripCode, 1, 3) == station) %>%
      select(.data$SampleDateUTC, .data$TripCode) %>%
      distinct()

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
      mutate(DateDiff = as.numeric(abs(.data$SampleDateUTC - .data$SampleDateUTC) / 3600),
             DateDiff = case_when(.data$DateDiff > 3 & station != "NSI" ~ NA_real_,
                                  .data$DateDiff > 15 & station %in% c("NSI", "KAI") ~ NA_real_,
                                  TRUE ~ .data$DateDiff))

    SampsMatch <- rawCTDCast %>%
      filter(substr(.data$TripCode, 1, 3) == station) %>%
      select(.data$SampleDateUTC, .data$file_id) %>%
      distinct()

    CastMatch <- Samps %>%
      tidyr::drop_na(.data$DateDiff) %>%
      inner_join(SampsMatch, by = "SampleDateUTC") %>%
      select(.data$file_id, .data$TripCode)

    df <- df %>%
      bind_rows(CastMatch) %>%
      tidyr::drop_na()
  }

  rawCTD <- rawCTD %>%
    select(-.data$TripCode) %>%
    left_join(df, by = "file_id")

}
