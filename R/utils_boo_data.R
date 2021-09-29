#' Access data for timeseries and climatology plots
#'
#' @param Survey CPR or NRS, defaults to NRS
#' @param Type Phyto or zoo, defaults to phyto
#'
#' @return dataframe to use in pr_plot functions
#' @export
#'
#' @examples
#' df <- pr_get_tsdata("NRS", "Z")
pr_get_tsdata <- function(Survey = c("CPR", "NRS"), Type = c("P", "Z")){

  if(Type == "Z"){
    parameter1 <- "Biomass_mgm3"
    parameter2 <- "CopepodEvenness"
  }
  if(Type == "P" & Survey == "CPR")
  {
    parameter1 <- "PhytoBiomassCarbon_pgm3"
    parameter2 <- "DinoflagellateEvenness"
  }
  if(Type == "P" & Survey == "NRS"){
    parameter1 <- "PhytoBiomassCarbon_pgL"
    parameter2 <- "DinoflagellateEvenness"
  }

    if(Survey == 'CPR'){
      dat <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "CPR_Indices.csv"), na = "NA", show_col_types = FALSE) %>%
        dplyr::select(.data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$BioRegion, .data$Biomass_mgm3, .data[[parameter1]]:.data[[parameter2]]) %>%
        dplyr::mutate(Biomass_mgm3 = ifelse(.data$Biomass_mgm3 < 0 , 0, .data$Biomass_mgm3),
                      SampleDateUTC = lubridate::round_date(.data$SampleDateUTC, "month"),
                      YearMon = paste(.data$Year, .data$Month)) %>% # this step can be improved when nesting supports data pronouns
        tidyr::complete(.data$BioRegion, .data$YearMon) %>%
        dplyr::mutate(Year = as.numeric(stringr::str_sub(.data$YearMon, 1, 4)),
                      Month = as.numeric(stringr::str_sub(.data$YearMon, -2, -1))) %>%
        tidyr::pivot_longer(.data[[parameter1]]:.data[[parameter2]], values_to = "Values", names_to = 'parameters') %>%
        dplyr::group_by(.data$SampleDateUTC, .data$Year, .data$Month, .data$BioRegion, .data$parameters) %>%
        dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                         .groups = "drop") %>%
        pr_reorder() %>%
        dplyr::filter(!is.na(.data$BioRegion), !.data$BioRegion %in% c('North', 'North-west')) %>%
        droplevels()
      return(dat)
  } else
  {
    dat <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "NRS_Indices.csv"), na = "NA", show_col_types = FALSE) %>%
        dplyr::mutate(Month = lubridate::month(.data$SampleDateLocal),
                  Year = lubridate::year(.data$SampleDateLocal),
                  StationCode = paste(.data$StationName, .data$StationCode)) %>%
      #tidyr::complete(.data$Year, tidyr::nesting(Station, Code)) %>% # Nesting doesn't support data pronouns at this time
      tidyr::complete(.data$Year, .data$StationCode) %>%
      dplyr::mutate(StationName = stringr::str_sub(.data$StationCode, 1, -5),
                    StationCode = stringr::str_sub(.data$StationCode, -3, -1)) %>%
      dplyr::select(.data$Year, .data$Month, .data$SampleDateLocal, .data$StationName, .data$StationCode, .data[[parameter1]]:.data[[parameter2]]) %>%
      tidyr::pivot_longer(-c(.data$Year:.data$StationCode), values_to = 'Values', names_to = "parameters") %>%
      pr_reorder()
   return(dat)
      }
}

#' To produce the climatology for plotting
#'
#' @param df output of pr_get_tsdata
#' @param x Year, Month, Day, time period of climatology
#'
#' @return dataframe to use in pr_plot_climate functions
#' @export
#'
#' @examples
#' df <- data.frame(Month = rep(1:12,10), StationCode = 'NSI', Values = runif(120, min=0, max=10))
#' pr_make_climatology(df, Month)
#' @import dplyr
#' @importFrom stats sd
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
pr_make_climatology <- function(df, x){
  x <- dplyr::enquo(arg = x)
  df_climate <- df %>% dplyr::filter(!!x != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, StationCode)
    dplyr::group_by(!!x, .data$StationCode) %>%
    dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                     N = length(.data$Values),
                     sd = stats::sd(.data$Values, na.rm = TRUE),
                     se = sd / sqrt(.data$N),
                     .groups = "drop")
  return(df_climate)
}

#' Get NRS nutrient timeseries data
#'
#' @return dataframe for plotting nutrient time series info
#' @export
#'
#' @examples
#' df <- pr_get_nuts()
pr_get_nuts <-  function(){
  Nuts <- readr::read_csv(paste0(planktonr::pr_get_site(), "BGC_Chemistry.csv"),
                        col_types = list(SAMPLEDATELOCAL = readr::col_datetime())) %>%
    dplyr::select_if(!grepl('FLAG', names(.)) & !grepl('COMMENTS', names(.)) & !grepl('MICROB', names(.))) %>%
    dplyr::filter(.data$PROJECTNAME == 'NRS') %>%
    pr_rename() %>%
    dplyr::mutate(StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                  Month = lubridate::month(.data$SampleDateLocal)) %>%
    dplyr::select(-.data$TripCode) %>%
    tidyr::pivot_longer(.data$Salinity_psu:.data$Oxygen_umolL, values_to = "Values", names_to = 'parameters') %>%
    pr_get_StationName() %>%
    pr_reorder()
}

#' Get NRS pigment timeseries data
#'
#' @return dataframe for plotting pigment time series info
#' @export
#'
#' @examples
#' df <- pr_get_pigs()
pr_get_pigs <-  function(){
  Pigs  <- readr::read_csv(paste0(planktonr::pr_get_site(), "BGC_Pigments.csv"),
                           col_types = list(PROJECTNAME = readr::col_character(),
                                            TRIP_CODE = readr::col_character(),
                                            SAMPLEDATELOCAL = readr::col_datetime(),
                                            SAMPLEDEPTH_M = readr::col_character(),
                                            .default = readr::col_double())) %>%
    dplyr::select_if(!grepl('FLAG', names(.)) & !grepl('COMMENTS', names(.))) %>%
    pr_rename() %>%
    dplyr::filter(.data$ProjectName == 'NRS', .data$SampleDepth_m != 'WC') %>%
    dplyr::rowwise() %>%
    dplyr::mutate(SampleDepth_m = as.numeric(.data$SampleDepth_m),
                  TotalChla = sum(.data$CPHLIDE_A, .data$DV_CPHL_A, .data$CPHL_A, na.rm = TRUE),
                  TotalChl = sum(.data$CPHLIDE_A, .data$DV_CPHL_A, .data$CPHL_A, .data$DV_CPHL_B, .data$CPHL_B, .data$CPHL_C3, .data$CPHL_C2, .data$CPHL_C1, na.rm = TRUE),
                  PPC = sum(.data$ALLO, .data$DIADCHR, .data$DIADINO, .data$DIATO, .data$ZEA,  na.rm = TRUE),#+ CARO, #Photoprotective Carotenoids
                  PSC = sum(.data$BUT_FUCO, .data$HEX_FUCO, .data$PERID,  na.rm = TRUE),#Photosynthetic Carotenoids
                  PSP = sum(.data$PSC, .data$TotalChl,  na.rm = TRUE),#Photosynthetic pigments
                  TCaro = sum(.data$PSC, .data$PSP,  na.rm = TRUE),#Total Carotenoids
                  TAcc = sum(.data$TCaro, .data$DV_CPHL_B, .data$CPHL_B, .data$CPHL_C3, .data$CPHL_C2, .data$CPHL_C1,  na.rm = TRUE),#Total Accessory pigments
                  TPig = sum(.data$TAcc, .data$TotalChla,  na.rm = TRUE),#Total pigments
                  TDP = sum(.data$PSC, .data$ALLO, .data$ZEA, .data$DV_CPHL_B, .data$CPHL_B,  na.rm = TRUE),#Total Diagnostic pigments
                  StationCode = stringr::str_sub(.data$TripCode, 1, 3),
                  Month = lubridate::month(.data$SampleDateLocal)) %>%
    dplyr::filter(.data$TotalChla != 0) %>%
    dplyr::select(.data$ProjectName:.data$SampleDepth_m, .data$TotalChla:.data$Month, -.data$TripCode)  %>%
    tidyr::pivot_longer(.data$TotalChla:.data$TDP, values_to = "Values", names_to = 'parameters') %>%
    pr_get_StationName() %>%
    pr_reorder()
}

