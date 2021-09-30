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


#' Summarise the plankton observations
#'
#' Summarise the plankton observations from the NRS and CPR.
#' @return a dataframe with a species summary
#' @export
#'
#' @examples
#' df <- pr_export_SppCount()
pr_export_SppCount <- function(){

  # First do Phytoplankton
  nrsP <- pr_get_NRSPhytoData() %>%
    mutate(TaxonName = stringr::str_c(.data$Genus, " ", .data$Species)) %>%  # Overwrite Taxon Name to remove the fluff
    select(.data$TaxonName, .data$SPCode, .data$CELL_COUNT) %>%
    rename(TaxonCount = .data$CELL_COUNT) %>%
    tidyr::drop_na()

  cprP <- pr_get_CPRPhytoData("Count") %>%
    mutate(TaxonName = stringr::str_c(.data$Genus, " ", .data$Species)) %>%  # Overwrite Taxon Name to remove the fluff
    rename(TaxonCount = .data$FovCount) %>%
    select(.data$TaxonName, .data$SPCode, .data$TaxonCount) %>%
    tidyr::drop_na()

  outP <- bind_rows(nrsP, cprP) %>%
    group_by(.data$TaxonName, .data$SPCode) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(.data$n)) %>%
    dplyr::filter(stringr::str_detect(.data$TaxonName, 'spp', negate = TRUE)) %>%
    mutate(Group = "Phytoplankton")

  # Now do Zooplankton
  nrsZ <- pr_get_NRSZooData() %>%
    mutate(TaxonName = stringr::str_c(.data$Genus, " ", .data$Species)) %>%  # Overwrite Taxon Name to remove f/m/j etc
    select(.data$TaxonName, .data$SPCode, .data$TaxonCount) %>%
    tidyr::drop_na()

  cprZ <- pr_get_CPRZooData("Count") %>%
    mutate(TaxonName = stringr::str_c(.data$Genus, " ", .data$Species)) %>%  # Overwrite Taxon Name to remove f/m/j etc
    select(.data$TaxonName, .data$SPCode, .data$TaxonCount) %>%
    tidyr::drop_na()

  outZ <- bind_rows(nrsZ, cprZ) %>%
    group_by(.data$TaxonName, .data$SPCode) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(.data$n)) %>%
    dplyr::filter(stringr::str_detect(.data$TaxonName, 'spp', negate = TRUE)) %>%
    mutate(Group = "Zooplankton")

  # Now combine them
  out <- bind_rows(outP, outZ)
  return(out)

}


#' Get the summary plankton observations
#'
#' Get the summary plankton observations from the NRS and CPR.
#' @return a dataframe with a species summary
#'
#' @param gp The group of plankton requested. Either "Zooplankton" or "Phytoplankton"
#'
#' @export
#'
#' @examples
#' df <- pr_get_SppCount("Zooplankton")
pr_get_SppCount <- function(gp){

  out <- sppSummary %>%
    filter(.data$Group == gp)

}


#' Random facts about plankton
#'
#' This function randomly returns a fun fact about plankton.
#'
#' @return
#' @export
#'
#' @examples
#' pr_get_facts()
pr_get_facts <- function(){

  facts <- list("Phytoplankton generate > 50 % of the worlds oxygen (reference)",
                "Phytoplankton generate > 50 % of the worlds oxygen (reference)",
                "Phytoplankton generate > 50 % of the worlds oxygen (reference)")

  r <- round(stats::runif(1, min = 1, max = length(facts)))

  out <- facts[[r]]

}




#' Random scientific papers using IMOS data
#'
#' This function randomly returns a publication that uses the IMOS plankton data
#'
#' @return
#' @export
#'
#' @examples
#' pr_get_papers()
pr_get_papers <- function(){

  papers <- list(
    "Campbell MD, Schoeman DS, Venables W, Abu-Alhaija R, Batten SD, Chiba S, et al. Testing Bergmann\'s rule in marine copepods. Ecography. 2021;n/a(n/a). doi: https://doi.org/10.1111/ecog.05545.",
    "Ajani P, Davies C, Eriksen R, Richardson A. Global Warming Impacts Micro-Phytoplankton at a Long-Term Pacific Ocean Coastal Station. Frontiers in Marine Science. 2020;7. doi:  https:// doi.org/10.3389/fmars.2020.576011 . PubMed PMID: WOS:000580605700001.",
    "Hallegraeff G, Eriksen R, Davies C, Slotwinski A, McEnnulty F, Coman F, et al. The marine planktonic dinoflagellate Tripos: 60 years of species-level distributions in Australian waters. Australian Systematic Botany. 2020;33(4):392-411. doi: https:// doi.org/10.1071/SB19043. PubMed PMID: WOS:000548434100004.",
    "Landry MR, Hood RR, Davies CH. Mesozooplankton biomass and temperature-enhanced grazing along a 110°E transect in the eastern Indian Ocean. Marine Ecology Progress Series. 2020;649:1-19. DOI: https://doi.org/10.3354/meps13444",
    "McCosker E, Davies C, Beckley L. Oceanographic influence on coastal zooplankton assemblages at three IMOS National Reference Stations in Western Australia. Marine and Freshwater Research. 2020;71(12):1672-85. doi: https:// doi.org/10.1071/MF19397. PubMed PMID: WOS:000556426000001.",
    "McEnnulty F, Davies C, Armstrong A, Atkins N, Coman F, Clementson L, et al. A database of zooplankton biomass in Australian marine waters. Scientific Data. 2020;7(1). doi: https:// doi.org/10.1038/s41597-020-00625-9. PubMed PMID: WOS:000571812600010.",
    "Robson B, Skerratt J, Baird M, Davies C, Herzfeld M, Jones E, et al. Enhanced assessment of the eReefs biogeochemical model for the Great Barrier Reef using the Concept/State/Process/System model evaluation framework. Environmental Modelling & Software. 2020;129. doi: https:// doi.org/10.1016/j.envsoft.2020.104707. PubMed PMID: WOS:000540077900007.",
    "Bailey K, Steinberg C, Davies C, Galibert G, Hidas M, McManus M, et al. Coastal Mooring Observing Networks and Their Data Products: Recommendations for the Next Decade. Frontiers in Marine Science. 2019;6. doi: https:// doi.org/10.3389/fmars.2019.00180. PubMed PMID: WOS:000464606500001.",
    "Berry T, Saunders B, Coghlan M, Stat M, Jarman S, Richardson A, et al. Marine environmental DNA biomonitoring reveals seasonal patterns in biodiversity and identifies ecosystem responses to anomalous climatic events. Plos Genetics. 2019;15(2). doi: https://doi.org/10.1371/journal.pgen.1007943. PubMed PMID: WOS:000459970100030.",
    "Eriksen R, Davies C, Bonham P, Coman F, Edgar S, McEnnulty F, et al. Australia\'s Long-Term Plankton Observations: The Integrated Marine Observing System National Reference Station Network. Frontiers in Marine Science. 2019;6. doi: https:// doi.org/10.3389/fmars.2019.00161. PubMed PMID: WOS:000465444800001.",
    "Skerratt J, Mongin M, Baird M, Wild-Allen K, Robson B, Schaffelke B, et al. Simulated nutrient and plankton dynamics in the Great Barrier Reef (2011-2016). Journal of Marine Systems. 2019;192:51-74. doi: https:// doi.org/10.1016/j.jmarsys.2018.12.006. PubMed PMID: WOS:000459523000005.",
    "Brown MV, van de Kamp J, Ostrowski M, Seymour JR, Ingleton T, Messer LF, et al. Systematic, continental scale temporal monitoring of marine pelagic microbiota by the Australian Marine Microbial Biodiversity Initiative. Scientific Data. 2018;5(1):180130. doi: https:// doi.org/10.1038/sdata.2018.130.",
    "Davies C, Ajani P, Armbrecht L, Atkins N, Baird M, Beard J, et al. A database of chlorophyll a in Australian waters. Scientific Data. 2018;5. doi: https:// doi.org/10.1038/sdata.2018.18. PubMed PMID: WOS:000425502700003.",
    "Dornelas M, Antao L, Moyes F, Bates A, Magurran A, Adam D, et al. BioTIME: A database of biodiversity time series for the Anthropocene. Global Ecology and Biogeography. 2018;27(7):760-86. doi: https:// doi.org/10.1111/geb.12729. PubMed PMID: WOS:000439785700001.",
    "Everett J, Baird M, Buchanan P, Bulman C, Davies C, Downie R, et al. Modeling What We Sample and Sampling What We Model: Challenges for Zooplankton Model Assessment. Frontiers in Marine Science. 2017;4. doi: https:// doi.org/10.3389/fmars.2017.00077. PubMed PMID: WOS:000457690600077.",
    "Kelly P, Clementson L, Davies C, Corney S, Swadling K. Zooplankton responses to increasing sea surface temperatures in the southeastern Australia global marine hotspot. Estuarine Coastal and Shelf Science. 2016;180:242-57. doi: https:// doi.org/10.1016/j.ecss.2016.07.019. PubMed PMID: WOS:000384866900024.",
    "Davies CH, Armstrong AJ, Baird M, Coman F, Edgar S, Gaughan D, et al. Over 75 years of zooplankton data from Australia. Ecology. 2014;95(11):3229-. doi: https:// doi.org/10.1890/14-0697.1.",
    "Hallegraeff G, Coman F, Davies C, Hayashi A, McLeod D, Slotwinski A, et al. Australian Dust Storm Associated with Extensive Aspergillus sydowii Fungal Bloom in Coastal Waters. Applied and Environmental Microbiology. 2014;80(11):3315-20. doi: https:// doi.org/10.1128/AEM.04118-13. PubMed PMID: WOS:000336035200004."
  )

  r <- round(stats::runif(1, min = 1, max = length(papers)))

  out <- papers[[r]]

}
