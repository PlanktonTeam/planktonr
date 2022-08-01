#' Get functional group data
#' @param ... to allow use of join when used within another function
#' @param Survey CPR or NRS data
#' @param Type Zooplankton or phytoplankton data
#'
#' @return dataframe for plotting functional group time series info
#' @export
#'
#' @examples
#' NRSfgz <- pr_get_FuncGroups("NRS", "Z")
#' NRSfgp <- pr_get_FuncGroups("NRS", "P")
#' CPRfgz <- pr_get_FuncGroups("CPR", "Z", join = "st_nearest_feature")
#' CPRfgp <- pr_get_FuncGroups("CPR", "P")
pr_get_FuncGroups <- function(Survey = "NRS", Type = "Z", ...){

  if(Survey == "CPR"){
    df <- pr_get_CPRData(Type, Variable = "abundance", Subset = "htg") %>%
      pr_add_Bioregions(...)
  } else if(Survey == "NRS"){
    df <- pr_get_NRSData(Type, Variable = "abundance", Subset = "htg") %>%
      dplyr::filter(.data$StationName != "Port Hacking 4")
  }

  if(Type == "P"){
    var_names <- c("Centric diatom", "Ciliate", "Cyanobacteria", "Dinoflagellate", "Flagellate", "Foraminifera",
                   "Pennate diatom", "Radiozoa", "Silicoflagellate")
  } else if(Type == "Z"){
    var_names <- c("Ascidian", "Ciliate", "Ciliophora", "Coelenterate", "Ctenophore", "Echinoderm", "Euphausiid",
                   "Mysida", "Ostracod", "Phoronid", "Polychaete", "Urochordata", "Amphipod", "Appendicularian",
                   "Brachiopod", "Bryozoan", "Cephalochordate", "Cephalopod", "Chaetognath", "Cirripedia",
                   "Cladoceran", "Cnidarian", "Copepod", "Cumacean", "Decapod", "Egg", "Fish", "Foraminifera",
                   "Hemichordata", "Isopod", "Mollusc", "Nematode", "Nemertean", "Noctilucaceae", "Platyhelminthes",
                   "Radiozoa", "Rotifer", "Sipunculid", "Thaliacean", "Thecostraca")
  }

  if(Survey == "CPR"){
    df <- df %>%
      dplyr::select(.data$BioRegion, .data$SampleTime_Local, .data$Month_Local, .data$Year_Local, tidyselect::any_of(var_names)) %>%
      dplyr::filter(!is.na(.data$BioRegion), !.data$BioRegion %in% c("North", "North-west")) %>%
      droplevels()
  } else if(Survey == "NRS"){
    df <- df %>%
      dplyr::select(.data$StationName, .data$StationCode, .data$SampleTime_Local,
                    .data$Month_Local, .data$Year_Local, tidyselect::any_of(var_names))
  }

  df <- df %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters")  %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    dplyr::filter(.data$Parameters != "Flagellate") %>%
    pr_reorder()

  if(Type == "P"){
    df <- df %>%
      dplyr::mutate(Parameters = ifelse(.data$Parameters %in% c("Ciliate","Foraminifera", "Radiozoa", "Silicoflagellate"), "Other", .data$Parameters),
                    Parameters = factor(.data$Parameters, levels = c("Centric diatom", "Pennate diatom", "Dinoflagellate", "Cyanobacteria",
                                                                     "Other")))
  } else if(Type == "Z"){
    df <- df %>%
      dplyr::mutate(Parameters = ifelse(.data$Parameters %in% c("Copepod", "Appendicularian", "Mollusc", "Cladoceran", "Chaetognath", "Thaliacean"), .data$Parameters, "Other"),
                    Parameters = factor(.data$Parameters, levels = c("Copepod", "Appendicularian", "Mollusc", "Cladoceran", "Chaetognath", "Thaliacean",
                                                                     "Other")))
  }

  df <- df %>%
    dplyr::group_by(dplyr::across(-.data$Values)) %>%
    dplyr::summarise(Values = sum(.data$Values, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(Values = ifelse(.data$Values < 1, 1, .data$Values))

  return(df)
}



#' Get data for frequency map plots
#'
#' @param Type Phytoplankton (P) or Zooplankton (Z), defaults to phyto
#'
#' @return dataframe for plotting with pr_plot_FreqMap
#' @export
#'
#' @examples
#' dfp <- pr_get_FreqMap("P")
#' dfz <- pr_get_FreqMap("Z")
pr_get_FreqMap <- function(Type = "Z"){

  NRSSamp <- pr_get_NRSTrips() %>%
    dplyr::rename(Sample = .data$TripCode) %>%
    dplyr::mutate(Survey = 'NRS') %>%
    dplyr::select(.data$Sample, .data$Survey, .data$SampleTime_Local, .data$Month_Local, .data$Latitude, .data$Longitude)

  CPRSamp <- pr_get_CPRTrips() %>%
    dplyr::mutate(Survey = 'CPR',
                  Latitude = round(Latitude, 4),
                  Longitude = round(Longitude, 4)) %>%
    dplyr::select(.data$Sample, .data$Survey, .data$SampleTime_Local, .data$Month_Local, .data$Latitude, .data$Longitude)

  SampLocs <- dplyr::bind_rows(CPRSamp, NRSSamp) %>%
    dplyr::mutate(DOY = lubridate::yday(.data$SampleTime_Local),
                  Start = as.Date(paste0(min(lubridate::year(.data$SampleTime_Local))-1, "-12-31")),
                  days = difftime(as.Date(.data$SampleTime_Local), .data$Start, units = "days") %>% as.numeric(),
                  thetadoy = (.data$days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
                  Lat = round(.data$Latitude/0.5, 0)*0.5,
                  Long = round(.data$Longitude/0.5, 0)*0.5,
                  Season = dplyr::case_when(.data$Month_Local > 2 & .data$Month_Local < 6 ~ "March - May",
                                            .data$Month_Local > 5 & .data$Month_Local < 9 ~ "June - August",
                                            .data$Month_Local > 8 & .data$Month_Local < 12 ~ "September - November",
                                            TRUE ~ "December - February")) %>%
    dplyr::select(.data$Sample, .data$Survey, .data$Lat, .data$Long, .data$Season)

  if(Type == "P"){
    PhytoCountNRS <- pr_get_NRSData(Type = "phytoplankton", Variable = "abundance", Subset = "species") %>%
      tidyr::pivot_longer(cols = !dplyr::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = "P")),
                          names_to = "Taxon", values_to = "Counts") %>%
      dplyr::rename(Sample = .data$TripCode) %>%
      dplyr::filter(Counts > 0) %>%
      dplyr::mutate(Survey = 'NRS')

    PhytoCountCPR <- pr_get_CPRData(Type = "phytoplankton", Variable = "abundance", Subset = "species") %>%
      tidyr::pivot_longer(cols = !dplyr::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = "P")),
                          names_to = "Taxon", values_to = "Counts") %>%
      dplyr::filter(Counts > 0) %>%
      dplyr::mutate(Survey = 'CPR') %>%
      dplyr::left_join(CPRSamp %>% dplyr::select(-c(Survey, Month_Local)), by = c("Latitude", "Longitude", "SampleTime_Local")) #TODO get rid of this step if product contains sample

    obs <- dplyr::bind_rows(PhytoCountCPR, PhytoCountNRS) %>%
      dplyr::mutate(Counts = as.integer(as.logical(.data$Counts))) %>%
      dplyr::select(.data$Sample, .data$Survey, .data$Taxon, .data$Counts) %>%
      dplyr::arrange(.data$Taxon)

  } else if(Type == "Z"){

    ZooCountNRS <- pr_get_NRSData(Type = "zooplankton", Variable = "abundance", Subset = "species") %>%
      tidyr::pivot_longer(cols = !tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = "Z")),
                          names_to = "Taxon", values_to = "Counts") %>%
      dplyr::rename(Sample = .data$TripCode) %>%
      dplyr::filter(Counts > 0) %>%
      dplyr::mutate(Survey = "NRS")

    ZooCountCPR <- pr_get_CPRData(Type = "zooplankton", Variable = "abundance", Subset = "species") %>%
      tidyr::pivot_longer(cols = !tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = "Z")),
                          names_to = "Taxon", values_to = "Counts") %>%
      dplyr::filter(Counts > 0) %>%
      dplyr::mutate(Survey = "CPR")  %>%
      dplyr::left_join(CPRSamp %>% dplyr::select(-c(Survey, Month_Local)), by = c("Latitude", "Longitude", "SampleTime_Local")) #TODO get rid of this step if product contains sample

    obs <- dplyr::bind_rows(ZooCountCPR, ZooCountNRS) %>%
      dplyr::mutate(Counts = as.integer(as.logical(.data$Counts))) %>%
      dplyr::select(.data$Sample, .data$Survey, .data$Taxon, .data$Counts) %>%
      dplyr::arrange(.data$Taxon)
  }

  mapdata <- obs %>%
    dplyr::select(.data$Sample, .data$Taxon, .data$Counts) %>%
    dplyr::left_join(SampLocs, by="Sample") %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$Season, .data$Survey, .data$Taxon, .data$Lat, .data$Long) %>%
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(SampLocs %>%
                       dplyr::group_by(.data$Lat, .data$Long, .data$Season) %>%
                       dplyr::summarise(samples = dplyr::n(), .groups = "drop"),
                     by = c("Lat", "Long", "Season")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freqsamp = .data$freq/.data$samples,
                  freqfac = as.factor(dplyr::case_when(.data$freqsamp < 0.375 ~ "Seen in 25%",
                                                       .data$freqsamp > 0.875 ~ "100 % of Samples",
                                                       .data$freqsamp > 0.375 & .data$freqsamp < 0.625 ~ "50%",
                                                       TRUE ~ "75%")),
                  Season = factor(.data$Season, levels = c("December - February","March - May","June - August","September - November")),
                  Taxon = as.factor(.data$Taxon)) %>%
    dplyr::select(.data$Season, .data$Survey, .data$Lat, .data$Long, .data$Taxon, .data$freqsamp, .data$freqfac)

  absences <-  SampLocs %>%
    dplyr::distinct(.data$Survey, .data$Lat, .data$Long, .data$Season) %>%
    dplyr::mutate(Taxon = "Taxon",
                  freqsamp = 0,
                  freqfac = as.factor("Absent"))

  freqMapData <- dplyr::bind_rows(mapdata, absences) %>%
    dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%", "50%", "75%", "100 % of Samples"))) %>%
    dplyr::arrange(freqfac)

  return(freqMapData)

}

# Add day/night marker to dataframe
# @param Type Phyto or zoo, defaults to phyto
#
# @return df to be used with pr_plot_DayNight
# @export
#
# @examples
# df <- pr_get_Indices(Survey = "NRS", Type = "Z")
# df <- pr_add_daynight(df)
# pr_add_daynight <- function(df){
#
#   dates <- df %>%
#     dplyr::select(.data$SampleTime_Local, .data$Latitude, .data$Longitude) %>%
#     dplyr::rename(date = .data$SampleTime_Local,
#                   lat = .data$Latitude,
#                   lon = .data$Longitude) %>%
#     dplyr::mutate(date = lubridate::as_date(.data$date))
#
#   daynight_df <- suncalc::getSunlightTimes(data = dates, #TODO quicker to change to Local now that it exists.
#                                            keep = c("sunrise", "sunset"),
#                                            tz = lutz::tz_lookup_coords(dates$lat, dates$lon, method = "fast", warn = FALSE)) %>% # TODO remove this when we can get tz into all dataframes
#     dplyr::bind_cols(df["SampleTime_Local"]) %>%
#     dplyr::mutate(daynight = dplyr::if_else(.data$SampleTime_Local > .data$sunrise &
#                                       .data$SampleTime_Local < .data$sunset, "Day", "Night"))
#
#   df <- df %>%
#     dplyr::bind_cols(daynight_df["daynight"])
#
# }

#' Get data for plots of species abundance by day and night using CPR data
#' @param Type "P" or "Z" (default)
#'
#' @return df to be used with pr_plot_DayNight
#' @export
#'
#' @examples
#' df <- pr_get_DayNight(Type = "Z")
pr_get_DayNight <- function(Type = "Z"){

  if(Type == "Z"){
    dat <- pr_get_CPRData(Type = "Z", Variable = "abundance", Subset = "copepods")

  } else if (Type == "P"){
    dat <- pr_get_CPRData(Type = "P", Variable = "abundance", Subset = "species")

  }

  dates <- dat %>%
    dplyr::select(.data$SampleTime_UTC, .data$Latitude, .data$Longitude) %>%
    dplyr::rename(date = .data$SampleTime_UTC,
                  lat = .data$Latitude,
                  lon = .data$Longitude) %>%
    dplyr::mutate(date = lubridate::as_date(.data$date))

  daynight_df <- suncalc::getSunlightTimes(data = dates, #TODO quicker to change to Local now that it exists.
                                           keep = c("sunrise", "sunset"),
                                           tz = "UTC") %>%
    dplyr::bind_cols(dat["SampleTime_UTC"]) %>%
    dplyr::mutate(daynight = ifelse(.data$SampleTime_UTC > .data$sunrise & .data$SampleTime_UTC < .data$sunset, "Day", "Night"))

  dat <- dat %>%
    dplyr::bind_cols(daynight_df["daynight"]) %>%
    dplyr::select(tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)), tidyselect::everything()) %>%
    tidyr::pivot_longer(-tidyselect::any_of(c(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type), "daynight")), values_to = "Species_m3", names_to = "Species") %>%
    dplyr::group_by(.data$Month_Local, .data$daynight, .data$Species) %>%
    dplyr::summarise(Species_m3 = mean(.data$Species_m3, na.rm = TRUE),
                     .groups = "drop")

}




#' Get data for STI plots of species abundance
#' @param Type Phyto or zoo, defaults to phyto
#'
#' @return df to be sued with pr_plot_STI
#' @export
#'
#' @examples
#' df <- pr_get_STI("P")
#' df <- pr_get_STI("Z")
pr_get_STI <-  function(Type = "P"){

  if(Type == "Z"){
    cprdat <- pr_get_CPRData(Type, Variable = "abundance", Subset = "copepods")

    nrsdat <- pr_get_NRSData(Type, Variable = "abundance", Subset = "copepods")
    parameter <- "CopeAbundance_m3"

  } else if(Type == "P"){
    cprdat <- pr_get_CPRData(Type, Variable = "abundance", Subset = "species")

    nrsdat <- pr_get_NRSData(Type, Variable = "abundance", Subset = "species")
    parameter <- "PhytoAbundance_m3"
  }

  ## These will be replace with proper satellite data from extractions in time
  nrssat <- readr::read_csv(system.file("extdata", "NRS_SatData.csv", package = "planktonr", mustWork = TRUE),
                            show_col_types = FALSE,
                            na = c("NA", "")) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_Local = .data$SAMPLEDATE_LOCAL)

  cprsat <- readr::read_csv(system.file("extdata", "CPR_SatData.csv", package = "planktonr", mustWork = TRUE),
                            show_col_types = FALSE) %>%
    pr_rename() %>%
    dplyr::rename(SampleTime_UTC = .data$SAMPLEDATE_UTC)

  cpr <- cprdat %>%
    tidyr::pivot_longer(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)), names_to = "Species", values_to = parameter) %>%
    dplyr::left_join(cprsat, by = c("Latitude", "Longitude", "SampleTime_UTC")) %>%
    dplyr::select(.data$Species, .data$SST, .data[[parameter]]) %>%
    dplyr::filter(!is.na(.data$SST) & .data[[parameter]] > 0) %>%
    dplyr::mutate(Project = "cpr",
                  Species_m3 = .data[[parameter]] + min(.data[[parameter]][.data[[parameter]]>0], na.rm = TRUE))

  nrs <- nrsdat %>%
    tidyr::pivot_longer(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = Type)), names_to = "Species", values_to = parameter) %>%
    dplyr::left_join(nrssat, by = c("Latitude", "Longitude", "SampleTime_Local")) %>%
    dplyr::select(.data$Species, .data$SST, .data[[parameter]]) %>%
    dplyr::filter(!is.na(.data$SST) & .data[[parameter]] > 0) %>%
    dplyr::mutate(Project = "nrs",
                  Species_m3 = .data[[parameter]] + min(.data[[parameter]][.data[[parameter]]>0], na.rm = TRUE))

  comball <- cpr %>%
    dplyr::bind_rows(nrs) %>%
    dplyr::mutate(SST = round(.data$SST/0.5) * 0.5) %>%
    dplyr::arrange(.data$Species)
}


#' Get the summary plankton observations
#'
#' Get the summary plankton observations from the NRS and CPR.
#' @return a dataframe with a species summary
#'
#' @param Type The group of plankton requested. Either "Z" or "P"
#'
#' @export
#'
#' @examples
#' df <- pr_get_SppCount("P")
#' df <- pr_get_SppCount("Z")
pr_get_SppCount <- function(Type = "Z"){

  if (Type == "P"){
    gp <- "Phytoplankton"
  } else if (Type == "Z"){
    gp <- "Zooplankton"
  }

out <- sppSummary %>%
  dplyr::filter(.data$Group == gp)

}


#' Random facts about plankton
#'
#' This function randomly returns a fun fact about plankton.
#'
#' @return A random plankton fact as a string
#' @export
#'
#' @examples
#' pr_get_Facts()
pr_get_Facts <- function(){

  facts <- list("Zooplankton are not only a major food source for commercial and invertebrate fisheries, but some groups are harvest directly for human consumption: 1.2 million tonnes of jellyfish are harvested each year in China, and about 400,000 tonnes of krill are caught annually in the Southern Ocean and off Japan, for human consumption and for fish meal (CCAMLR 2021).",
                "Omega-3 fatty acids support human neurological function, cardiovascular health, and immune response (Calder 2015). As zooplankton have high levels of Omega-3 fatty acids, 1000 tonnes of the copepod _Calanus finmarchicus_ are harvested by Norway annually for use in human supplements.",
                "Almost all wild-caught fish (80 million tonnes per year) and crustaceans (prawns, shrimps, lobsters and crabs: 3.4 million tonnes) have larval stages that live as plankton drifting in the water column.",
                "Plankton such as flagellates, diatoms, copepods, krill and mysids are used extensively in aquaculture production for rearing fish larvae and juveniles, and for feeding shellfish (Richardson et al. 2019)",
                "Some phytoplankton species produce toxins that cause skin and eye irritation, digestive upsets, breakdown of liver cells, attack the nervous system, and can even cause death. The major pathway to humans is when we eat shellfish that have filtered toxic phytoplankton from the water.",
                "Zooplankton with chitinous exoskeletons, particularly copepods, are hosts for bacterial pathogens such as Vibrio cholerae, which is responsible for ~5 million cases and 120 000 deaths per year.",
                "Zooplankton, phytoplankton, bacterioplankton and marine viruses are used in bioprospecting and other commercial products.",
                "Copepods and larvae of crabs, prawns and mysids are used in studies assessing acute and sublethal pollutant impacts because they are extremely sensitive to toxins, can be mass cultured, have a short life cycle, and have distinct stages that provides endpoints to determine toxicity of contaminants (Van Dam et al. 2008).",
                "Plankton are commonly used as ecological indicators in report cards and ecosystem assessments to assess the health of marine systems. These plankton indicators have been developed to assess human impacts on marine systems: climate change, ocean acidification and heatwaves; eutrophication; overfishing; and species invasions (Richardson et al. 2020).",
                "Biomimetics is the field of imitation of natural systems to solve human problems. For example, the elegant and diverse body forms of diatoms, coccolithophores, silicoflagellates, tintinnids, radiolarians, foraminiferans and acantharians are inspiring novel building designs by architects and engineers (Pohl & Nachtigall 2015).",
                "Biomimetics is the field of imitation of natural systems to solve human problems. For example, scientists are studying how plankton produce composite materials, in different orientations, to make their structure strong and lightweight (Pohl & Nachtigall 2015).",
                "The biological pump, controlled by phytoplankton and zooplankton, fixes vast amounts of CO2 via photosynthesis, which is ultimately removed from surface to deeper waters by sinking and active transport. How the biological pump will be stimulated or impeded with climate change is an area of active research.",
                "Harpacticoid copepods are valuable in marine aquaria because they clean the substrate and aquarium panels, and their nauplii and copepodites provide food for invertebrates such as corals, clams and sea cucumbers.",
                "As phytoplankton and zooplankton are key to processing and cycling nutrients in the marine food web, data on their abundance and type are used to assess the eReefs biogeochemical model for managing water quality on the Great Barrier Reef (Robson et al. 2020; Skerratt et al. 2018).")

  # Krill – biology, ecology and fishing. Commission for the Conservation of Antarctic Marine Living Resources. Retrieved 17 October 2021. https://www.ccamlr.org/en/fisheries/krill-%E2%80%93-biology-ecology-and-fishing
  # Calder PC (2015) Functional roles of fatty acids and their effects on human health. Journal of Parenteral and Enteral Nutrition, 39, 18S–32S.
  # Richardson AJ, Uribe-Palomino J, Slotwinski A, Coman F, Miskiewicz AG, Rothlisberg PC, Young JW, Suthers IM (2019) Chapter 8. Coastal and marine zooplankton: identification, biology and ecology. In Plankton: A Guide to Their Ecology and Monitoring for Water Quality. Edited by Suthers I, Rissik D, Richardson AJ. 2nd edition. CSIRO Publishing. pp. 141-208
  # Vezzulli L, Grande C, Reid PC, Hélaouët P, Edwards M, Höfle MG, et al. (2016) Climate influence on Vibrio and associated human diseases during the past half-century in the coastal North Atlantic. Proceedings of the National Academy of Sciences of the United States of America 113, E5062–E5071. doi:10.1073/pnas.1609157113
  # Van Dam RA, Harford AJ, Houston MA, Hogan AC, Negri AP (2008) Tropical marine toxicity testing in Australia: a review and recommendations. Australasian Journal of Ecotoxicology 14: 55–88.
  # Richardson AJ, Eriksen R, Moltmann T, Hodgson-Johnston I, Wallis JR (2020) State and Trends of Australia’s Ocean Report, Integrated Marine Observing System, Hobart. 164 pp. https://www.imosoceanreport.org.au/
  # Pohl G, Nachtigall W (2015) Biomimetics for Architecture and Design. Nature – Analogies – Technology. 1st edn. Springer, Heidelberg, Germany.
  # Robson BJ, Skerratt J, Baird ME, Davies C, Herzfeld M, Jones EM, Mongin M, Richardson AJ, Rizwi F, Wild-Allen K, Steven A (2020) Enhanced assessment of the eReefs marine models for the Great Barrier Reef using a four-level model evaluation framework. Environmental Modelling and Software 129: 104707. 15 pp.
  # Skerratt JH, Mongin M, Wild-Allen KA, Baird ME, Robson BJ, Schaffelke B, Soja-Wozniak M, Margvelashvili N, Davies CH, Richardson AJ, Steven ADL (2019) Simulated nutrient and plankton dynamics in the Great Barrier Reef (2011-2016). Journal of Marine Systems 192: 51-74

  r <- round(stats::runif(1, min = 1, max = length(facts)))

  out <- facts[[r]]

  return(out)

}




#' Random scientific papers using IMOS data
#'
#' This function randomly returns a publication that uses the IMOS plankton data
#'
#' @return A random publication reference as a string
#' @export
#'
#' @examples
#' pr_get_Papers()
pr_get_Papers <- function(){

  papers <- list(
    "Campbell MD, Schoeman DS, Venables W, Abu-Alhaija R, Batten SD, Chiba S, et al. Testing Bergmann\'s rule in marine copepods. Ecography. 2021;n/a(n/a). doi: https://doi.org/10.1111/ecog.05545.",
    "Ajani P, Davies C, Eriksen R, Richardson A. Global Warming Impacts Micro-Phytoplankton at a Long-Term Pacific Ocean Coastal Station. Frontiers in Marine Science. 2020;7. doi:  https:// doi.org/10.3389/fmars.2020.576011 . PubMed PMID: WOS:000580605700001.",
    "Hallegraeff G, Eriksen R, Davies C, Slotwinski A, McEnnulty F, Coman F, et al. The marine planktonic dinoflagellate Tripos: 60 years of species-level distributions in Australian waters. Australian Systematic Botany. 2020;33(4):392-411. doi: https:// doi.org/10.1071/SB19043. PubMed PMID: WOS:000548434100004.",
    "Landry MR, Hood RR, Davies CH. Mesozooplankton biomass and temperature-enhanced grazing along a 110\U00B0E transect in the eastern Indian Ocean. Marine Ecology Progress Series. 2020;649:1-19. DOI: https://doi.org/10.3354/meps13444",
    "McCosker E, Davies C, Beckley L. Oceanographic influence on coastal zooplankton assemblages at three IMOS National Reference Stations in Western Australia. Marine and Freshwater Research. 2020;71(12):1672-85. doi: https:// doi.org/10.1071/MF19397. PubMed PMID: WOS:000556426000001.",
    "McEnnulty F, Davies C, Armstrong A, Atkins N, Coman F, Clementson L, et al. A database of zooplankton biomass in Australian marine waters. Scientific Data. 2020;7(1). doi: https://doi.org/10.1038/s41597-020-00625-9. PubMed PMID: WOS:000571812600010.",
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

  return(out)

}

#' Data for IMOS progress map
#'
#' @param Survey one of NRS, CPR, Both
#'
#' @return A dataframe for input into pr_plot_Progress()
#' @export
#'
#' @examples
#' df <- pr_get_ProgressMap(c("NRS", "CPR"))

pr_get_ProgressMap <- function(Survey = c("NRS", "CPR")){

  if("NRS" %in% Survey) {
    PMapDataNRS <- planktonr::pr_get_NRSTrips(Type = c("P", "Z")) %>%
      dplyr::select(.data$StationCode, .data$Longitude, .data$Latitude) %>%
      dplyr::rename(Region = .data$StationCode) %>%
      dplyr::mutate(Survey = "NRS") %>%
      dplyr::filter(.data$Region != "PH4")
    if (("CPR" %in% Survey) == FALSE){ # Return data if no CPR
      return(PMapDataNRS)
    }
  }

  if ("CPR" %in% Survey) {
    # PMapDataCPR <- readr::read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/deprecated/CPR_Samp.csv", show_col_types = FALSE)  %>%
    PMapDataCPR <- pr_get_s3("cpr_samp") %>%
      dplyr::select(.data$REGION, .data$LONGITUDE, .data$LATITUDE) %>%
      pr_rename() %>%
      dplyr::mutate(Survey = "CPR")
    if (("NRS" %in% Survey) == FALSE){ # Return data if no NRS
      return(PMapDataCPR)
    }
  }

  if(("NRS" %in% Survey & "CPR" %in% Survey) | "Both" %in% Survey) {
    PMapData <- dplyr::bind_rows(PMapDataNRS, PMapDataCPR)
    return(PMapData)
  }

}
