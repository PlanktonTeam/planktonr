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
#' CPRfgz <- pr_get_FuncGroups("CPR", "Z", near_dist_km = 250)
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
      dplyr::select("BioRegion", "SampleTime_Local", "Month_Local", "Year_Local", tidyselect::any_of(var_names)) %>%
      dplyr::filter(!is.na(.data$BioRegion), !.data$BioRegion %in% c("North", "North-west")) %>%
      droplevels()
  } else if(Survey == "NRS"){
    df <- df %>%
      dplyr::select("StationName", "StationCode", "SampleTime_Local",
                    "Month_Local", "Year_Local", tidyselect::any_of(var_names))
  }

  df <- df %>%
    tidyr::pivot_longer(tidyselect::any_of(var_names), values_to = "Values", names_to = "Parameters")  %>%
    dplyr::mutate(Values = .data$Values + min(.data$Values[.data$Values>0], na.rm = TRUE)) %>%
    dplyr::filter(.data$Parameters != "Flagellate") %>%
    pr_reorder()

  if(Type == "P"){
    df <- df %>%
      dplyr::mutate(Parameters = ifelse(.data$Parameters %in% c("Centric diatom", "Pennate diatom", "Dinoflagellate", "Cyanobacteria"), .data$Parameters, "Other"),
                    Parameters = factor(.data$Parameters, levels = c("Centric diatom", "Pennate diatom", "Dinoflagellate", "Cyanobacteria",
                                                                     "Other")))
  } else if(Type == "Z"){
    df <- df %>%
      dplyr::mutate(Parameters = ifelse(.data$Parameters %in% c("Copepod", "Appendicularian", "Mollusc", "Cladoceran", "Chaetognath", "Thaliacean"), .data$Parameters, "Other"),
                    Parameters = factor(.data$Parameters, levels = c("Copepod", "Appendicularian", "Mollusc", "Cladoceran", "Chaetognath", "Thaliacean",
                                                                     "Other")))
  }

  df <- df %>%
    dplyr::group_by(dplyr::across(-"Values")) %>%
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
#' df <- pr_get_FreqMap("Z")

pr_get_FreqMap <- function(Type = "Z"){

  NRS <- pr_get_NRSData(Type = Type, Variable = "abundance", Subset = "species") %>%
    tidyr::pivot_longer(cols = !dplyr::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = Type)),
                        names_to = "Species", values_to = "Counts")

  CountNRS <- NRS %>%
    dplyr::rename(Sample = "TripCode") %>%
    dplyr::mutate(Survey = 'NRS') %>%
    dplyr::select("Sample", "Survey", "Species", "Counts", "SampleTime_Local", "Month_Local", "Latitude", "Longitude")

  CPR <- pr_get_CPRData(Type = Type, Variable = "abundance", Subset = "species") %>%
    tidyr::pivot_longer(cols = !dplyr::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)),
                        names_to = "Species", values_to = "Counts")

  CountCPR <- CPR %>%
    dplyr::rename(Sample = "Sample_ID") %>%
    dplyr::mutate(Survey = 'CPR') %>%
    dplyr::select("Sample", "Survey", "Species", "Counts", "SampleTime_Local", "Month_Local", "Latitude", "Longitude")

  dat <- dplyr::bind_rows(CountCPR, CountNRS)  %>%
    dplyr::mutate(Latitude = round(.data$Latitude/0.5, 0)*0.5,
                  Longitude = round(.data$Longitude/0.5, 0)*0.5,
                  Season = dplyr::case_when(.data$Month_Local > 2 & .data$Month_Local < 6 ~ "March - May",
                                            .data$Month_Local > 5 & .data$Month_Local < 9 ~ "June - August",
                                            .data$Month_Local > 8 & .data$Month_Local < 12 ~ "September - November",
                                            TRUE ~ "December - February"))

  totals <- dat %>% dplyr::select(-c('Species', 'Counts')) %>% # All samples including where nothing is counted
    dplyr::distinct() %>%
    dplyr::group_by(.data$Season, .data$Survey, .data$Latitude, .data$Longitude) %>%
    dplyr::summarise(samples = dplyr::n(), .groups = "drop")

  obs <- dat %>%  # Samples where something is counted
    dplyr::filter(.data$Counts > 0) %>%
    dplyr::group_by(.data$Season, .data$Survey, .data$Species, .data$Latitude, .data$Longitude) %>%
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(totals, by = c('Season', 'Survey', 'Latitude', 'Longitude')) %>%
    dplyr::mutate(freqsamp = .data$freq/.data$samples,
                  freqfac = as.factor(dplyr::case_when(.data$freqsamp < 0.375 ~ "Seen in 25%",
                                                       .data$freqsamp > 0.875 ~ "100% of Samples",
                                                       .data$freqsamp > 0.375 & .data$freqsamp < 0.625 ~ "50%",
                                                       TRUE ~ "75%")))

  # Adding empty samples back in for absences
  mapData <-  totals %>% dplyr::left_join(obs, by = c('Season', 'Survey', 'Latitude', 'Longitude', 'samples')) %>%
    dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Seen in 25%",'50%', '75%', '100% of Samples'))) %>%
    dplyr::arrange(.data$Species)

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
#     dplyr::select("SampleTime_Local", "Latitude", "Longitude") %>%
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
    dplyr::select("SampleTime_UTC", "Latitude", "Longitude") %>%
    dplyr::rename(date = "SampleTime_UTC",
                  lat = "Latitude",
                  lon = "Longitude") %>%
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

#' Get data for satellite data
#' @param Survey either NRS or CPR
#'
#' @return df with either NRS or CPR satellite data
#' @export
#'
#' @examples
#' df <- pr_get_SatData("NRS")
## These will be replace with proper satellite data from extractions in time

pr_get_SatData <- function(Survey = 'NRS'){

  if(Survey == "NRS"){
    NRS_SatData <- readr::read_csv(system.file("extdata", "NRS_SatData.csv", package = "planktonr", mustWork = TRUE),
                                   show_col_types = FALSE,
                                   na = c("NA", "")) %>%
      pr_rename()
  } else {
    CPR_SatData <- readr::read_csv(system.file("extdata", "CPR_SatData.csv", package = "planktonr", mustWork = TRUE),
                                   show_col_types = FALSE) %>%
      pr_rename()
  }
}


#' Get data for STI plots of species abundance
#' @param Type Phyto or zoo, defaults to phyto
#'
#' @return df to be sued with pr_plot_STI
#' @export
#'
#' @examples
#' df <- pr_get_STIdata("P")
#' df <- pr_get_STIdata("Z")
pr_get_STIdata <-  function(Type = "P"){

  if(Type == "Z"){
    cprdat <- pr_get_CPRData(Type, Variable = "abundance", Subset = "copepods")

    nrsdat <- pr_get_NRSData(Type, Variable = "abundance", Subset = "copepods")
    parameter <- "CopeAbundance_m3"

  } else if(Type == "P"){
    cprdat <- pr_get_CPRData(Type, Variable = "abundance", Subset = "species")

    nrsdat <- pr_get_NRSData(Type, Variable = "abundance", Subset = "species")
    parameter <- "PhytoAbundance_m3"
  }

  cprsat <- pr_get_SatData("CPR")
  nrssat <- pr_get_SatData("NRS")

  cpr <- cprdat %>%
    tidyr::pivot_longer(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "CPR", Type = Type)), names_to = "Species", values_to = parameter) %>%
    dplyr::left_join(cprsat, by = c("Latitude", "Longitude", "SampleTime_UTC")) %>%
    dplyr::select("Species", "SST", tidyselect::all_of(parameter)) %>%
    dplyr::filter(!is.na(.data$SST) & .data[[parameter]] > 0) %>%
    dplyr::mutate(Project = "cpr",
                  Species_m3 = .data[[parameter]] + min(.data[[parameter]][.data[[parameter]]>0], na.rm = TRUE))

  nrs <- nrsdat %>%
    tidyr::pivot_longer(-tidyselect::all_of(pr_get_NonTaxaColumns(Survey = "NRS", Type = Type)), names_to = "Species", values_to = parameter) %>%
    dplyr::left_join(nrssat, by = c("Latitude", "Longitude", "SampleTime_Local")) %>%
    dplyr::select("Species", "SST", tidyselect::all_of(parameter)) %>%
    dplyr::filter(!is.na(.data$SST) & .data[[parameter]] > 0) %>%
    dplyr::mutate(Project = "nrs",
                  Species_m3 = .data[[parameter]] + min(.data[[parameter]][.data[[parameter]]>0], na.rm = TRUE))

  comball <- cpr %>%
    dplyr::bind_rows(nrs) %>%
    dplyr::mutate(SST = round(.data$SST/0.5) * 0.5) %>%
    dplyr::arrange(.data$Species)
}

#' Get STI kernel density for each species
#'
#' @param Type Zooplankton (Copepods) or phytoplankton species
#'
#' @return df of STI kernel density for each species
#' @export
#'
#' @examples
#' df <- pr_get_STI('P')
#'
pr_get_STI <-  function(Type = "Z"){
  df <- pr_get_STIdata(Type)

  species <- unique(df$Species)

  calc_sti <-  function(species){
    means <- df %>%
      dplyr::group_by(.data$Project) %>%
      dplyr::summarise(mean = mean(.data$Species_m3, na.rm = TRUE))

    #means are so different so log data as the abundance scale is so wide

    sti <- df %>%
      dplyr::filter(.data$Species == species) %>%
      dplyr::left_join(means, by = "Project") %>%
      dplyr::mutate(relab = .data$Species_m3/.data$mean) %>%
      dplyr::group_by(.data$SST, .data$Species) %>%
      dplyr::summarize(relab = sum(.data$relab),
                       freq = dplyr::n(),
                       a = sum(.data$relab)/dplyr::n(),
                       .groups = "drop")

    n <- length(sti$SST)
    # have a stop if n < 10

    ## STI via kernel density

    kernStep <- 0.001
    kernMin <- 0
    kernMax <- 32
    kernN <- round((kernMax - kernMin) / kernStep + 1)
    kernTemps <- seq(kernMin, kernMax, length.out=kernN)
    kernBw <- 2

    kern_yp <- matrix(0, nrow = kernN, ncol = 1)
    kypout <- matrix(0, nrow = kernN, ncol = 1)

    taxon <- unique(sti$Species)
    sti$Species <- factor(sti$Species)
    sti$weight <- with(sti, abs(relab) / sum(relab))
    kernOut <- with(sti,
                    density(SST, weight=weight,
                            bw=kernBw,
                            from=kernMin,
                            to=kernMax,
                            n=kernN))

    z <- data.frame(kernTemps, y = kernOut$y)
    STI <- round(z[which.max(z[,2]),]$kernTemps,1)
  }

  STI <- purrr::map(species, calc_sti)
  STI  <- data.frame(Species = species, STI = matrix(unlist(STI), nrow = length(STI), byrow = TRUE))

}

#' Get CTI for sample
#'
#' @param Type Zooplankton (Copepods) or phytoplankton species
#'
#' @return df of CTI for each sample
#' @export
#'
#' @examples
#' df <- pr_get_CTI("P")
#'
pr_get_CTI <-  function(Type = 'Z'){

  df <- pr_get_STI(Type)

  if(Type == 'Z'){
    dat <- pr_get_NRSData("zooplankton", "abundance", "species")
    vars <- pr_get_NonTaxaColumns(Survey = "NRS", Type)
  } else {
    dat <- pr_get_NRSData("phytoplankton", "abundance", "species")
    vars <- pr_get_NonTaxaColumns(Survey = "NRS", Type)
  }

  dat <- dat %>%
    dplyr::mutate(StationCode = ifelse(.data$StationCode == 'PH4', 'PHB', .data$StationCode)) %>%
    dplyr::select(-(tidyselect::any_of(vars)), "SampleTime_Local", "StationCode", "Project") %>%
    tidyr::pivot_longer(-c("SampleTime_Local", "StationCode", "Project"), values_to = "Abundance", names_to = "Species") %>%
    dplyr::inner_join(df, by = 'Species') %>%
    dplyr::group_by(.data$SampleTime_Local, .data$StationCode, .data$Project) %>%
    dplyr::summarise(CTI = sum(.data$Abundance * .data$STI, na.rm = TRUE)/sum(.data$Abundance, na.rm = TRUE),
                     .groups = "drop") %>%
    as.data.frame()

  return(dat)
}

# Get the summary plankton observations
#
# Get the summary plankton observations from the NRS and CPR.
# @return a dataframe with a species summary
#
# @param Type The group of plankton requested. Either "Z" or "P"
#
# @export
#
# @examples
# df <- pr_get_SppCount("P")
# df <- pr_get_SppCount("Z")
# pr_get_SppCount <- function(Type = "Z"){
#
#   if (Type == "P"){
#     gp <- "Phytoplankton"
#   } else if (Type == "Z"){
#     gp <- "Zooplankton"
#   }
#
# out <- sppSummary %>%
#   dplyr::filter(.data$Group == gp)
#
# }


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
#' @param Survey one of NRS, CPR or Both
#' @param interactive A logical TRUE/FALSE if the data is to be used for an interactive plot.
#' @param ... variables to be passed to pr_add_Bioregions. At the moment it only supports `near_dist_km` which is the distance (km) around each bioregion to pad the allocation of points.
#'
#' @return A dataframe for input into pr_plot_Progress()
#' @export
#'
#' @examples
#' df <- pr_get_ProgressMapData(c("NRS", "CPR"))
#' df <- pr_get_ProgressMapData(c("NRS", "CPR", "Coastal", "GO-SHIP"))
#' df <- pr_get_ProgressMapData(c("NRS", "CPR"), interactive = TRUE)
pr_get_ProgressMapData <- function(Survey = c("NRS", "CPR"), interactive = FALSE, ...){

  if (interactive == FALSE){

    PMap <- list()

    if("NRS" %in% Survey) {
      PMap[[1]] <- planktonr::pr_get_NRSTrips(Type = c("P", "Z")) %>%
        dplyr::select("StationCode", "Longitude", "Latitude") %>%
        dplyr::rename(Region = "StationCode") %>%
        dplyr::mutate(Survey = "NRS") %>%
        dplyr::filter(.data$Region != "PH4")
    }

    if ("CPR" %in% Survey) {
      PMap[[2]] <- pr_get_s3("cpr_samp") %>%
        dplyr::select("REGION", "LONGITUDE", "LATITUDE") %>%
        pr_rename() %>%
        dplyr::mutate(Survey = "CPR")
    }

    if ("Coastal" %in% Survey) {
      PMap[[3]]  <- planktonr::pr_get_NRSMicro("Coastal") %>% ## coastal microbial data
        dplyr::select("StationCode", "Longitude", "Latitude") %>%
        dplyr::rename(Region = "StationCode") %>%
        dplyr::mutate(Survey = "Coastal")
    }


    if ("Coastal" %in% Survey) {
      PMap[[4]] <- planktonr::pr_get_NRSMicro("GO-SHIP") %>%
        dplyr::select("Longitude", "Latitude") %>%
        dplyr::mutate(Region = "GO-Ship",
                      Survey = "GO-Ship") %>%
        dplyr::relocate("Region", .before = tidyselect::everything())

    }

    PMapData <- dplyr::bind_rows(PMap)
    return(PMapData)

  } else if (interactive == TRUE){

    PMapDataCoastal <- planktonr::pr_get_NRSMicro("Coastal") %>%
      dplyr::filter(.data$Parameters == "Prochlorococcus_cellsmL") %>%
      dplyr::mutate(Day_Local = lubridate::day(SampleTime_Local)) %>%
      # dplyr::distinct(StationCode, SampleDepth_m, Year_Local, Month_Local, Day_Local, .keep_all = TRUE)
      dplyr::group_by(StationCode, SampleDepth_m, Year_Local, Month_Local, Day_Local) %>%
      dplyr::summarise(Values = mean(Values, na.rm = TRUE),
                       Parameters = dplyr::first(Parameters),
                       StationName = dplyr::first(StationName),
                       Latitude = dplyr::first(Latitude),
                       Longitude = dplyr::first(Longitude),
                       State = dplyr::first(State),
                       SampleTime_Local = dplyr::first(SampleTime_Local),
                       .groups = "drop_last") %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values") %>%
      dplyr::rename(Name = "StationName") %>%
      dplyr::select(-"StationCode") %>%
      dplyr::mutate(Survey = "Coastal")


    PMapDataNRS <- dplyr::bind_rows(planktonr::pr_get_Indices(Survey = "NRS", Type = "Z", ...),
                                    planktonr::pr_get_Indices(Survey = "NRS", Type = "P", ...)) %>%
      dplyr::filter(.data$Parameters == "ZoopAbundance_m3" | .data$Parameters == "PhytoAbundance_CellsL") %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values") %>%
      dplyr::rename(Name = "StationName") %>%
      dplyr::select(-"StationCode") %>%
      dplyr::mutate(Survey = "NRS")


    PMapDataCPR <- dplyr::bind_rows(planktonr::pr_get_Indices(Survey = "CPR", Type = "Z", ...),
                                    planktonr::pr_get_Indices(Survey = "CPR", Type = "P", ...)) %>%
      dplyr::filter(.data$Parameters == "ZoopAbundance_m3" |
                      .data$Parameters == "PhytoAbundance_Cellsm3" |
                      .data$Parameters == "PCI") %>%
      tidyr::pivot_wider(names_from = "Parameters", values_from = "Values") %>%
      dplyr::mutate(PhytoAbundance_Cellsm3 = .data$PhytoAbundance_Cellsm3/1e3,
                    Survey = "CPR") %>%
      dplyr::rename(PhytoAbundance_CellsL = "PhytoAbundance_Cellsm3",
                    Name = "BioRegion")


    PMapData <- dplyr::bind_rows(PMapDataNRS, PMapDataCPR) %>%
      dplyr::select(-c("Year_Local", "Month_Local", "tz"))

    # Map colours for easy plotting
    PMapData <- PMapData %>%
      dplyr::left_join(mbr %>%
                         sf::st_drop_geometry(),
                       by = c("Name" = "REGION"))

    return(PMapData)
  }

}


#' Data for PCI plot from CPR data
#'
#' @return dataframe of PCI data
#' @export
#'
#' @examples
#' head(pr_get_PCIData(),5)
pr_get_PCIData <- function(){
  dat <- planktonr::pr_get_Indices("CPR", "W", near_dist_km = 250) %>%
    dplyr::filter(.data$Parameters == 'PCI') %>%
    dplyr::mutate(Longitude = round(.data$Longitude, 0),
                  Latitude = round(.data$Latitude, 0),
                  Season = dplyr::case_when(.data$Month_Local > 2 & .data$Month_Local < 6 ~ "March - May",
                                            .data$Month_Local > 5 & .data$Month_Local < 9 ~ "June - August",
                                            .data$Month_Local > 8 & .data$Month_Local < 12 ~ "September - November",
                                            TRUE ~ "December - February")) %>%
    dplyr::group_by(.data$Longitude, .data$Latitude, .data$Season, .data$BioRegion) %>%
    dplyr::summarise(PCI = mean(.data$Values, na.rm = TRUE),
                     .groups = 'drop')

}

#' Get taxa accumulation data for plotting
#'
#' @param Survey "NRS or "CPR"
#' @param Type "Z" or "P"
#'
#' @return A data frame
#' @export
#'
#' @examples
#' df <- pr_get_TaxaAccum(Survey = "NRS", Type = "Z")
pr_get_TaxaAccum <- function(Survey = "NRS", Type = "Z"){

  if (Survey == "NRS"){
    dat <- pr_get_NRSData(Type = Type, Variable = "abundance", Subset = "raw")
  } else if (Survey == "CPR"){
    dat <- pr_get_CPRData(Type = Type, Variable = "abundance", Subset = "raw")
  }

  dat <- dat %>%
    tidyr::pivot_longer(-pr_get_NonTaxaColumns(Survey = Survey, Type = Type), names_to = "Taxa", values_to = "Abundance") %>%
    dplyr::filter(.data$Abundance > 0) %>%
    dplyr::arrange(.data$SampleTime_Local) %>%
    dplyr::group_by(.data$Taxa) %>%
    dplyr::summarise(First = dplyr::first(.data$SampleTime_Local), .groups = "drop") %>%
    dplyr::arrange(.data$First) %>%
    dplyr::mutate(RowN = dplyr::row_number())
}


