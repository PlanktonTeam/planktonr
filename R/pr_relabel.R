
#' Format a parameter string for plotting
#'
#' @param s A string for reformatting
#' @param style The style of plotting the string will be used for
#'
#' @return A reformatted string (plotly) or a reformatted expression call (ggplot)
#' @export
#'
#' @examples
#' pr_relabel("Chla_mgm3", style = "ggplot")
#'
pr_relabel <- function(s, style = "ggplot"){

  relabel_df <- tibble::as_tibble(matrix(c(
    "AbundancePhyto_CellsL", rlang::expr(paste("Phytoplankton Abundance (cells L"^-1,")")), "Phytoplankton Abundance (cells L<sup>-1</sup>)", "Phytoplankton Abundance",
    "Ammonium_umolL", rlang::expr(paste("Ammonium (","\U00B5","mol L"^-1,")")), "Ammonium (&#181; mol L<sup>-1</sup>)", "Ammonium",
    "AvgCellVol_um3", rlang::expr(paste("Average Cell Volume (mum"^3,"sample"^-1,")")), "Average Cell Volume (&#181;m<sup>3</sup> sample<sup>-1</sup>)", "Average Cell Volume",
    "AvgTotalLengthCopepod_mm", rlang::expr(paste("Avg. Copepod Length (mm)")), "Avg. Copepod Length (mm)", "Copepod Length",
    "Biomass_mgm3", rlang::expr(paste("Biomass (mg m"^-3,")")), "Biomass (mg m<sup>-3</sup>)", "Zooplankton Biomass",
    "Biovolume_um3L", rlang::expr(paste("Biovolume (mum"^3," L"^-1,")")), "Biovolume (&#181;m<sup>3 L<sup>-1</sup>)", "Phytoplankton Biovolume",
    "BioVolume_um3m3", rlang::expr(paste("Biovolume (mum"^3," m"^-3,")")), "Biovolume (&#181;m<sup>3 m^-3</sup>)", "Phytoplankton Biovolume",
    "Cells_L", rlang::expr(paste("Cells (L"^-1,")")), "Cells (L<sup>-1</sup>)", "Cells",
    "Chla_mgm3", rlang::expr(paste("Chla (mg m"^-3,")")), "Chlorophyll <i>a</i> (mg m<sup>-3</sup>)", "Chlorophyll a",
    "Conductivity_Sm", rlang::expr(paste("Conductivity (Sm")), "Conductivity (Sm)", "Conductivity (Sm)",
    "CopeAbundance_m3", rlang::expr(paste("Copepod Abundance (m"^-3,")")), "Copepod Abundance (m<sup>-3</sup>)", "Copepod Abundance",
    "CopepodEvenness", rlang::expr(paste("Copepod Evenness")), "Copepod Evenness", "Copepod Evenness",
    "Density_kgm3", rlang::expr(paste("Density (kg m"^-3,")")), "Density (kg m<sup>-3</sup>)", "Density",
    "DiatomDinoflagellateRatio", rlang::expr(paste("Diatom:Dinoflagellate Ratio")), "Diatom:Dinoflagellate Ratio", "Diatom:Dinoflagellate Ratio",
    "DiatomEvenness", rlang::expr(paste("Diatom Evenness")), "Diatom Evenness", "Diatom Evenness",
    "DIC_umolkg", rlang::expr(paste("DIC (","\U00B5","mol kg"^-1,")")), "DIC (&#181; mol kg<sup>-1</sup>)", "DIC",
    "DinoflagellateEvenness", rlang::expr(paste("Dinoflagellate Evenness")), "Dinoflagellate Evenness", "Dinoflagellate Evenness",
    "DissolvedOxygen_umolkg", rlang::expr(paste("Dissolved Oxygen (","\U00B5","mol kg"^-1,")")), "Dissolved Oxygen (&#181; mol kg<sup>-1</sup>)", "Dissolved Oxygen",
    "FunctionalGroup_m3", rlang::expr(paste("Functional Group (m"^-3,")")), "Functional Group (m<sup>-3</sup>)", "Functional Group",
    "FunctionalGroup_CellsL", rlang::expr(paste("Functional Group (cells L"^-1,")")), "Functional Group (cells L<sup>-1</sup>)", "Functional Group",
    "GearDepth_m", rlang::expr(paste("Gear Depth (m)")), "Gear Depth (m)", "GearDepth",
    "GearMesh_um", rlang::expr(paste("Gear Mesh (","\U00B5","m")), "Gear Mesh (&#181; m)", "GearMesh",
    "HerbivoreCarnivoreCopepodRatio", rlang::expr(paste("Copepod Herbivore:Carnivore Ratio")), "Copepod Herbivore:Carnivore Ratio", "Copepod Herbivore:Carnivore Ratio",
    "InorganicFraction_mgL", rlang::expr(paste("Inorganic Fraction (mg L"^-1,")")), "Inorganic Fraction (mg L<sup>-1</sup>)", "Inorganic Fraction",
    "Length_mm", rlang::expr(paste("Length (mm)")), "Length (mm)", "Length",
    "Nitrate_umolL", rlang::expr(paste("Nitrate (","\U00B5","mol L"^-1,")")), "Nitrate (&#181; mol L<sup>-1</sup>)", "Nitrate",
    "Nitrite_umolL", rlang::expr(paste("Nitrite (","\U00B5","mol L"^-1,")")), "Nitrite (&#181; mol L<sup>-1</sup>)", "Nitrite",
    "NoCopepodSpecies_Sample", rlang::expr(paste("Copepod Species (Sample "^-1,")")), "Copepod Species (Sample <sup>-1</sup>)", "Copepod Species Count",
    "NoDiatomSpecies_Sample", rlang::expr(paste("Diatom Species (Sample "^-1,")")), "Diatom Species (Sample <sup>-1</sup>)", "Diatom Species Count",
    "NoDinoSpecies_Sample", rlang::expr(paste("Dinoflagellate Species (Sample "^-1,")")), "Dinoflagellate Species (Sample <sup>-1</sup>)", "Dinoflagellate Species Count",
    "NoPhytoSpecies_Sample", rlang::expr(paste("Phytoplankton Species (Sample "^-1,")")), "Phytoplankton Species (Sample <sup>-1</sup>)", "Phytoplankton Species Count",
    "OrganicFraction_mgL", rlang::expr(paste("Organic Fraction (mg L"^-1,")")), "Organic Fraction (mg L<sup>-1</sup>)", "Organic Fraction",
    "Oxygen_umolL", rlang::expr(paste("Oxygen (","\U00B5","mol L"^-1,")")), "Oxygen (&#181; mol L<sup>-1</sup>)", "Oxygen",
    "Phosphate_umolL", rlang::expr(paste("Phosphate (","\U00B5","mol L"^-1,")")), "Phosphate (&#181; mol L<sup>-1</sup>)", "Phosphate",
    "PhytoAbund_m3", rlang::expr(paste("Phytoplankton Abundance (m"^-3,")")), "Phytoplankton Abundance (m<sup>-3</sup>)", "Phytoplankton Abundance",
    "PhytoAbund_Cellsm3", rlang::expr(paste("Phytoplankton Abundance (cells m"^-3,")")), "Phytoplankton Abundance (cells m<sup>-3</sup>)", "Phytoplankton Abundance",
    "PhytoBiomassCarbon_pgm3", rlang::expr(paste("Phytoplankton Biomass (pg C m"^-3,")")), "Phytoplankton Biomass (pg C m<sup>-3</sup>)", "Phytoplankton Biomass",
    "PhytoBiomassCarbon_pgL", rlang::expr(paste("Phytoplankton Biomass (pg C L"^-1,")")), "Phytoplankton Biomass (pg C L<sup>-1</sup>)", "Phytoplankton Biomass",
    "PhytoEvenness", rlang::expr(paste("Phytoplankton Evenness")), "Phytoplankton Evenness", "Phytoplankton Evenness",
    "Picoeukaryotes_Cellsml", rlang::expr(paste("Picoeukaryotes (cells ml"^-1,")")), "Picoeukaryotes (cells ml<sup>-1</sup>)", "Picoeukaryote Abundance",
    "Picoplankton_Cellsml", rlang::expr(paste("Picoplankton (cells ml"^-1,")")), "Picoplankton (cells ml<sup>-1</sup>)", "Picoplankton Abundance",
    "PPC", rlang::expr(paste("Photoprotective Carotenoids (mg m"^-3,")")), "Photoprotective Carotenoids (mg m<sup>-3</sup>)", "Photoprotective Carotenoid Abundance",
    "Pressure_dbar", rlang::expr(paste("Pressure (dbar)")), "Pressure (dbar)", "Pressure",
    "Prochlorococcus_Cellsml", rlang::expr(paste("Prochlorococcus (cells ml"^-1,")")), "Prochlorococcus (cells ml<sup>-1</sup>)", "Prochlorococcus Abundance",
    "PSC", rlang::expr(paste("Photosynthetic Carotenoids (mg m"^-3,")")), "Photosynthetic Carotenoids (mg m<sup>-3</sup>)", "Photosynthetic Carotenoid Abundance",
    "PSP", rlang::expr(paste("Photosynthetic Pigments (mg m"^-3,")")), "Photosynthetic Pigments (mg m<sup>-3</sup>)", "Photosynthetic Pigment Abundance",
    "Salinity_psu", rlang::expr(paste("Salinity (psu)")), "Salinity (psu)", "Salinity",
    "SampleDateLocal", rlang::expr(paste("Sample Date (Local)")), "Sample Date (Local)", "Sample Date",
    "SampleDateUTC", rlang::expr(paste("Sample Date (UTC)")), "Sample Date (UTC)", "Sample Date",
    "SampleDepth_m", rlang::expr(paste("Sample Depth (m)")), "Sample Depth (m)", "Sample Depth",
    "SampVol_L", rlang::expr(paste("Sample Volume (L)")), "Sample Volume (L)", "Sample Volume",
    "SampVol_m3", rlang::expr(paste("Sample Volume (m"^-3,")")), "Sample Volume (m<sup>-3</sup>)", "Sample Volume",
    "Secchi_m", rlang::expr(paste("Secchi Depth (m)")), "Secchi Depth (m)", "Secchi Depth",
    "ShannonCopepodDiversity", rlang::expr(paste("Copepod Diversity")), "Copepod Diversity", "Copepod Diversity",
    "ShannonDiatomDiversitycpr", rlang::expr(paste("Diatom Diversity")), "Diatom Diversity", "Diatom Diversity",
    "ShannonDiatomDiversity", rlang::expr(paste("Diatom Diversity")), "Diatom Diversity", "Diatom Diversity",
    "ShannonDinoDiversitycpr", rlang::expr(paste("Dinoflagellate Diversity")), "Dinoflagellate Diversity", "Dinoflagellate Diversity",
    "ShannonDinoDiversity", rlang::expr(paste("Dinoflagellate Diversity")), "Dinoflagellate Diversity", "Dinoflagellate Diversity",
    "ShannonPhytoDiversitycpr", rlang::expr(paste("Phytoplankton Diversity")), "Phytoplankton Diversity", "Phytoplankton Diversity",
    "ShannonPhytoDiversity", rlang::expr(paste("Phytoplankton Diversity")), "Phytoplankton Diversity", "Phytoplankton Diversity",
    "Silicate_umolL", rlang::expr(paste("Silicate (","\U00B5","mol L"^-1,")")), "Silicate (&#181; mol L<sup>-1</sup>)", "Silicate",
    "StationCode", rlang::expr(paste("Station Code")), "Station Code", "Station Code",
    "StationName", rlang::expr(paste("Station Name")), "Station Name", "Station Name",
    "Synecochoccus_Cellsml", rlang::expr(paste("Synecochoccus (cells ml"^-1,")")), "Synecochoccus (cells ml<sup>-1</sup>)", "Synecochoccus Abundance",
    "TAcc", rlang::expr(paste("Total Accessory Pigments (mg m"^-3,")")), "Total Accessory Pigments (mg m<sup>-3</sup>)", "Total Accessory Pigments",
    "TAlkalinity_umolkg", rlang::expr(paste("TAlkalinity (","\U00B5","mol kg"^-1,")")), "Total Alkalinity (&#181; mol kg<sup>-1</sup>)", "Total Alkalinity",
    "TaxonCount", rlang::expr(paste("Taxon Count")), "Taxon Count", "Taxon Count",
    "TaxonGroup", rlang::expr(paste("Taxon Group")), "Taxon Group", "Taxon Group",
    "TaxonName", rlang::expr(paste("Taxon Name")), "Taxon Name", "Taxon Name",
    "TCaro", rlang::expr(paste("Total Carotenoids (mg m"^-3,")")), "Total Carotenoids (mg m<sup>-3</sup>)", "Total Carotenoids",
    "TDP", rlang::expr(paste("Total Diagnostic Pigments (mg m"^-3,")")), "Total Diagnostic Pigments (mg m<sup>-3</sup>)", "Total Diagnostic Pigments",
    "Temperature_degC", rlang::expr(paste("Temperature (","\U00B0","C)")), "Temperature (&#8451;)", "Temperature",
    "TowType", rlang::expr(paste("Tow Type")), "Tow Type", "Tow Type",
    "TotalChla", rlang::expr(paste("Total Chlorophyll ",italic("a")," (mg m"^-3,")")), "Total Chlorophyll <i>a</i>, (mg m<sup>-3</sup>)", "Total Chlorophyll a",
    "TPig", rlang::expr(paste("Total Pigments (mg m"^-3,")")), "Total Pigments (mg m<sup>-3</sup>)", "Total Pigments",
    "TripCode", rlang::expr(paste("Trip Code")), "Trip Code", "Trip Code",
    "TSS_mgL", rlang::expr(paste("TSS (mg L"^-1,")")), "TSS (mg L<sup>-1</sup>)", "TSS",
    "Turbidity_NTU", rlang::expr(paste("Turbidity (NTU)")), "Turbidity (NTU)", "Turbidity",
    "Volume_m3", rlang::expr(paste("Volume m"^-3,")")), "Volume m<sup>-3</sup>)", "Volume",
    "WaterDensity_kgm3", rlang::expr(paste("Water Density (kg m"^-3,")")), "Water Density (kg m<sup>-3</sup>)", "Water Density",
    "WaterDepth_m", rlang::expr(paste("Water Depth (m)")), "Water Depth (m)", "Water Depth",
    "ZoopAbund_m3", rlang::expr(paste("Zooplankton Abundance (m"^-3,")")), "Zooplankton Abundance (m<sup>-3</sup>)", "Zooplankton Abundance",
    "Bacterial_Richness", rlang::expr(paste("Bacterial Richness")), "Bacterial Richness", "Bacterial Richness",
    "Archaeal_Richness", rlang::expr(paste("Archaeal Richness")), "Archaeal_Richness", "Archaeal_Richness",
    "Eukaryote_Richness", rlang::expr(paste("Eukaryote Richness")), "Eukaryote Richness", "Eukaryote Richness",
    "Bacterial_Niche_Cluster", rlang::expr(paste("Bacterial Niche Cluster")), "Bacterial Niche Cluster", "Bacterial Niche Cluster",
    "Eukaryote_Niche_Cluster", rlang::expr(paste("Eukaryote Niche Cluster")), "Eukaryote Niche Cluster", "Eukaryote Niche Cluster",
    "Archaea_Niche_Cluster", rlang::expr(paste("Archaea Niche Cluster")), "Archaea Niche Cluster", "Archaea Niche Cluster",
    "Bacterial_Chlorophyll_Index", rlang::expr(paste("Bacterial Chlorophyll Index")), "Bacterial Chlorophyll Index", "Bacterial Chlorophyll Index",
    "Bacterial_Nitrogen_Index", rlang::expr(paste("Bacterial Nitrogen Index")), "Bacterial Nitrogen Index", "Bacterial Nitrogen Index",
    "Bacterial_Oxygen_Index", rlang::expr(paste("Bacterial Oxygen Index")), "Bacterial Oxygen Index", "Bacterial Oxygen Index",
    "Bacterial_Phosphate_Index", rlang::expr(paste("Bacterial Phosphate Index")), "Bacterial Phosphate Index", "Bacterial Phosphate Index",
    "Bacterial_Salinity_Index", rlang::expr(paste("Bacterial Salinity Index")), "Bacterial Salinity Index", "Bacterial Salinity Index",
    "Bacterial_Silicate_Index", rlang::expr(paste("Bacterial Silicate Index")), "Bacterial Silicate Index", "Bacterial Silicate Index",
    "Bacterial_Temperature_Index", rlang::expr(paste("Bacterial Temperature Index")), "Bacterial Temperature Index", "Bacterial Temperature Index",
    "Archaeal_Temperature_Index", rlang::expr(paste("Archaeal Temperature Index")), "Archaeal Temperature Index", "Archaeal Temperature Index",
    "Archaeal_Salinity_Index", rlang::expr(paste("Archaeal Salinity Index")), "Archaeal Salinity Index", "Archaeal Salinity Index",
    "Archaeal_Nitrogen_Index", rlang::expr(paste("Archaeal Nitrogen Index")), "Archaeal Nitrogen Index", "Archaeal Nitrogen Index",
    "Archaeal_Phosphate_Index", rlang::expr(paste("Archaeal Phosphate Index")), "Archaeal Phosphate Index", "Archaeal Phosphate Index",
    "Archaeal_Silicate_Index", rlang::expr(paste("Archaeal Silicate Index")), "Archaeal Silicate Index", "Archaeal Silicate Index",
    "Archaeal_Oxygen_Index", rlang::expr(paste("Archaeal Oxygen Index")), "Archaeal Oxygen Index", "Archaeal Oxygen Index",
    "Archaeal_Chlorophyll_Index", rlang::expr(paste("Archaeal Chlorophyll Index")), "Archaeal Chlorophyll Index", "Archaeal Chlorophyll Index",
    "Eukaryote_Temperature_Index", rlang::expr(paste("Eukaryote Temperature Index")), "Eukaryote Temperature Index", "Eukaryote Temperature Index",
    "Eukaryote_Salinity_Index", rlang::expr(paste("Eukaryote Salinity Index")), "Eukaryote Salinity Index", "Eukaryote Salinity Index",
    "Eukaryote_Nitrogen_Index", rlang::expr(paste("Eukaryote Nitrogen Index")), "Eukaryote Nitrogen Index", "Eukaryote Nitrogen Index",
    "Eukaryote_Phosphate_Index", rlang::expr(paste("Eukaryote Phosphate Index")), "Eukaryote Phosphate Index", "Eukaryote Phosphate Index",
    "Eukaryote_Silicate_Index", rlang::expr(paste("Eukaryote Silicate Index")), "Eukaryote Silicate Index", "Eukaryote Silicate Index",
    "Eukaryote_Oxygen_Diversity", rlang::expr(paste("Eukaryote Oxygen Diversity")), "Eukaryote Oxygen Diversity", "Eukaryote Oxygen Diversity",
    "Eukaryote_Chlorophyll_Index", rlang::expr(paste("Eukaryote Chlorophyll Index")), "Eukaryote Chlorophyll Index", "Eukaryote Chlorophyll Index"),
    ncol = 4, byrow = TRUE, dimnames = list(NULL, c("Variable","gg", "pl", "simple"))))

  i <- which(relabel_df$Variable %in% s)

  if(style == "ggplot" & length(i) > 0) {
    return(relabel_df$gg[[i]]) # Returned as a list due to expr
  } else if (style == "plotly" & length(i) > 0){
    return(unlist(relabel_df$pl[i]))
  } else if (style == "simple" & length(i) > 0){
    out <- relabel_df$Variable[i]
    names(out) <- relabel_df$simple[i]
    out <- out[order(names(out))] # Put in alpha order
    return(out)
  } else if (length(i) == 1){
    return(s) # If no match, return the original string
  }
}

# s <- c("Temperature_degC", "WaterDensity_kgm3")
# s <- c("Temperature_degC")
# e <- pr_relabel(s, style = "ggplot")
# ggplot() + ggtitle(enexpr(e)) + ylab(enexpr(e)) + xlab(enexpr(e))

# Working prototype
#
# pr_relabel2 <- function(s){
#   relabel_df <- tibble::as_tibble(matrix(c(
#     "TotalChla", rlang::expr(paste("Total Chlorophyll ",italic(a)," (mg m"^-3,")")),
#     "PPC", rlang::expr(paste("Photoprotective Carotenoids (mg m"^-3,")")),
#     "PSC", rlang::expr(paste("Photosynthetic Carotenoids (mg m"^-3,")"))),
#     ncol = 2, byrow = TRUE, dimnames = list(NULL, c("St","Ex"))))
#
#   i <- which(stringr::str_detect(relabel_df$St, s))
#
#   return(relabel_df$Ex[[i]])
# }
#
# s <- "TotalChla"
# e <- pr_relabel2(s)
# ggplot2::ggplot() + ggplot2::ggtitle(e) + ggplot2::ylab(e) + ggplot2::xlab(e)
# class(e)
#
# s <- "PSC"
# e <- pr_relabel(s, "ggplot")
# ggplot2::ggplot() + ggplot2::ggtitle(e) + ggplot2::ylab(e) + ggplot2::xlab(e)
#
# s <- "ZoopAbund_m3"
# e <- pr_relabel(s, "ggplot")
# ggplot2::ggplot() + ggplot2::ggtitle(e) + ggplot2::ylab(e) + ggplot2::xlab(e)
