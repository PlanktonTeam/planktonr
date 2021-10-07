
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
    "Ammonium_umolL", expr(paste("Ammonium (",mu,"mol L"^-1,")")), "Ammonium (&micro; mol L<sup>-1</sup>)",
    "AvgCellVol_um3", expr(paste("Average Cell Volume (mum"^3,")")), "Biovolume (&micro;m<sup>3)",
    "AvgTotalLengthCopepod_mm", expr(paste("Avg. Copepod Length (mm)")), "Avg. Copepod Length (mm)",
    "Biomass_mgm3", expr(paste("Biomass (mg m"^-3,")")), "Biomass (mg m<sup>-3</sup>)",
    "Biovolume_um3L", expr(paste("Biovolume (mum"^3," L"^-1,")")), "Biovolume (&micro;m<sup>3 L<sup>-1</sup>)",
    "BioVolume_um3m3", expr(paste("Biovolume (mum"^3," m"^-3,")")), "Biovolume (&micro;m<sup>3 m^-3</sup>)",
    "Cells_L", expr(paste("Cells (L"^-1,")")), "Cells (L<sup>-1</sup>)",
    "Chla_mgm3", expr(paste("Chla (mg m"^-3,")")), "Chlorophyll <i>a</i> (mg m<sup>-3</sup>)",
    "Conductivity_Sm", expr(paste("Conductivity (Sm")), "Conductivity (Sm)",
    "CopeAbundance_m3", expr(paste("Copepod Abundance (m"^-3,")")), "Copepod Abundance (m<sup>-3</sup>)",
    "CopepodEvenness", expr(paste("Copepod Evenness")), "Copepod Evenness",
    "Density_kgm3", expr(paste("Density (kg m"^-3,")")), "Density (kg m<sup>-3</sup>)",
    "DiatomDinoflagellateRatio", expr(paste("Diatom:Dinoflagellate Ratio")), "Diatom:Dinoflagellate Ratio",
    "DiatomEvenness", expr(paste("Diatom Evenness")), "Diatom Evenness",
    "DIC_umolkg", expr(paste("DIC (",mu,"mol kg"^-1,")")), "DIC (&micro; mol kg<sup>-1</sup>)",
    "DinoflagellateEvenness", expr(paste("Dinoflagellate Evenness")), "Dinoflagellate Evenness",
    "DissolvedOxygen_umolkg", expr(paste("Dissolved Oxygen (",mu,"mol kg"^-1,")")), "Dissolved Oxygen (&micro; mol kg<sup>-1</sup>)",
    "GearDepth_m", expr(paste("GearDepth (m)")), "GearDepth (m)",
    "GearMesh_um", expr(paste("GearMesh (",mu,"m")), "GearMesh (&micro; m)",
    "HerbivoreCarnivoreCopepodRatio", expr(paste("Copepod Herbivore:Carnivore Ratio")), "Copepod Herbivore:Carnivore Ratio",
    "InorganicFraction_mgL", expr(paste("Inorganic Fraction (mg L"^-1,")")), "Inorganic Fraction (mg L<sup>-1</sup>)",
    "Length_mm", expr(paste("Length (mm)")), "Length (mm)",
    "Nitrate_umolL", expr(paste("Nitrate (",mu,"mol L"^-1,")")), "Nitrate (&micro; mol L<sup>-1</sup>)",
    "Nitrite_umolL", expr(paste("Nitrite (",mu,"mol L"^-1,")")), "Nitrite (&micro; mol L<sup>-1</sup>)",
    "NoCopepodSpecies_Sample", expr(paste("Copepod Species (Sample "^-1,")")), "Copepod Species (Sample <sup>-1</sup>",
    "NoDiatomSpecies_Sample", expr(paste("Diatom Species (Sample "^-1,")")), "Diatom Species (Sample <sup>-1</sup>",
    "NoDinoSpecies_Sample", expr(paste("Dinoflagellate Species (Sample "^-1,")")), "Dinoflagellate Species (Sample <sup>-1</sup>",
    "NoPhytoSpecies_Sample", expr(paste("Phytoplankton Species (Sample "^-1,")")), "Phytoplankton Species (Sample <sup>-1</sup>",
    "OrganicFraction_mgL", expr(paste("Organic Fraction (mg L"^-1,")")), "Organic Fraction (mg L<sup>-1</sup>)",
    "Oxygen_umolL", expr(paste("Oxygen (",mu,"mol L"^-1,")")), "Oxygen (&micro; mol L<sup>-1</sup>)",
    "Phosphate_umolL", expr(paste("Phosphate (",mu,"mol L"^-1,")")), "Phosphate (&micro; mol L<sup>-1</sup>)",
    "PhytoAbund_m3", expr(paste("Phytoplankton Abundance (m"^-3,")")), "Phytoplankton Abundance (m<sup>-3</sup>)",
    "PhytoAbund_Cellsm3", expr(paste("Phytoplankton Abundance (cells m"^-3,")")), "Phytoplankton Abundance (cells m<sup>-3</sup>)",
    "PhytoBiomassCarbon_pgm3", expr(paste("Phytoplankton Biomass (pg C m"^-3,")")), "Phytoplankton Biomass (pg C m<sup>-3</sup>)",
    "PhytoEvenness", expr(paste("Phytoplankton Evenness")), "Phytoplankton Evenness",
    "Picoeukaryotes_Cellsml", expr(paste("Picoeukaryotes (cells ml"^-1,")")), "Picoeukaryotes (cells ml<sup>-1</sup>)",
    "PPC", expr(paste("Photoprotective Carotenoids (mg m"^-3,")")), "Photoprotective Carotenoids (mg m<sup>-3</sup>)",
    "Pressure_dbar", expr(paste("Pressure (dbar)")), "Pressure (dbar)",
    "Prochlorococcus_Cellsml", expr(paste("Prochlorococcus (cells ml"^-1,")")), "Prochlorococcus (cells ml<sup>-1</sup>)",
    "PSC", expr(paste("Photosynthetic Carotenoids (mg m"^-3,")")), "Photosynthetic Carotenoids (mg m<sup>-3</sup>)",
    "PSP", expr(paste("Photosynthetic Pigments (mg m"^-3,")")), "Photosynthetic Pigments (mg m<sup>-3</sup>)",
    "Salinity_psu", expr(paste("Salinity (psu)")), "Salinity (psu)",
    "SampleDateLocal", expr(paste("Sample Date (Local)")), "Sample Date (Local)",
    "SampleDateUTC", expr(paste("Sample Date (UTC)")), "Sample Date (UTC)",
    "SampleDepth_m", expr(paste("Sample Depth (m)")), "Sample Depth (m)",
    "SampVol_L", expr(paste("Sample Vol. (L)")), "Sample Vol. (L)",
    "SampVol_m3", expr(paste("Sample Vol. (m"^-3,")")), "Sample Vol. (m<sup>-3</sup>)",
    "Secchi_m", expr(paste("Secchi Depth (m)")), "Secchi Depth (m)",
    "ShannonCopepodDiversity", expr(paste("Copepod Diversity (Shannons)")), "Copepod Diversity (Shannons)",
    "ShannonDiatomDiversitycpr", expr(paste("Diatom Diversity (Shannons)")), "Diatom Diversity (Shannons)",
    "ShannonDinoDiversitycpr", expr(paste("Dinoflagellate Diversity (Shannons)")), "Dinoflagellate Diversity (Shannons)",
    "ShannonPhytoDiversitycpr", expr(paste("Phytoplankton Diversity (Shannons)")), "Phytoplankton Diversity (Shannons)",
    "Silicate_umolL", expr(paste("Silicate (",mu,"mol L"^-1,")")), "Silicate (&micro; mol L<sup>-1</sup>)",
    "StationCode", expr(paste("Station Code")), "Station Code)",
    "StationName", expr(paste("Station Name")), "Station Name)",
    "Synecochoccus_Cellsml", expr(paste("Synecochoccus (cells ml"^-1,")")), "Synecochoccus (cells ml<sup>-1</sup>)",
    "TAcc", expr(paste("Total Accessory Pigments (mg m"^-3,")")), "Total Accessory Pigments (mg m<sup>-3</sup>)",
    "TAlkalinity_umolkg", expr(paste("TAlkalinity (",mu,"mol kg"^-1,")")), "TAlkalinity (&micro; mol kg<sup>-1</sup>)",
    "TaxonCount", expr(paste("Taxon Count")), "Taxon Count)",
    "TaxonGroup", expr(paste("Taxon Group")), "Taxon Group)",
    "TaxonName", expr(paste("Taxon Name")), "Taxon Name)",
    "TCaro", expr(paste("Total Carotenoids (mg m"^-3,")")), "Total Carotenoids (mg m<sup>-3</sup>)",
    "TDP", expr(paste("Total Diagnostic pigments (mg m"^-3,")")), "Total Diagnostic pigments (mg m<sup>-3</sup>)",
    "Temperature_degC", expr(paste("Temperature (",degree,"C)")), "Temperature (&deg;C C)",
    "TowType", expr(paste("Tow Type")), "Tow Type)",
    "TotalChla", expr(paste("Total Chlorophyll ",italic(a)," (mg m"^-3,")")), "Total Chlorophyll <i>a</i>, (mg m<sup>-3</sup>)",
    "TPig", expr(paste("Total Pigments (mg m"^-3,")")), "Total Pigments (mg m<sup>-3</sup>)",
    "TripCode", expr(paste("Trip Code")), "Trip Code)",
    "TSS_mgL", expr(paste("TSS (mg L"^-1,")")), "TSS (mg L<sup>-1</sup>)",
    "Turbidity_NTU", expr(paste("Turbidity (NTU)")), "Turbidity (NTU)",
    "Volume_m3", expr(paste("Volume m"^-3,")")), "Volume m<sup>-3</sup>)",
    "WaterDensity_kgm3", expr(paste("Water Density (kg m"^-3,")")), "Water Density (kg m<sup>-3</sup>)",
    "WaterDepth_m", expr(paste("Water Depth (m)")), "Water Depth (m)",
    "ZoopAbund_m3", expr(paste("Zooplankton Abundance (m"^-3,")")), "Zooplankton Abundance (m<sup>-3</sup>)"),
    ncol = 3, byrow = TRUE, dimnames = list(NULL, c("Variable","gg", "pl"))))

  i <- which(stringr::str_detect(relabel_df$Variable, s))

  if(style == "ggplot" & length(i) > 0) {
    return(relabel_df$gg[[i]])
  } else if (style == "plotly" & length(i) > 0){
    return(relabel_df$pl[[i]])
  } else if (length(i) == 1){
    return(s) # If no match, return the original string
  }
}

# s <- "Temperature_degC"
# e <- pr_relabel(s)
# ggplot2::ggplot() + ggplot2::ggtitle(enexpr(e)) + ggplot2::ylab(enexpr(e)) + ggplot2::xlab(enexpr(e))

