
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
    "AbundancePhyto_CellsL", expr(paste("Phytoplankton Abundance (cells L"^-1,")")), "Phytoplankton Abundance (cells L<sup>-1</sup>)", "Phytoplankton Abundance",
    "Ammonium_umolL", expr(paste("Ammonium (",mu,"mol L"^-1,")")), "Ammonium (&#181; mol L<sup>-1</sup>)", "Ammonium",
    "AvgCellVol_um3", expr(paste("Average Cell Volume (mum"^3,")")), "Biovolume (&#181;m<sup>3)", "Biovolume",
    "AvgTotalLengthCopepod_mm", expr(paste("Avg. Copepod Length (mm)")), "Avg. Copepod Length (mm)", "Copepod Length",
    "Biomass_mgm3", expr(paste("Biomass (mg m"^-3,")")), "Biomass (mg m<sup>-3</sup>)", "Zooplankton Biomass",
    "Biovolume_um3L", expr(paste("Biovolume (mum"^3," L"^-1,")")), "Biovolume (&#181;m<sup>3 L<sup>-1</sup>)", "Phytoplankton Biovolume",
    "BioVolume_um3m3", expr(paste("Biovolume (mum"^3," m"^-3,")")), "Biovolume (&#181;m<sup>3 m^-3</sup>)", "Phytoplankton Biovolume",
    "Cells_L", expr(paste("Cells (L"^-1,")")), "Cells (L<sup>-1</sup>)", "Cells",
    "Chla_mgm3", expr(paste("Chla (mg m"^-3,")")), "Chlorophyll <i>a</i> (mg m<sup>-3</sup>)", "Chlorophyll a",
    "Conductivity_Sm", expr(paste("Conductivity (Sm")), "Conductivity (Sm)", "Conductivity (Sm)",
    "CopeAbundance_m3", expr(paste("Copepod Abundance (m"^-3,")")), "Copepod Abundance (m<sup>-3</sup>)", "Copepod Abundance",
    "CopepodEvenness", expr(paste("Copepod Evenness")), "Copepod Evenness", "Copepod Evenness",
    "Density_kgm3", expr(paste("Density (kg m"^-3,")")), "Density (kg m<sup>-3</sup>)", "Density",
    "DiatomDinoflagellateRatio", expr(paste("Diatom:Dinoflagellate Ratio")), "Diatom:Dinoflagellate Ratio", "Diatom:Dinoflagellate Ratio",
    "DiatomEvenness", expr(paste("Diatom Evenness")), "Diatom Evenness", "Diatom Evenness",
    "DIC_umolkg", expr(paste("DIC (",mu,"mol kg"^-1,")")), "DIC (&#181; mol kg<sup>-1</sup>)", "DIC",
    "DinoflagellateEvenness", expr(paste("Dinoflagellate Evenness")), "Dinoflagellate Evenness", "Dinoflagellate Evenness",
    "DissolvedOxygen_umolkg", expr(paste("Dissolved Oxygen (",mu,"mol kg"^-1,")")), "Dissolved Oxygen (&#181; mol kg<sup>-1</sup>)", "Dissolved Oxygen",
    "GearDepth_m", expr(paste("Gear Depth (m)")), "Gear Depth (m)", "GearDepth",
    "GearMesh_um", expr(paste("Gear Mesh (",mu,"m")), "Gear Mesh (&#181; m)", "GearMesh",
    "HerbivoreCarnivoreCopepodRatio", expr(paste("Copepod Herbivore:Carnivore Ratio")), "Copepod Herbivore:Carnivore Ratio", "Copepod Herbivore:Carnivore Ratio",
    "InorganicFraction_mgL", expr(paste("Inorganic Fraction (mg L"^-1,")")), "Inorganic Fraction (mg L<sup>-1</sup>)", "Inorganic Fraction",
    "Length_mm", expr(paste("Length (mm)")), "Length (mm)", "Length",
    "Nitrate_umolL", expr(paste("Nitrate (",mu,"mol L"^-1,")")), "Nitrate (&#181; mol L<sup>-1</sup>)", "Nitrate",
    "Nitrite_umolL", expr(paste("Nitrite (",mu,"mol L"^-1,")")), "Nitrite (&#181; mol L<sup>-1</sup>)", "Nitrite",
    "NoCopepodSpecies_Sample", expr(paste("Copepod Species (Sample "^-1,")")), "Copepod Species (Sample <sup>-1</sup>)", "Copepod Species Count",
    "NoDiatomSpecies_Sample", expr(paste("Diatom Species (Sample "^-1,")")), "Diatom Species (Sample <sup>-1</sup>)", "Diatom Species Count",
    "NoDinoSpecies_Sample", expr(paste("Dinoflagellate Species (Sample "^-1,")")), "Dinoflagellate Species (Sample <sup>-1</sup>)", "Dinoflagellate Species Count",
    "NoPhytoSpecies_Sample", expr(paste("Phytoplankton Species (Sample "^-1,")")), "Phytoplankton Species (Sample <sup>-1</sup>)", "Phytoplankton Species Count",
    "OrganicFraction_mgL", expr(paste("Organic Fraction (mg L"^-1,")")), "Organic Fraction (mg L<sup>-1</sup>)", "Organic Fraction",
    "Oxygen_umolL", expr(paste("Oxygen (",mu,"mol L"^-1,")")), "Oxygen (&#181; mol L<sup>-1</sup>)", "Oxygen",
    "Phosphate_umolL", expr(paste("Phosphate (",mu,"mol L"^-1,")")), "Phosphate (&#181; mol L<sup>-1</sup>)", "Phosphate",
    "PhytoAbund_m3", expr(paste("Phytoplankton Abundance (m"^-3,")")), "Phytoplankton Abundance (m<sup>-3</sup>)", "Phytoplankton Abundance",
    "PhytoAbund_Cellsm3", expr(paste("Phytoplankton Abundance (cells m"^-3,")")), "Phytoplankton Abundance (cells m<sup>-3</sup>)", "Phytoplankton Abundance",
    "PhytoBiomassCarbon_pgm3", expr(paste("Phytoplankton Biomass (pg C m"^-3,")")), "Phytoplankton Biomass (pg C m<sup>-3</sup>)", "Phytoplankton Biomass",
    "PhytoBiomassCarbon_pgL", expr(paste("Phytoplankton Biomass (pg C L"^-1,")")), "Phytoplankton Biomass (pg C L<sup>-1</sup>)", "Phytoplankton Biomass",
    "PhytoEvenness", expr(paste("Phytoplankton Evenness")), "Phytoplankton Evenness", "Phytoplankton Evenness",
    "Picoeukaryotes_Cellsml", expr(paste("Picoeukaryotes (cells ml"^-1,")")), "Picoeukaryotes (cells ml<sup>-1</sup>)", "Picoeukaryote Abundance",
    "PPC", expr(paste("Photoprotective Carotenoids (mg m"^-3,")")), "Photoprotective Carotenoids (mg m<sup>-3</sup>)", "Photoprotective Carotenoid Abundance",
    "Pressure_dbar", expr(paste("Pressure (dbar)")), "Pressure (dbar)", "Pressure",
    "Prochlorococcus_Cellsml", expr(paste("Prochlorococcus (cells ml"^-1,")")), "Prochlorococcus (cells ml<sup>-1</sup>)", "Prochlorococcus Abundance",
    "PSC", expr(paste("Photosynthetic Carotenoids (mg m"^-3,")")), "Photosynthetic Carotenoids (mg m<sup>-3</sup>)", "Photosynthetic Carotenoid Abundance",
    "PSP", expr(paste("Photosynthetic Pigments (mg m"^-3,")")), "Photosynthetic Pigments (mg m<sup>-3</sup>)", "Photosynthetic Pigment Abundance",
    "Salinity_psu", expr(paste("Salinity (psu)")), "Salinity (psu)", "Salinity",
    "SampleDateLocal", expr(paste("Sample Date (Local)")), "Sample Date (Local)", "Sample Date",
    "SampleDateUTC", expr(paste("Sample Date (UTC)")), "Sample Date (UTC)", "Sample Date",
    "SampleDepth_m", expr(paste("Sample Depth (m)")), "Sample Depth (m)", "Sample Depth",
    "SampVol_L", expr(paste("Sample Volume (L)")), "Sample Volume (L)", "Sample Volume",
    "SampVol_m3", expr(paste("Sample Volume (m"^-3,")")), "Sample Volume (m<sup>-3</sup>)", "Sample Volume",
    "Secchi_m", expr(paste("Secchi Depth (m)")), "Secchi Depth (m)", "Secchi Depth",
    "ShannonCopepodDiversity", expr(paste("Copepod Diversity (Shannons)")), "Copepod Diversity (Shannons)", "Copepod Diversity",
    "ShannonDiatomDiversitycpr", expr(paste("Diatom Diversity (Shannons)")), "Diatom Diversity (Shannons)", "Diatom Diversity",
    "ShannonDiatomDiversity", expr(paste("Diatom Diversity (Shannons)")), "Diatom Diversity (Shannons)", "Diatom Diversity",
    "ShannonDinoDiversitycpr", expr(paste("Dinoflagellate Diversity (Shannons)")), "Dinoflagellate Diversity (Shannons)", "Dinoflagellate Diversity",
    "ShannonDinoDiversity", expr(paste("Dinoflagellate Diversity (Shannons)")), "Dinoflagellate Diversity (Shannons)", "Dinoflagellate Diversity",
    "ShannonPhytoDiversitycpr", expr(paste("Phytoplankton Diversity (Shannons)")), "Phytoplankton Diversity (Shannons)", "Phytoplankton Diversity",
    "ShannonPhytoDiversity", expr(paste("Phytoplankton Diversity (Shannons)")), "Phytoplankton Diversity (Shannons)", "Phytoplankton Diversity",
    "Silicate_umolL", expr(paste("Silicate (",mu,"mol L"^-1,")")), "Silicate (&#181; mol L<sup>-1</sup>)", "Silicate",
    "StationCode", expr(paste("Station Code")), "Station Code", "Station Code",
    "StationName", expr(paste("Station Name")), "Station Name", "Station Name",
    "Synecochoccus_Cellsml", expr(paste("Synecochoccus (cells ml"^-1,")")), "Synecochoccus (cells ml<sup>-1</sup>)", "Synecochoccus Abundance",
    "TAcc", expr(paste("Total Accessory Pigments (mg m"^-3,")")), "Total Accessory Pigments (mg m<sup>-3</sup>)", "Total Accessory Pigments",
    "TAlkalinity_umolkg", expr(paste("TAlkalinity (",mu,"mol kg"^-1,")")), "Total Alkalinity (&#181; mol kg<sup>-1</sup>)", "Total Alkalinity",
    "TaxonCount", expr(paste("Taxon Count")), "Taxon Count", "Taxon Count",
    "TaxonGroup", expr(paste("Taxon Group")), "Taxon Group", "Taxon Group",
    "TaxonName", expr(paste("Taxon Name")), "Taxon Name", "Taxon Name",
    "TCaro", expr(paste("Total Carotenoids (mg m"^-3,")")), "Total Carotenoids (mg m<sup>-3</sup>)", "Total Carotenoids",
    "TDP", expr(paste("Total Diagnostic Pigments (mg m"^-3,")")), "Total Diagnostic Pigments (mg m<sup>-3</sup>)", "Total Diagnostic Pigments",
    "Temperature_degC", expr(paste("Temperature (",degree,"C)")), "Temperature (&#8451;)", "Temperature",
    "TowType", expr(paste("Tow Type")), "Tow Type", "Tow Type",
    "TotalChla", expr(paste("Total Chlorophyll ",italic(a)," (mg m"^-3,")")), "Total Chlorophyll <i>a</i>, (mg m<sup>-3</sup>)", "Total Chlorophyll a",
    "TPig", expr(paste("Total Pigments (mg m"^-3,")")), "Total Pigments (mg m<sup>-3</sup>)", "Total Pigments",
    "TripCode", expr(paste("Trip Code")), "Trip Code", "Trip Code",
    "TSS_mgL", expr(paste("TSS (mg L"^-1,")")), "TSS (mg L<sup>-1</sup>)", "TSS",
    "Turbidity_NTU", expr(paste("Turbidity (NTU)")), "Turbidity (NTU)", "Turbidity",
    "Volume_m3", expr(paste("Volume m"^-3,")")), "Volume m<sup>-3</sup>)", "Volume",
    "WaterDensity_kgm3", expr(paste("Water Density (kg m"^-3,")")), "Water Density (kg m<sup>-3</sup>)", "Water Density",
    "WaterDepth_m", expr(paste("Water Depth (m)")), "Water Depth (m)", "Water Depth",
    "ZoopAbund_m3", expr(paste("Zooplankton Abundance (m"^-3,")")), "Zooplankton Abundance (m<sup>-3</sup>)", "Zooplankton Abundance"),
    ncol = 4, byrow = TRUE, dimnames = list(NULL, c("Variable","gg", "pl", "simple"))))

  i <- which(relabel_df$Variable %in% s)

  if(style == "ggplot" & length(i) > 0) {
    return(relabel_df$gg[i]) # Returned as a list due to expr
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

s <- c("Temperature_degC", "WaterDensity_kgm3")
e <- pr_relabel(s, style = "simple")
ggplot2::ggplot() + ggplot2::ggtitle(enexpr(e)) + ggplot2::ylab(enexpr(e)) + ggplot2::xlab(enexpr(e))

