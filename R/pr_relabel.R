
#' Format a parameter string for plotting
#'
#' @param s A string for reformatting
#' @param style The style of plotting the string will be used for
#' @param named Should the output be an `unnamed` (FALSE) or `named` (TRUE) vector? Only available for `style = "simple"` and `style = "plotly"`
#'
#'
#' @return A reformatted expression call (ggplot)
#' @export
#'
#' @examples
#' pr_relabel("Chla_mgm3", style = "ggplot")
#'
pr_relabel <- function(s, style = "ggplot", named = FALSE){

  relabel_df <- tibble::as_tibble(matrix(c(
    "AbundancePhyto_CellsL", rlang::expr(paste("Phytoplankton Abundance (cells L"^-1,")")), "Phytoplankton Abundance (cells L<sup>-1</sup>)", "Phytoplankton Abundance",
    "Alkalinity_umolkg", rlang::expr(paste("TAlkalinity (","\U00B5","mol kg"^-1,")")), "Total Alkalinity (&#181; mol kg<sup>-1</sup>)", "Total Alkalinity",
    "Ammonium_umolL", rlang::expr(paste("Ammonium (","\U00B5","mol L"^-1,")")), "Ammonium (&#181; mol L<sup>-1</sup>)", "Ammonium",
    "AshFreeBiomass_mgm3", rlang::expr(paste("Ash Free Biomass (mg m"^-3,")")), "Ash Free Biomass (mg m<sup>-3</sup>)", "Zooplankton Ash Free Biomass",
    "AvgCellVol_um3", rlang::expr(paste("Average Cell Volume (mum"^3,"sample"^-1,")")), "Average Cell Volume (&#181;m<sup>3</sup> sample<sup>-1</sup>)", "Average Cell Volume",
    "AvgTotalLengthCopepod_mm", rlang::expr(paste("Avg. Copepod Length (mm)")), "Avg. Copepod Length (mm)", "Copepod Length",
    "Biomass_mgm3", rlang::expr(paste("Zooplankton Biomass (mg m"^-3,")")), "Zooplankton Biomass (mg m<sup>-3</sup>)", "Zooplankton Biomass",
    "BiomassIndex_mgm3", rlang::expr(paste("Biomass Index (mg m"^-3,")")), "Biomass Index (mg m<sup>-3</sup>)", "Zooplankton Biomass Index",
    "Biovolume_um3L", rlang::expr(paste("Biovolume (mum"^3," L"^-1,")")), "Biovolume (&#181;m<sup>3 L<sup>-1</sup>)", "Phytoplankton Biovolume",
    "BioVolume_um3m3", rlang::expr(paste("Biovolume (mum"^3," m"^-3,")")), "Biovolume (&#181;m<sup>3 m^-3</sup>)", "Phytoplankton Biovolume",
    "Cells_L", rlang::expr(paste("Cells (L"^-1,")")), "Cells (L<sup>-1</sup>)", "Cells",
    "chl_gsm", rlang::expr(paste("Chlorophyll \U1D44E (GSM) (mg m"^-3,")")), "Chlorophyll (GSM) <i>a</i> (mg m<sup>-3</sup>)", "Chlorophyll a",
    "chl_oc3", rlang::expr(paste("Chlorophyll \U1D44E (OC3) (mg m"^-3,")")), "Chlorophyll (OC3) <i>a</i> (mg m<sup>-3</sup>)", "Chlorophyll a",
    "Chla_mgm3", rlang::expr(paste("Chlorophyll \U1D44E (mg m"^-3,")")), "Chlorophyll <i>a</i> (mg m<sup>-3</sup>)", "Chlorophyll a",
    "ChlF_mgm3", rlang::expr(paste("Chlorophyll from fluoresence")), "Chlorophyll from fluoresence", "Chlorophyll from fluoresence",
    "Conductivity_Sm", rlang::expr(paste("Conductivity (Sm")), "Conductivity (Sm)", "Conductivity (Sm)",
    "CopeAbundance_m3", rlang::expr(paste("Copepod Abundance (m"^-3,")")), "Copepod Abundance (m<sup>-3</sup>)", "Copepod Abundance",
    "CopepodEvenness", rlang::expr(paste("Copepod Evenness")), "Copepod Evenness", "Copepod Evenness",
    "CTDChlaF_mgm3", rlang::expr(paste("Surface Chlorophyll from fluoresence")), "Surface Chlorophyll from fluoresence", "Chlorophyll from fluoresence",
    "CTDSalinity_PSU", rlang::expr(paste("Surface Salinity CTD")), "Surface Salinity CTD", "Salinity",
    "CTD_Salinity", rlang::expr(paste("CTD Salinity")), "CTD Salinity", "Salinity",
    "CTDTemperature_degC", rlang::expr(paste("Surface Temperature CTD (","\U00B0","C)")), "Surface Temperature CTD (&#8451;)", "Temperature", # binned surface temp from CTD
    "CTD_Temperature_degC", rlang::expr(paste("Temperature CTD (","\U00B0","C)")), "Temperature CTD (&#8451;)", "Temperature", # Temp from CTD
    "DCM_m", rlang::expr(paste("Deep Chlorophyll Maximum (m)")), "Deep Chlorophyll Maximum (m)", "Deep Chlorophyll Maximum (m)",
    "Density_kgm3", rlang::expr(paste("Density (kg m"^-3,")")), "Density (kg m<sup>-3</sup>)", "Density",
    "DiatomDinoflagellateRatio", rlang::expr(paste("Diatom:Dinoflagellate Ratio")), "Diatom:Dinoflagellate Ratio", "Diatom:Dinoflagellate Ratio",
    "DiatomEvenness", rlang::expr(paste("Diatom Evenness")), "Diatom Evenness", "Diatom Evenness",
    "DIC_umolkg", rlang::expr(paste("DIC (","\U00B5","mol kg"^-1,")")), "DIC (&#181; mol kg<sup>-1</sup>)", "DIC",
    'DIN_umolL', rlang::expr(paste("Dissolved Inorganic Nitrogen (","\U00B5","mol kg"^-1,")")), "Dissolved Inorganic Nitrogen (&#181; mol kg<sup>-1</sup>)", "DIN",
    "DinoflagellateEvenness", rlang::expr(paste("Dinoflagellate Evenness")), "Dinoflagellate Evenness", "Dinoflagellate Evenness",
    "DissolvedOxygen_umolkg", rlang::expr(paste("Dissolved Oxygen (","\U00B5","mol kg"^-1,")")), "Dissolved Oxygen (&#181; mol kg<sup>-1</sup>)", "Dissolved Oxygen",
    "FunctionalGroup_CellsL", rlang::expr(paste("Functional Group (cells L"^-1,")")), "Functional Group (cells L<sup>-1</sup>)", "Functional Group",
    "FunctionalGroup_m3", rlang::expr(paste("Functional Group (m"^-3,")")), "Functional Group (m<sup>-3</sup>)", "Functional Group",
    "FunctionalGroup", rlang::expr(paste("Functional Group")), "Functional Group", "Functional Group",
    "GearDepth_m", rlang::expr(paste("Gear Depth (m)")), "Gear Depth (m)", "GearDepth",
    "GearMesh_um", rlang::expr(paste("Gear Mesh (","\U00B5","m")), "Gear Mesh (&#181; m)", "GearMesh",
    "HerbivoreCarnivoreCopepodRatio", rlang::expr(paste("Copepod Herbivore:Carnivore Ratio")), "Copepod Herbivore:Carnivore Ratio", "Copepod Herbivore:Carnivore Ratio",
    "InorganicFraction_mgL", rlang::expr(paste("Inorganic Fraction (mg L"^-1,")")), "Inorganic Fraction (mg L<sup>-1</sup>)", "Inorganic Fraction",
    "Latitude", rlang::expr(paste("Latitude (","\U00B0","S)")), "Latitude (<sup>o</sup>S)", "Latitude",
    "Length_mm", rlang::expr(paste("Length (mm)")), "Length (mm)", "Length",
    "MLD_m", rlang::expr(paste("Mixed Layer Depth (m)")), "Mixed Layer Depth (m)", "Mixed Layer Depth (m)",
    "Nitrite_umolL", rlang::expr(paste("Nitrite (","\U00B5","mol L"^-1,")")), "Nitrite (&#181; mol L<sup>-1</sup>)", "Nitrite",
    "Nitrate_umolL", rlang::expr(paste("Nitrate (","\U00B5","mol L"^-1,")")), "Nitrate (&#181; mol L<sup>-1</sup>)", "Nitrate",
    "NoCopepodSpecies_Sample", rlang::expr(paste("Copepod Species (Sample "^-1,")")), "Copepod Species (Sample <sup>-1</sup>)", "Copepod Species Count",
    "NoDiatomSpecies_Sample", rlang::expr(paste("Diatom Species (Sample "^-1,")")), "Diatom Species (Sample <sup>-1</sup>)", "Diatom Species Count",
    "NoDinoSpecies_Sample", rlang::expr(paste("Dinoflagellate Species (Sample "^-1,")")), "Dinoflagellate Species (Sample <sup>-1</sup>)", "Dinoflagellate Species Count",
    "NoPhytoSpecies_Sample", rlang::expr(paste("Phytoplankton Species (Sample "^-1,")")), "Phytoplankton Species (Sample <sup>-1</sup>)", "Phytoplankton Species Count",
    "NOx_umolL", rlang::expr(paste("Nitrate + Nitrite (","\U00B5","mol L"^-1,")")), "Nitrate + Nitrite (&#181; mol L<sup>-1</sup>)", "Nitrate + Nitrite",
    "OmnivoreCarnivoreCopepodRatio", rlang::expr(paste("Omnivore:Carnivore Copepod Ratio")), "Omnivore:Carnivore Copepod Ratio", "Omnivore:Carnivore Copepod Ratio",
    "OrganicFraction_mgL", rlang::expr(paste("Organic Fraction (mg L"^-1,")")), "Organic Fraction (mg L<sup>-1</sup>)", "Organic Fraction",
    "Oxygen_umolL", rlang::expr(paste("Oxygen (","\U00B5","mol L"^-1,")")), "Oxygen (&#181; mol L<sup>-1</sup>)", "Oxygen",
    "PCI", rlang::expr(paste("Phytoplankton Colour Index (PCI)")), "Phytoplankton Colour Index (PCI)", "Phytoplankton Colour Index",
    "Phosphate_umolL", rlang::expr(paste("Phosphate (","\U00B5","mol L"^-1,")")), "Phosphate (&#181; mol L<sup>-1</sup>)", "Phosphate",
    "PhytoAbundance_CellsL", rlang::expr(paste("Phytoplankton Abundance (cells L"^-1,")")), "Phytoplankton Abundance (cells L<sup>-1</sup>)", "Phytoplankton Abundance",
    "PhytoAbundance_Cellsm3", rlang::expr(paste("Phytoplankton Abundance (cells m"^-3,")")), "Phytoplankton Abundance (cells m<sup>-3</sup>)", "Phytoplankton Abundance",
    "PhytoAbund_m3", rlang::expr(paste("Phytoplankton Abundance (m"^-3,")")), "Phytoplankton Abundance (m<sup>-3</sup>)", "Phytoplankton Abundance",
    "PhytoBiomassCarbon_pgL", rlang::expr(paste("Phytoplankton Biomass (pg C L"^-1,")")), "Phytoplankton Biomass (pg C L<sup>-1</sup>)", "Phytoplankton Biomass",
    "PhytoBiomassCarbon_pgm3", rlang::expr(paste("Phytoplankton Biomass (pg C m"^-3,")")), "Phytoplankton Biomass (pg C m<sup>-3</sup>)", "Phytoplankton Biomass",
    "PhytoEvenness", rlang::expr(paste("Phytoplankton Evenness")), "Phytoplankton Evenness", "Phytoplankton Evenness",
    "Picoeukaryotes_cellsmL", rlang::expr(paste("Picoeukaryotes (cells ml"^-1,")")), "Picoeukaryotes (cells ml<sup>-1</sup>)", "Picoeukaryote Abundance",
    "Picoplankton_Cellsml", rlang::expr(paste("Picoplankton (cells ml"^-1,")")), "Picoplankton (cells ml<sup>-1</sup>)", "Picoplankton Abundance",
    "PigmentChla_mgm3", rlang::expr(paste("Chlorophyll \U1D44E (mg m"^-3,")")), "Chlorophyll <i>a</i> (mg m<sup>-3</sup>)", "Chlorophyll a",
    "PPC", rlang::expr(paste("Photoprotective Carotenoids (mg m"^-3,")")), "Photoprotective Carotenoids (mg m<sup>-3</sup>)", "Photoprotective Carotenoid Abundance",
    "Pressure_dbar", rlang::expr(paste("Pressure (dbar)")), "Pressure (dbar)", "Pressure",
    "Prochlorococcus_cellsmL", rlang::expr(paste("Prochlorococcus (cells ml"^-1,")")), "Prochlorococcus (cells ml<sup>-1</sup>)", "Prochlorococcus Abundance",
    "PSC", rlang::expr(paste("Photosynthetic Carotenoids (mg m"^-3,")")), "Photosynthetic Carotenoids (mg m<sup>-3</sup>)", "Photosynthetic Carotenoid Abundance",
    "PSP", rlang::expr(paste("Photosynthetic Pigments (mg m"^-3,")")), "Photosynthetic Pigments (mg m<sup>-3</sup>)", "Photosynthetic Pigment Abundance",
    'Redfield', rlang::expr(paste("Redfield Ratio")), "Redfield Ratio", "Redfield",
    "Salinity", rlang::expr(paste("Salinity")), "Salinity", "Salinity",
    "SampleDate_Local", rlang::expr(paste("Sample Date (Local)")), "Sample Date (Local)", "Sample Date",
    "SampleDate_UTC", rlang::expr(paste("Sample Date (UTC)")), "Sample Date (UTC)", "Sample Date",
    "SampleTime_Local", rlang::expr(paste("Sample Date (Local)")), "Sample Date (Local)", "Sample Date",
    "SampleTime_UTC", rlang::expr(paste("Sample Date (UTC)")), "Sample Date (UTC)", "Sample Date",
    "SampleDepth_m", rlang::expr(paste("Sample Depth (m)")), "Sample Depth (m)", "Sample Depth",
    "SampVol_L", rlang::expr(paste("Sample Volume (L)")), "Sample Volume (L)", "Sample Volume",
    "SampleVolume_m3", rlang::expr(paste("Sample Volume (m"^-3,")")), "Sample Volume (m<sup>-3</sup>)", "Sample Volume",
    "SecchiDepth_m", rlang::expr(paste("Secchi Depth (m)")), "Secchi Depth (m)", "Secchi Depth",
    "Secchi_m", rlang::expr(paste("Secchi Depth (m)")), "Secchi Depth (m)", "Secchi Depth",
    "ShannonCopepodDiversity", rlang::expr(paste("Copepod Diversity")), "Copepod Diversity", "Copepod Diversity",
    "ShannonCopepodDiversityCPR", rlang::expr(paste("Copepod Diversity")), "Copepod Diversity", "Copepod Diversity",
    "ShannonDiatomDiversity", rlang::expr(paste("Diatom Diversity")), "Diatom Diversity", "Diatom Diversity",
    "ShannonDiatomDiversitycpr", rlang::expr(paste("Diatom Diversity")), "Diatom Diversity", "Diatom Diversity",
    "ShannonDinoDiversity", rlang::expr(paste("Dinoflagellate Diversity")), "Dinoflagellate Diversity", "Dinoflagellate Diversity",
    "ShannonDinoDiversitycpr", rlang::expr(paste("Dinoflagellate Diversity")), "Dinoflagellate Diversity", "Dinoflagellate Diversity",
    "ShannonPhytoDiversity", rlang::expr(paste("Phytoplankton Diversity")), "Phytoplankton Diversity", "Phytoplankton Diversity",
    "ShannonPhytoDiversitycpr", rlang::expr(paste("Phytoplankton Diversity")), "Phytoplankton Diversity", "Phytoplankton Diversity",
    "Silicate_umolL", rlang::expr(paste("Silicate (","\U00B5","mol L"^-1,")")), "Silicate (&#181; mol L<sup>-1</sup>)", "Silicate",
    "SST", rlang::expr(paste("SST (","\U00B0","C)")), "SST (&#8451;)", "SST",
    "StationCode", rlang::expr(paste("Station Code")), "Station Code", "Station Code",
    "StationName", rlang::expr(paste("Station Name")), "Station Name", "Station Name",
    "Synechococcus_cellsmL", rlang::expr(paste("Synechococcus (cells ml"^-1,")")), "Synechococcus (cells ml<sup>-1</sup>)", "Synechococcus Abundance",
    "TAcc", rlang::expr(paste("Total Accessory Pigments (mg m"^-3,")")), "Total Accessory Pigments (mg m<sup>-3</sup>)", "Total Accessory Pigments",
    "TaxonCount", rlang::expr(paste("Taxon Count")), "Taxon Count", "Taxon Count",
    "TaxonGroup", rlang::expr(paste("Taxon Group")), "Taxon Group", "Taxon Group",
    "TaxonName", rlang::expr(paste("Taxon Name")), "Taxon Name", "Taxon Name",
    "TCaro", rlang::expr(paste("Total Carotenoids (mg m"^-3,")")), "Total Carotenoids (mg m<sup>-3</sup>)", "Total Carotenoids",
    "TDP", rlang::expr(paste("Total Diagnostic Pigments (mg m"^-3,")")), "Total Diagnostic Pigments (mg m<sup>-3</sup>)", "Total Diagnostic Pigments",
    "Temperature_degC", rlang::expr(paste("Temperature (","\U00B0","C)")), "Temperature (&#8451;)", "Temperature",
    "TotalChl", rlang::expr(paste("Total Chlorophyll (mg m"^-3,")")), "Total Chlorophyll (mg m<sup>-3</sup>)", "Total Chlorophyll",
    "TotalChla", rlang::expr(paste("Total Chlorophyll \U1D44E (mg m"^-3,")")), "Total Chlorophyll <i>a</i>, (mg m<sup>-3</sup>)", "Total Chlorophyll a",
    "TowType", rlang::expr(paste("Tow Type")), "Tow Type", "Tow Type",
    "TPig", rlang::expr(paste("Total Pigments (mg m"^-3,")")), "Total Pigments (mg m<sup>-3</sup>)", "Total Pigments",
    "Trichodesmium", rlang::expr(paste("Trichodesmium Abundance (cells L"^-1,")")), "Trichodesmium Abundance (cells L<sup>-1</sup>)", "Trichodesmium Abundance",
    "TripCode", rlang::expr(paste("Trip Code")), "Trip Code", "Trip Code",
    "TSS_mgL", rlang::expr(paste("TSS (mg L"^-1,")")), "TSS (mg L<sup>-1</sup>)", "TSS",
    "Turbidity_NTU", rlang::expr(paste("Turbidity (NTU)")), "Turbidity (NTU)", "Turbidity",
    "Volume_m3", rlang::expr(paste("Volume m"^-3,")")), "Volume m<sup>-3</sup>)", "Volume",
    "WaterDensity_kgm3", rlang::expr(paste("Water Density (kg m"^-3,")")), "Water Density (kg m<sup>-3</sup>)", "Water Density",
    "WaterDepth_m", rlang::expr(paste("Water Depth (m)")), "Water Depth (m)", "Water Depth",
    "ZoopAbund_m3", rlang::expr(paste("Zooplankton Abundance (m"^-3,")")), "Zooplankton Abundance (m<sup>-3</sup>)", "Zooplankton Abundance",
    "ZoopAbundance_m3", rlang::expr(paste("Zooplankton Abundance (m"^-3,")")), "Zooplankton Abundance (m<sup>-3</sup>)", "Zooplankton Abundance",
    ## Microbial
    "NifH_genes_per_mil_reads", rlang::expr(paste("Nitrogen Fixing Genes (million reads"^-1,")")), "Nitrogen Fixing Genes (million reads<sup>-1</sup>)", "Nitrogen Fixing Genes",
    "RuBisCo_genes_per_mil_reads", rlang::expr(paste("Carbon Fixing Genes (million reads"^-1,")")), "Carbon Fixing Genes (million reads<sup>-1</sup>)", "Carbon Fixing Genes",
    "fish_parasites", rlang::expr(paste("Total Fish Parasite ASVs")), "Total Fish Parasite ASVs", "Fish Parasite ASVs",
    "nitrogen_fixation_organisms", rlang::expr(paste("Total Nitrogen Fixing ASVs")), "Total Nitrogen Fixing ASVs", "Nitrogen Fixing ASVs",
    "Bacterial_Salinity_Index_KD", rlang::expr(paste("Bacterial Salinity Index")), "Bacterial Salinity Index", "Bacterial Salinity Index",
    "Bacterial_Salinity_Index_mean", rlang::expr(paste("Bacterial Salinity Index Mean")), "Bacterial Salinity Index Mean", "Bacterial Salinity Index Mean",
    "Bacterial_Salinity_Range", rlang::expr(paste("Bacterial Salinity Range")), "Bacterial Salinity Range", "Bacterial Salinity Range",
    "Bacterial_Salinity_Proportion", rlang::expr(paste("Bacterial Salinity Proportion")), "Bacterial Salinity Proportion", "Bacterial Salinity Proportion",
    "Bacterial_Salinity_Bias", rlang::expr(paste("Bacterial Salinity Bias")), "Bacterial Salinity Bias", "Bacterial Salinity Bias",
    "Bacterial_Salinity_Diversity", rlang::expr(paste("Bacterial Salinity Diversity")), "Bacterial Salinity Diversity", "Bacterial Salinity Diversity",
    "Archaeal_Salinity_Index_KD", rlang::expr(paste("Archaeal Salinity Index")), "Archaeal Salinity Index", "Archaeal Salinity Index",
    "Archaeal_Salinity_Index_mean", rlang::expr(paste("Archaeal Salinity Index Mean")), "Archaeal Salinity Index Mean", "Archaeal Salinity Index Mean",
    "Archaeal_Salinity_Range", rlang::expr(paste("Archaeal Salinity Range")), "Archaeal Salinity Range", "Archaeal Salinity Range",
    "Archaeal_Salinity_Proportion", rlang::expr(paste("Archaeal Salinity Proportion")), "Archaeal Salinity Proportion", "Archaeal Salinity Proportion",
    "Archaeal_Salinity_Bias", rlang::expr(paste("Archaeal Salinity Bias")), "Archaeal Salinity Bias", "Archaeal Salinity Bias",
    "Archaeal_Salinity_Diversity", rlang::expr(paste("Archaeal Salinity Diversity")), "Archaeal Salinity Diversity", "Archaeal Salinity Diversity",
    "Eukaryote_Salinity_Index_KD", rlang::expr(paste("Eukaryote Salinity Index")), "Eukaryote Salinity Index", "Eukaryote Salinity Index",
    "Eukaryote_Salinity_Index_mean", rlang::expr(paste("Eukaryote Salinity Index Mean")), "Eukaryote Salinity Index Mean", "Eukaryote Salinity Index Mean",
    "Eukaryote_Salinity_Range", rlang::expr(paste("Eukaryote Salinity Range")), "Eukaryote Salinity Range", "Eukaryote Salinity Range",
    "Eukaryote_Salinity_Proportion", rlang::expr(paste("Eukaryote Salinity Proportion")), "Eukaryote Salinity Proportion", "Eukaryote Salinity Proportion",
    "Eukaryote_Salinity_Bias", rlang::expr(paste("Eukaryote Salinity Bias")), "Eukaryote Salinity Bias", "Eukaryote Salinity Bias",
    "Eukaryote_Salinity_Diversity", rlang::expr(paste("Eukaryote Salinity Diversity")), "Eukaryote Salinity Diversity", "Eukaryote Salinity Diversity",
    "Bacterial_Nitrogen_Index_KD", rlang::expr(paste("Bacterial Nitrogen Index")), "Bacterial Nitrogen Index", "Bacterial Nitrogen Index",
    "Bacterial_Nitrogen_Index_mean", rlang::expr(paste("Bacterial Nitrogen Index Mean")), "Bacterial Nitrogen Index Mean", "Bacterial Nitrogen Index Mean",
    "Bacterial_Nitrogen_Range", rlang::expr(paste("Bacterial Nitrogen Range")), "Bacterial Nitrogen Range", "Bacterial Nitrogen Range",
    "Bacterial_Nitrogen_Proportion", rlang::expr(paste("Bacterial Nitrogen Proportion")), "Bacterial Nitrogen Proportion", "Bacterial Nitrogen Proportion",
    "Bacterial_Nitrogen_Bias", rlang::expr(paste("Bacterial Nitrogen Bias")), "Bacterial Nitrogen Bias", "Bacterial Nitrogen Bias",
    "Bacterial_Nitrogen_Diversity", rlang::expr(paste("Bacterial Nitrogen Diversity")), "Bacterial Nitrogen Diversity", "Bacterial Nitrogen Diversity",
    "Archaeal_Nitrogen_Index_KD", rlang::expr(paste("Archaeal Nitrogen Index")), "Archaeal Nitrogen Index", "Archaeal Nitrogen Index",
    "Archaeal_Nitrogen_Index_mean", rlang::expr(paste("Archaeal Nitrogen Index Mean")), "Archaeal Nitrogen Index Mean", "Archaeal Nitrogen Index Mean",
    "Archaeal_Nitrogen_Range", rlang::expr(paste("Archaeal Nitrogen Range")), "Archaeal Nitrogen Range", "Archaeal Nitrogen Range",
    "Archaeal_Nitrogen_Proportion", rlang::expr(paste("Archaeal Nitrogen Proportion")), "Archaeal Nitrogen Proportion", "Archaeal Nitrogen Proportion",
    "Archaeal_Nitrogen_Bias", rlang::expr(paste("Archaeal Nitrogen Bias")), "Archaeal Nitrogen Bias", "Archaeal Nitrogen Bias",
    "Archaeal_Nitrogen_Diversity", rlang::expr(paste("Archaeal Nitrogen Diversity")), "Archaeal Nitrogen Diversity", "Archaeal Nitrogen Diversity",
    "Eukaryote_Nitrogen_Index_KD", rlang::expr(paste("Eukaryote Nitrogen Index")), "Eukaryote Nitrogen Index", "Eukaryote Nitrogen Index",
    "Eukaryote_Nitrogen_Index_mean", rlang::expr(paste("Eukaryote Nitrogen Index Mean")), "Eukaryote Nitrogen Index Mean", "Eukaryote Nitrogen Index Mean",
    "Eukaryote_Nitrogen_Range", rlang::expr(paste("Eukaryote Nitrogen Range")), "Eukaryote Nitrogen Range", "Eukaryote Nitrogen Range",
    "Eukaryote_Nitrogen_Proportion", rlang::expr(paste("Eukaryote Nitrogen Proportion")), "Eukaryote Nitrogen Proportion", "Eukaryote Nitrogen Proportion",
    "Eukaryote_Nitrogen_Bias", rlang::expr(paste("Eukaryote Nitrogen Bias")), "Eukaryote Nitrogen Bias", "Eukaryote Nitrogen Bias",
    "Eukaryote_Nitrogen_Diversity", rlang::expr(paste("Eukaryote Nitrogen Diversity")), "Eukaryote Nitrogen Diversity", "Eukaryote Nitrogen Diversity",
    "Bacterial_Phosphate_Index_KD", rlang::expr(paste("Bacterial Phosphate Index")), "Bacterial Phosphate Index", "Bacterial Phosphate Index",
    "Bacterial_Phosphate_Index_mean", rlang::expr(paste("Bacterial Phosphate Index Mean")), "Bacterial Phosphate Index Mean", "Bacterial Phosphate Index Mean",
    "Bacterial_Phosphate_Range", rlang::expr(paste("Bacterial Phosphate Range")), "Bacterial Phosphate Range", "Bacterial Phosphate Range",
    "Bacterial_Phosphate_Proportion", rlang::expr(paste("Bacterial Phosphate Proportion")), "Bacterial Phosphate Proportion", "Bacterial Phosphate Proportion",
    "Bacterial_Phosphate_Bias", rlang::expr(paste("Bacterial Phosphate Bias")), "Bacterial Phosphate Bias", "Bacterial Phosphate Bias",
    "Bacterial_Phosphate_Diversity", rlang::expr(paste("Bacterial Phosphate Diversity")), "Bacterial Phosphate Diversity", "Bacterial Phosphate Diversity",
    "Archaeal_Phosphate_Index_KD", rlang::expr(paste("Archaeal Phosphate Index")), "Archaeal Phosphate Index", "Archaeal Phosphate Index",
    "Archaeal_Phosphate_Index_mean", rlang::expr(paste("Archaeal Phosphate Index Mean")), "Archaeal Phosphate Index Mean", "Archaeal Phosphate Index Mean",
    "Archaeal_Phosphate_Range", rlang::expr(paste("Archaeal Phosphate Range")), "Archaeal Phosphate Range", "Archaeal Phosphate Range",
    "Archaeal_Phosphate_Proportion", rlang::expr(paste("Archaeal Phosphate Proportion")), "Archaeal Phosphate Proportion", "Archaeal Phosphate Proportion",
    "Archaeal_Phosphate_Bias", rlang::expr(paste("Archaeal Phosphate Bias")), "Archaeal Phosphate Bias", "Archaeal Phosphate Bias",
    "Archaeal_Phosphate_Diversity", rlang::expr(paste("Archaeal Phosphate Diversity")), "Archaeal Phosphate Diversity", "Archaeal Phosphate Diversity",
    "Eukaryote_Phosphate_Index_KD", rlang::expr(paste("Eukaryote Phosphate Index")), "Eukaryote Phosphate Index", "Eukaryote Phosphate Index",
    "Eukaryote_Phosphate_Index_mean", rlang::expr(paste("Eukaryote Phosphate Index Mean")), "Eukaryote Phosphate Index Mean", "Eukaryote Phosphate Index Mean",
    "Eukaryote_Phosphate_Range", rlang::expr(paste("Eukaryote Phosphate Range")), "Eukaryote Phosphate Range", "Eukaryote Phosphate Range",
    "Eukaryote_Phosphate_Proportion", rlang::expr(paste("Eukaryote Phosphate Proportion")), "Eukaryote Phosphate Proportion", "Eukaryote Phosphate Proportion",
    "Eukaryote_Phosphate_Bias", rlang::expr(paste("Eukaryote Phosphate Bias")), "Eukaryote Phosphate Bias", "Eukaryote Phosphate Bias",
    "Eukaryote_Phosphate_Diversity", rlang::expr(paste("Eukaryote Phosphate Diversity")), "Eukaryote Phosphate Diversity", "Eukaryote Phosphate Diversity",
    "Bacterial_Silicate_Index_KD", rlang::expr(paste("Bacterial Silicate Index")), "Bacterial Silicate Index", "Bacterial Silicate Index",
    "Bacterial_Silicate_Index_mean", rlang::expr(paste("Bacterial Silicate Index Mean")), "Bacterial Silicate Index Mean", "Bacterial Silicate Index Mean",
    "Bacterial_Silicate_Range", rlang::expr(paste("Bacterial Silicate Range")), "Bacterial Silicate Range", "Bacterial Silicate Range",
    "Bacterial_Silicate_Proportion", rlang::expr(paste("Bacterial Silicate Proportion")), "Bacterial Silicate Proportion", "Bacterial Silicate Proportion",
    "Bacterial_Silicate_Bias", rlang::expr(paste("Bacterial Silicate Bias")), "Bacterial Silicate Bias", "Bacterial Silicate Bias",
    "Bacterial_Silicate_Diversity", rlang::expr(paste("Bacterial Silicate Diversity")), "Bacterial Silicate Diversity", "Bacterial Silicate Diversity",
    "Archaeal_Silicate_Index_KD", rlang::expr(paste("Archaeal Silicate Index")), "Archaeal Silicate Index", "Archaeal Silicate Index",
    "Archaeal_Silicate_Index_mean", rlang::expr(paste("Archaeal Silicate Index Mean")), "Archaeal Silicate Index Mean", "Archaeal Silicate Index Mean",
    "Archaeal_Silicate_Range", rlang::expr(paste("Archaeal Silicate Range")), "Archaeal Silicate Range", "Archaeal Silicate Range",
    "Archaeal_Silicate_Proportion", rlang::expr(paste("Archaeal Silicate Proportion")), "Archaeal Silicate Proportion", "Archaeal Silicate Proportion",
    "Archaeal_Silicate_Bias", rlang::expr(paste("Archaeal Silicate Bias")), "Archaeal Silicate Bias", "Archaeal Silicate Bias",
    "Archaeal_Silicate_Diversity", rlang::expr(paste("Archaeal Silicate Diversity")), "Archaeal Silicate Diversity", "Archaeal Silicate Diversity",
    "Eukaryote_Silicate_Index_KD", rlang::expr(paste("Eukaryote Silicate Index")), "Eukaryote Silicate Index", "Eukaryote Silicate Index",
    "Eukaryote_Silicate_Index_mean", rlang::expr(paste("Eukaryote Silicate Index Mean")), "Eukaryote Silicate Index Mean", "Eukaryote Silicate Index Mean",
    "Eukaryote_Silicate_Range", rlang::expr(paste("Eukaryote Silicate Range")), "Eukaryote Silicate Range", "Eukaryote Silicate Range",
    "Eukaryote_Silicate_Proportion", rlang::expr(paste("Eukaryote Silicate Proportion")), "Eukaryote Silicate Proportion", "Eukaryote Silicate Proportion",
    "Eukaryote_Silicate_Bias", rlang::expr(paste("Eukaryote Silicate Bias")), "Eukaryote Silicate Bias", "Eukaryote Silicate Bias",
    "Eukaryote_Silicate_Diversity", rlang::expr(paste("Eukaryote Silicate Diversity")), "Eukaryote Silicate Diversity", "Eukaryote Silicate Diversity",
    "Bacterial_Oxygen_Index_KD", rlang::expr(paste("Bacterial Oxygen Index")), "Bacterial Oxygen Index", "Bacterial Oxygen Index",
    "Bacterial_Oxygen_Index_mean", rlang::expr(paste("Bacterial Oxygen Index Mean")), "Bacterial Oxygen Index Mean", "Bacterial Oxygen Index Mean",
    "Bacterial_Oxygen_Range", rlang::expr(paste("Bacterial Oxygen Range")), "Bacterial Oxygen Range", "Bacterial Oxygen Range",
    "Bacterial_Oxygen_Proportion", rlang::expr(paste("Bacterial Oxygen Proportion")), "Bacterial Oxygen Proportion", "Bacterial Oxygen Proportion",
    "Bacterial_Oxygen_Bias", rlang::expr(paste("Bacterial Oxygen Bias")), "Bacterial Oxygen Bias", "Bacterial Oxygen Bias",
    "Bacterial_Oxygen_Diversity", rlang::expr(paste("Bacterial Oxygen Diversity")), "Bacterial Oxygen Diversity", "Bacterial Oxygen Diversity",
    "Archaeal_Oxygen_Index_KD", rlang::expr(paste("Archaeal Oxygen Index")), "Archaeal Oxygen Index", "Archaeal Oxygen Index",
    "Archaeal_Oxygen_Index_mean", rlang::expr(paste("Archaeal Oxygen Index Mean")), "Archaeal Oxygen Index Mean", "Archaeal Oxygen Index Mean",
    "Archaeal_Oxygen_Range", rlang::expr(paste("Archaeal Oxygen Range")), "Archaeal Oxygen Range", "Archaeal Oxygen Range",
    "Archaeal_Oxygen_Proportion", rlang::expr(paste("Archaeal Oxygen Proportion")), "Archaeal Oxygen Proportion", "Archaeal Oxygen Proportion",
    "Archaeal_Oxygen_Bias", rlang::expr(paste("Archaeal Oxygen Bias")), "Archaeal Oxygen Bias", "Archaeal Oxygen Bias",
    "Archaeal_Oxygen_Diversity", rlang::expr(paste("Archaeal Oxygen Diversity")), "Archaeal Oxygen Diversity", "Archaeal Oxygen Diversity",
    "Eukaryote_Oxygen_Index_KD", rlang::expr(paste("Eukaryote Oxygen Index")), "Eukaryote Oxygen Index", "Eukaryote Oxygen Index",
    "Eukaryote_Oxygen_Index_mean", rlang::expr(paste("Eukaryote Oxygen Index Mean")), "Eukaryote Oxygen Index Mean", "Eukaryote Oxygen Index Mean",
    "Eukaryote_Oxygen_Range", rlang::expr(paste("Eukaryote Oxygen Range")), "Eukaryote Oxygen Range", "Eukaryote Oxygen Range",
    "Eukaryote_Oxygen_Proportion", rlang::expr(paste("Eukaryote Oxygen Proportion")), "Eukaryote Oxygen Proportion", "Eukaryote Oxygen Proportion",
    "Eukaryote_Oxygen_Bias", rlang::expr(paste("Eukaryote Oxygen Bias")), "Eukaryote Oxygen Bias", "Eukaryote Oxygen Bias",
    "Eukaryote_Oxygen_Diversity", rlang::expr(paste("Eukaryote Oxygen Diversity")), "Eukaryote Oxygen Diversity", "Eukaryote Oxygen Diversity",
    "Bacterial_Temperature_Index_KD", rlang::expr(paste("Bacterial Temperature Index")), "Bacterial Temperature Index", "Bacterial Temperature Index",
    "Bacterial_Temperature_Index_mean", rlang::expr(paste("Bacterial Temperature Index Mean")), "Bacterial Temperature Index Mean", "Bacterial Temperature Index Mean",
    "Bacterial_Temperature_Range", rlang::expr(paste("Bacterial Temperature Range")), "Bacterial Temperature Range", "Bacterial Temperature Range",
    "Bacterial_Temperature_Proportion", rlang::expr(paste("Bacterial Temperature Proportion")), "Bacterial Temperature Proportion", "Bacterial Temperature Proportion",
    "Bacterial_Temperature_Bias", rlang::expr(paste("Bacterial Temperature Bias")), "Bacterial Temperature Bias", "Bacterial Temperature Bias",
    "Bacterial_Temperature_Diversity", rlang::expr(paste("Bacterial Temperature Diversity")), "Bacterial Temperature Diversity", "Bacterial Temperature Diversity",
    "Archaeal_Temperature_Index_KD", rlang::expr(paste("Archaeal Temperature Index")), "Archaeal Temperature Index", "Archaeal Temperature Index",
    "Archaeal_Temperature_Index_mean", rlang::expr(paste("Archaeal Temperature Index Mean")), "Archaeal Temperature Index Mean", "Archaeal Temperature Index Mean",
    "Archaeal_Temperature_Range", rlang::expr(paste("Archaeal Temperature Range")), "Archaeal Temperature Range", "Archaeal Temperature Range",
    "Archaeal_Temperature_Proportion", rlang::expr(paste("Archaeal Temperature Proportion")), "Archaeal Temperature Proportion", "Archaeal Temperature Proportion",
    "Archaeal_Temperature_Bias", rlang::expr(paste("Archaeal Temperature Bias")), "Archaeal Temperature Bias", "Archaeal Temperature Bias",
    "Archaeal_Temperature_Diversity", rlang::expr(paste("Archaeal Temperature Diversity")), "Archaeal Temperature Diversity", "Archaeal Temperature Diversity",
    "Eukaryote_Temperature_Index_KD", rlang::expr(paste("Eukaryote Temperature Index")), "Eukaryote Temperature Index", "Eukaryote Temperature Index",
    "Eukaryote_Temperature_Index_mean", rlang::expr(paste("Eukaryote Temperature Index Mean")), "Eukaryote Temperature Index Mean", "Eukaryote Temperature Index Mean",
    "Eukaryote_Temperature_Range", rlang::expr(paste("Eukaryote Temperature Range")), "Eukaryote Temperature Range", "Eukaryote Temperature Range",
    "Eukaryote_Temperature_Proportion", rlang::expr(paste("Eukaryote Temperature Proportion")), "Eukaryote Temperature Proportion", "Eukaryote Temperature Proportion",
    "Eukaryote_Temperature_Bias", rlang::expr(paste("Eukaryote Temperature Bias")), "Eukaryote Temperature Bias", "Eukaryote Temperature Bias",
    "Eukaryote_Temperature_Diversity", rlang::expr(paste("Eukaryote Temperature Diversity")), "Eukaryote Temperature Diversity", "Eukaryote Temperature Diversity",
    "Bacteria_unique_ASVs", rlang::expr(paste("Bacterial Unique ASVs")), "Bacterial Unique ASVs", "Bacterial Unique ASVs",
    "Bacteria_shannon_index", rlang::expr(paste("Bacterial Shannon Index")), "Bacterial Shannon Index", "Bacterial Shannon Index",
    "Bacteria_simpsons_index", rlang::expr(paste("Bacterial Simpson's Index")), "Bacterial Simpson's Index", "Bacterial Simpson's Index",
    "Bacteria_invsimpson_index", rlang::expr(paste("Bacterial Inverse Simpson's Index")), "Bacterial Inverse Simpson's Index", "Bacterial Inverse Simpson's Index",
    "Bacteria_total_observations", rlang::expr(paste("Bacterial Total ASVs")), "Bacterial Total ASVs", "Bacterial Total ASVs",
    "Archaea_unique_ASVs", rlang::expr(paste("Archaeal Unique ASVs")), "Archaeal Unique ASVs", "Archaeal Unique ASVs",
    "Archaea_shannon_index", rlang::expr(paste("Archaeal Shannon Index")), "Archaeal Shannon Index", "Archaeal Shannon Index",
    "Archaea_simpsons_index", rlang::expr(paste("Archaeal Simpson's Index")), "Archaeal Simpson's Index", "Archaeal Simpson's Index",
    "Archaea_invsimpson_index", rlang::expr(paste("Archaeal Inverse Simpson's Index")), "Archaeal Inverse Simpson's Index", "Archaeal Inverse Simpson's Index",
    "Archaea_total_observations", rlang::expr(paste("Archaeal Total ASVs")), "Archaeal Total ASVs", "Archaeal Total ASVs",
    "Eukaryote_unique_ASVs", rlang::expr(paste("Eukaryote Unique ASVs")), "Eukaryote Unique ASVs", "Eukaryote Unique ASVs",
    "Eukaryote_shannon_index", rlang::expr(paste("Eukaryote Shannon Index")), "Eukaryote Shannon Index", "Eukaryote Shannon Index",
    "Eukaryote_simpsons_index", rlang::expr(paste("Eukaryote Simpson's Index")), "Eukaryote Simpson's Index", "Eukaryote Simpson's Index",
    "Eukaryote_invsimpson_index", rlang::expr(paste("Eukaryote Inverse Simpson's Index")), "Eukaryote Inverse Simpson's Index", "Eukaryote Inverse Simpson's Index",
    "Eukaryote_total_observations", rlang::expr(paste("Eukaryote Total ASVs")), "Eukaryote Total ASVs", "Eukaryote Total ASVs"),
    ncol = 4, byrow = TRUE, dimnames = list(NULL, c("Variable","gg", "pl", "simple"))))

  i <- which(relabel_df$Variable %in% s)

  if (length(i) != length(unique(s))){
    warning("Missing replacements in planktonr::pr_relabel()")
    print(unlist(relabel_df$Variable[i]))
    print(unique(s))
  }


  if(style == "ggplot" & length(i) > 0) {

    return(relabel_df$gg[[i]]) # Returned as a list due to expr

  } else if (style == "plotly" & length(i) > 0){

    if (isFALSE(named)) { # Unnamed vector
      out <- unlist(relabel_df$pl[i])
    } else { # Named vector
      out <- relabel_df$Variable[i]
      names(out) <- relabel_df$pl[i]
    }
    return(out)

  } else if (style == "simple" & length(i) > 0){

    if (isFALSE(named)){ # Unnamed vector
      out <- relabel_df$simple[i]
    } else {  # Named vector
      out <- relabel_df$Variable[i]
      names(out) <- relabel_df$simple[i]
      out <- out[order(names(out))] # Put in alpha order
    }
    return(out)
  } else if (length(i) == 1){
    return(s) # If no match, return the original string
  } else {
    return(s)
  }

}
