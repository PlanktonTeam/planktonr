# planktonr Package Streamlining Architecture

## Executive Summary

The planktonr package currently exports **67 functions** (36 data retrieval + 22 plotting + 9 utility functions). This analysis proposes a streamlined API using wrapper functions to reduce the user-facing API to approximately **30-35 core functions** while maintaining backward compatibility.

## Current State Analysis

### Exported Functions Inventory

**Data Retrieval Functions (36):**
- **CPR Survey:** `pr_get_CPRData()`, `pr_get_CPRTrips()`
- **NRS Survey:** `pr_get_NRSData()`, `pr_get_NRSTrips()`, `pr_get_NRSChemistry()`, `pr_get_NRSPigments()`, `pr_get_NRSPico()`, `pr_get_NRSMicro()`, `pr_get_NRSTSS()`, `pr_get_NRSCTD()`, `pr_get_NRSEnvContour()`
- **SOTS Survey:** `pr_get_SOTSMoorData()`, `pr_get_SOTSvariables()`
- **Coastal Seas:** `pr_get_CSChem()`
- **Indices & Metrics:** `pr_get_Indices()`, `pr_get_EOVs()`, `pr_get_CTI()`, `pr_get_STI()`, `pr_get_STIdata()`, `pr_get_FuncGroups()`, `pr_get_TaxaAccum()`, `pr_get_DayNight()`
- **Info Functions:** `pr_get_PlanktonInfo()`, `pr_get_PolicyInfo()`, `pr_get_SpeciesInfo()`, `pr_get_Stations()`
- **Map Data:** `pr_get_PCIData()`, `pr_get_FreqMap()`, `pr_get_ProgressMapData()`, `pr_get_DataLocs()`
- **Specialized:** `pr_get_LFData()`, `pr_get_LTnuts()`, `pr_get_SatData()`
- **Low-level:** `pr_get_Raw()`, `pr_get_Site()`, `pr_get_NonTaxaColumns()`

**Plotting Functions (22):**
- **Maps:** `pr_plot_CPRmap()`, `pr_plot_NRSmap()`, `pr_plot_Voyagemap()`, `pr_plot_PCImap()`, `pr_plot_FreqMap()`, `pr_plot_ProgressMap()`
- **Time Series:** `pr_plot_TimeSeries()`, `pr_plot_Trends()`, `pr_plot_Climatology()`, `pr_plot_tsclimate()`, `pr_plot_tsfg()`
- **Environmental:** `pr_plot_Enviro()`, `pr_plot_NRSEnvContour()`
- **Relationships:** `pr_plot_scatter()`, `pr_plot_box()`, `pr_plot_latitude()`
- **Specialized:** `pr_plot_Gantt()`, `pr_plot_TaxaAccum()`, `pr_plot_PieFG()`, `pr_plot_DayNight()`, `pr_plot_STI()`, `pr_plot_EOVs()`

### Key Pain Points

1. **Too many NRS functions** - 9 separate functions for different NRS data types
2. **Redundant map functions** - 6 different map plotting functions with similar structure
3. **Scattered info functions** - 3 separate info retrieval functions
4. **Confusing naming** - `pr_get_NRSData()` vs `pr_get_NRSChemistry()` etc.
5. **Discoverability issues** - Hard for users to find the right function

## Proposed Wrapper Architecture

### 1. Data Retrieval Wrapper: `pr_get_data()`

**Replace:** `pr_get_NRSData()`, `pr_get_CPRData()`, and potentially extend to other surveys

```r
pr_get_data(
  survey = c("NRS", "CPR", "SOTS"),
  type = c("Phytoplankton", "Zooplankton", "Chemistry", "Pigments", 
           "Pico", "Micro", "TSS", "CTD"),
  variable = c("abundance", "biovolume"),  # for plankton
  subset = c("raw", "htg", "genus", "species", "copepods"),
  ...
)
```

**Examples:**
```r
# Current
dat <- pr_get_NRSData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")

# Proposed
dat <- pr_get_data(survey = "NRS", type = "Phytoplankton", 
                   variable = "abundance", subset = "raw")
```

**Consolidates:**
- `pr_get_NRSData()` → `pr_get_data(survey = "NRS", type = "Phytoplankton/Zooplankton")`
- `pr_get_NRSChemistry()` → `pr_get_data(survey = "NRS", type = "Chemistry")`
- `pr_get_NRSPigments()` → `pr_get_data(survey = "NRS", type = "Pigments")`
- `pr_get_NRSPico()` → `pr_get_data(survey = "NRS", type = "Pico")`
- `pr_get_NRSMicro()` → `pr_get_data(survey = "NRS", type = "Micro")`
- `pr_get_NRSTSS()` → `pr_get_data(survey = "NRS", type = "TSS")`
- `pr_get_NRSCTD()` → `pr_get_data(survey = "NRS", type = "CTD")`
- `pr_get_CPRData()` → `pr_get_data(survey = "CPR", type = "Phytoplankton/Zooplankton")`

### 2. Map Plotting Wrapper: `pr_plot_map()`

**Replace:** All map plotting functions with single interface

```r
pr_plot_map(
  data = NULL,  # optional data input
  map_type = c("stations", "bioregions", "voyage", "frequency", "progress", "PCI"),
  survey = c("NRS", "CPR", "SOTS"),
  sites = NULL,  # for station highlighting
  species = NULL,  # for frequency maps
  interactive = FALSE,
  ...
)
```

**Examples:**
```r
# Current
pr_plot_NRSmap(sites = c("MAI", "PHB"), Survey = "NRS")

# Proposed
pr_plot_map(map_type = "stations", survey = "NRS", sites = c("MAI", "PHB"))

# Current
pr_plot_CPRmap(sites = c("South-east", "Temperate East"))

# Proposed  
pr_plot_map(map_type = "bioregions", survey = "CPR", 
            sites = c("South-east", "Temperate East"))
```

**Consolidates:**
- `pr_plot_NRSmap()` → `pr_plot_map(map_type = "stations", survey = "NRS")`
- `pr_plot_CPRmap()` → `pr_plot_map(map_type = "bioregions", survey = "CPR")`
- `pr_plot_Voyagemap()` → `pr_plot_map(map_type = "voyage")`
- `pr_plot_PCImap()` → `pr_plot_map(map_type = "PCI")`
- `pr_plot_FreqMap()` → `pr_plot_map(map_type = "frequency")`
- `pr_plot_ProgressMap()` → `pr_plot_map(map_type = "progress")`

### 3. Information Wrapper: `pr_get_info()`

**Replace:** Multiple info functions with unified interface

```r
pr_get_info(
  type = c("plankton", "policy", "species", "stations"),
  survey = NULL,  # for policy/stations
  taxa = NULL,    # for species info
  ...
)
```

**Examples:**
```r
# Current
pr_get_PlanktonInfo(Type = "Zooplankton")

# Proposed
pr_get_info(type = "plankton", taxa = "Zooplankton")

# Current
pr_get_PolicyInfo(Survey = "NRS")

# Proposed
pr_get_info(type = "policy", survey = "NRS")
```

**Consolidates:**
- `pr_get_PlanktonInfo()` → `pr_get_info(type = "plankton")`
- `pr_get_PolicyInfo()` → `pr_get_info(type = "policy")`
- `pr_get_SpeciesInfo()` → `pr_get_info(type = "species")`
- `pr_get_Stations()` → `pr_get_info(type = "stations")`

### 4. Additional Proposed Wrappers

#### 4a. Trip Metadata: `pr_get_trips()`

```r
pr_get_trips(survey = c("NRS", "CPR", "SOTS"), ...)
```

**Consolidates:**
- `pr_get_NRSTrips()` → `pr_get_trips(survey = "NRS")`
- `pr_get_CPRTrips()` → `pr_get_trips(survey = "CPR")`

#### 4b. Satellite Data: Keep as is
`pr_get_SatData()` - already well-designed with survey parameter

#### 4c. Environmental Plotting: `pr_plot_enviro()`

```r
pr_plot_enviro(
  data,
  plot_type = c("depth_profile", "contour", "time_series", "trends", "climatology"),
  ...
)
```

**Consolidates:**
- `pr_plot_Enviro()` → `pr_plot_enviro(plot_type = "depth_profile")`
- `pr_plot_NRSEnvContour()` → `pr_plot_enviro(plot_type = "contour")`
- `pr_plot_TimeSeries()` → `pr_plot_enviro(plot_type = "time_series")`
- `pr_plot_Trends()` → `pr_plot_enviro(plot_type = "trends")`
- `pr_plot_Climatology()` → `pr_plot_enviro(plot_type = "climatology")`

## Implementation Strategy

### Phase 1: Core Wrappers (Priority: HIGH)

1. **`pr_get_data()`** - Highest impact consolidation
   - Consolidates 8 NRS data functions + CPR
   - Most frequently used functions
   - Clear user benefit

2. **`pr_plot_map()`** - Visual consistency
   - Consolidates 6 map functions
   - Improves user experience in Shiny apps
   - Easy to implement

3. **`pr_get_info()`** - Logical grouping
   - Consolidates 4 info functions
   - Clear conceptual grouping

### Phase 2: Extended Wrappers (Priority: MEDIUM)

4. **`pr_get_trips()`** - Simple consolidation
   - Only 2 functions to merge
   - Similar signatures

5. **`pr_plot_enviro()`** - Complex but valuable
   - Consolidates 5 plotting functions
   - Requires careful parameter design

### Phase 3: Specialized Functions (Priority: LOW)

Keep specialized functions as-is (already well-designed):
- `pr_get_Indices()`
- `pr_get_EOVs()`
- `pr_get_FuncGroups()`
- `pr_plot_Gantt()`
- `pr_plot_TaxaAccum()`
- etc.

## Backward Compatibility Strategy

### Deprecation Timeline

**Version 0.7.0 (Next Major Release):**
- Introduce all wrapper functions
- Keep old functions working with deprecation warnings
- Add documentation showing migration path

**Version 0.8.0 (6 months later):**
- Increase warning visibility
- Update all vignettes to use new API

**Version 1.0.0 (12 months later):**
- Remove deprecated functions OR
- Keep as simple aliases indefinitely (preferred for scientific software)

### Deprecation Warning Template

```r
pr_get_NRSChemistry <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSChemistry()",
    with = "pr_get_data(survey = 'NRS', type = 'Chemistry')"
  )
  pr_get_data(survey = "NRS", type = "Chemistry")
}
```

### Documentation Migration

1. **New function docs:** Comprehensive with examples
2. **Old function docs:** Add deprecation notice with migration example
3. **Vignettes:** Update all to use new API
4. **NEWS.md:** Clear migration guide
5. **Website:** Add migration guide article

## Detailed Design: `pr_get_data()`

### Function Signature

```r
pr_get_data <- function(
  survey = c("NRS", "CPR", "SOTS"),
  type = c("Phytoplankton", "Zooplankton", "Chemistry", "Pigments", 
           "Pico", "Micro", "TSS", "CTD", "LarvalFish"),
  variable = NULL,  # for plankton: "abundance", "biovolume"
  subset = NULL,    # for plankton: "raw", "htg", "genus", "species", "copepods"
  format = "all",   # for pigments: "all", "binned"
  ...
) {
  # Input validation
  survey <- match.arg(survey)
  type <- match.arg(type)
  
  # Route to appropriate function based on survey + type
  if (survey == "NRS") {
    switch(type,
      "Phytoplankton" = ,
      "Zooplankton" = pr_get_NRSData(Type = type, Variable = variable, Subset = subset),
      "Chemistry" = pr_get_NRSChemistry(),
      "Pigments" = pr_get_NRSPigments(Format = format),
      "Pico" = pr_get_NRSPico(),
      "Micro" = pr_get_NRSMicro(...),
      "TSS" = pr_get_NRSTSS(),
      "CTD" = pr_get_NRSCTD(),
      "LarvalFish" = pr_get_LFData(),
      stop("Invalid type for NRS survey")
    )
  } else if (survey == "CPR") {
    # CPR routing logic
  } else if (survey == "SOTS") {
    # SOTS routing logic
  }
}
```

### Parameter Logic

| Survey | Type | Requires `variable` | Requires `subset` | Requires `format` |
|--------|------|---------------------|-------------------|-------------------|
| NRS | Phytoplankton | Yes | Yes | No |
| NRS | Zooplankton | Yes | Yes | No |
| NRS | Chemistry | No | No | No |
| NRS | Pigments | No | No | Yes |
| NRS | Pico | No | No | No |
| NRS | Micro | No | No | No |
| NRS | TSS | No | No | No |
| NRS | CTD | No | No | No |
| CPR | Phytoplankton | Yes | Yes | No |
| CPR | Zooplankton | Yes | Yes | No |

## Detailed Design: `pr_plot_map()`

### Function Signature

```r
pr_plot_map <- function(
  data = NULL,
  map_type = c("stations", "bioregions", "voyage", "frequency", "progress", "PCI"),
  survey = NULL,
  sites = NULL,
  species = NULL,
  type = NULL,  # for NRS: "Phytoplankton" or "Zooplankton"
  interactive = FALSE,
  labels = TRUE,
  country = c("Australia"),
  ...
) {
  map_type <- match.arg(map_type)
  
  switch(map_type,
    "stations" = {
      if (is.null(survey)) stop("'survey' required for stations map")
      pr_plot_NRSmap(sites = sites, Survey = survey, Type = type %||% "Zooplankton")
    },
    "bioregions" = {
      pr_plot_CPRmap(sites = sites)
    },
    "voyage" = {
      if (is.null(data)) stop("'data' required for voyage map")
      pr_plot_Voyagemap(dat = data, dats = data %>% slice(1:5000), Country = country)
    },
    "frequency" = {
      if (is.null(data) || is.null(species)) 
        stop("'data' and 'species' required for frequency map")
      pr_plot_FreqMap(dat = data, species = species, interactive = interactive)
    },
    "progress" = {
      if (is.null(data)) stop("'data' required for progress map")
      pr_plot_ProgressMap(dat = data, interactive = interactive, labels = labels)
    },
    "PCI" = {
      if (is.null(data)) stop("'data' required for PCI map")
      pr_plot_PCImap(dat = data)
    }
  )
}
```

## Benefits Analysis

### User Benefits

1. **Reduced cognitive load** - Fewer functions to remember
2. **Consistent API** - Similar patterns across functions
3. **Better discoverability** - Autocomplete shows related options
4. **Clearer documentation** - Consolidated help pages
5. **Easier onboarding** - Simpler mental model

### Developer Benefits

1. **Easier maintenance** - Changes in one place
2. **Better testing** - Centralized validation
3. **Consistent error handling** - Single validation logic
4. **Flexible extension** - Easy to add new types/surveys

### Package Benefits

1. **Smaller NAMESPACE** - ~30 exports instead of 67
2. **Better documentation** - Comprehensive help pages
3. **Modern R practices** - Follows tidyverse patterns
4. **Scientific reproducibility** - Clear, consistent API

## Risks and Mitigation

### Risk 1: Breaking Changes

**Mitigation:**
- Maintain old functions as aliases indefinitely
- Use lifecycle package for proper deprecation
- Provide clear migration guide

### Risk 2: Increased Complexity

**Mitigation:**
- Keep wrapper logic simple (just routing)
- Comprehensive documentation with examples
- Clear error messages

### Risk 3: Performance

**Mitigation:**
- Wrappers add negligible overhead (just routing)
- Actual work done by existing functions
- No performance impact expected

### Risk 4: User Confusion

**Mitigation:**
- Extensive documentation
- Migration guide in vignettes
- Deprecation warnings with helpful messages
- Blog post announcing changes

## Documentation Plan

### New Documentation Needed

1. **Migration Guide** (vignette)
   - Old → New function mapping
   - Code examples for common use cases
   - Troubleshooting section

2. **Wrapper Function Help Pages**
   - Comprehensive parameter descriptions
   - Multiple examples per function
   - See Also sections linking related functions

3. **Update Existing Vignettes**
   - Replace all deprecated function calls
   - Show new API patterns
   - Highlight benefits

4. **NEWS.md Entry**
   - List all changes
   - Migration instructions
   - Links to detailed guide

## Testing Strategy

1. **Unit Tests for Wrappers**
   - Test all routing logic
   - Verify parameter passing
   - Check error messages

2. **Integration Tests**
   - Verify old functions still work
   - Test deprecation warnings
   - Validate output consistency

3. **Regression Tests**
   - Compare old vs new function outputs
   - Ensure identical results

4. **Documentation Tests**
   - All examples must run
   - Verify cross-references

## Implementation Checklist

### Phase 1: Preparation
- [ ] Create feature branch
- [ ] Review this plan with stakeholders
- [ ] Gather user feedback on proposed API
- [ ] Finalize wrapper signatures

### Phase 2: Core Development
- [ ] Implement `pr_get_data()`
- [ ] Implement `pr_plot_map()`
- [ ] Implement `pr_get_info()`
- [ ] Add comprehensive tests
- [ ] Update documentation

### Phase 3: Deprecation
- [ ] Add deprecation warnings to old functions
- [ ] Create migration guide vignette
- [ ] Update all existing vignettes
- [ ] Update README examples

### Phase 4: Release
- [ ] Update NEWS.md
- [ ] Create pkgdown site article
- [ ] Announce on social media/blog
- [ ] Monitor issue tracker for problems

### Phase 5: Future
- [ ] Implement Phase 2 wrappers
- [ ] Consider removing deprecated functions (v1.0.0)
- [ ] Continuous refinement based on feedback

## Recommendation

**PROCEED with Phase 1 implementation:**

1. Start with `pr_get_data()` - highest impact
2. Follow with `pr_plot_map()` - clear user benefit  
3. Add `pr_get_info()` - logical grouping

Keep existing functions as indefinite aliases for scientific software stability. Add lifecycle deprecation warnings but don't remove old functions.

**Timeline:**
- Phase 1 implementation: 2-3 weeks
- Testing and documentation: 1 week
- Beta testing with users: 2 weeks
- Release v0.7.0: Target Q1 2025

## Questions for Stakeholders

1. Do the proposed wrapper signatures make sense for your use cases?
2. Are there any other function groups that should be consolidated?
3. Should we keep deprecated functions indefinitely or remove after v1.0.0?
4. Are there any backward compatibility concerns we haven't addressed?
5. Would you prefer a more aggressive or conservative deprecation timeline?

---

*This architecture document is a living document and should be updated as implementation progresses and feedback is received.*