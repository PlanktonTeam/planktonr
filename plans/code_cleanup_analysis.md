# planktonr Package: Comprehensive Code Cleanup Analysis

**Date**: 2025-12-12  
**Package Version**: 0.6.7  
**Review Type**: Comprehensive code quality, architecture, and cleanup assessment

---

## Executive Summary

The `planktonr` package is a well-structured R package for accessing and analyzing biological oceanography data from IMOS. The codebase is generally well-documented with excellent roxygen2 documentation. However, there are several areas that need attention for improved maintainability, consistency, and code quality.

**Overall Assessment**: The package is functional and production-ready but would benefit from cleanup to improve long-term maintainability.

**Key Strengths**:
- Excellent roxygen2 documentation with detailed parameter descriptions
- Consistent use of modern tidyverse patterns
- Well-implemented S3 class system (`planktonr_dat`)
- Comprehensive input validation using `assertthat`
- Clear separation of concerns (utils, plotting, data retrieval functions)
- Good test infrastructure setup

**Priority Areas for Improvement**:
1. Resolve 18 TODO comments (some critical)
2. Clean up commented-out code
3. Address potential bugs in edge cases
4. Improve consistency in some patterns
5. Reduce function complexity in plotting functions

---

## 1. TODO Comments Analysis (18 found)

### CRITICAL (Action Required)

#### 1.1 Data Quality Issues

**Location**: [`R/utils_bgc.R:64`](R/utils_bgc.R:64)
```r
# TODO Check if it is Alk or Total Alk. Our code used to say Total Alk.
```
- **Impact**: Documentation accuracy
- **Action**: Verify with data provider whether it's Alkalinity or Total Alkalinity
- **Priority**: Medium

**Location**: [`R/utils_bgc.R:530`](R/utils_bgc.R:530)
```r
#TODO Check with Claire
```
- **Impact**: Data filtering accuracy
- **Action**: Verify why specific CTD file IDs are filtered out
- **Priority**: High - affects data integrity

#### 1.2 Data Processing Issues

**Location**: [`R/utils_bgc.R:464`](R/utils_bgc.R:464)
```r
# TODO these should fixed soon
```
- **Impact**: Data completeness
- **Action**: Coordinate with data providers to fix upstream data issues
- **Priority**: Medium

**Location**: [`R/utils_bgc.R:529`](R/utils_bgc.R:529)
```r
# TODO flags are small f, make function work with both
```
- **Impact**: QC flag application robustness
- **Action**: Update [`pr_apply_Flags()`](R/utils.R:465) to handle both uppercase and lowercase flag column names
- **Priority**: High

#### 1.3 Temporary Fixes

**Location**: [`R/utils.R:113`](R/utils.R:113)
```r
# TODO Temporarily rename Bonney Coast. Can be removed when datasets are updated. Likely after 21 March
```
- **Impact**: Data consistency
- **Action**: Check if upstream data has been updated; if yes, remove this workaround
- **Priority**: Medium - check if date has passed

**Location**: [`R/utils_nrs.R:116`](R/utils_nrs.R:116)
```r
#TODO - once we get rid of SOTS_RAS we can delete this
```
- **Impact**: Code simplification
- **Action**: Once SOTS_RAS is fully deprecated, remove this conditional logic
- **Priority**: Low

**Location**: [`R/plot_timeseries.R:556`](R/plot_timeseries.R:556)
```r
#TODO Temp fix to convert to date and fix ticks below
```
- **Impact**: Plot formatting
- **Action**: Implement proper date handling in climatology plots
- **Priority**: Low

### ARCHITECTURAL DECISIONS

#### 1.4 Model Handling

**Location**: [`R/plot_timeseries.R:278`](R/plot_timeseries.R:278) and [`R/plot_timeseries.R:288`](R/plot_timeseries.R:288)
```r
# TODO Decide if we will always provide models or not. Currently we are forcing models to be run and shown.
```
- **Impact**: API design
- **Action**: Document decision and make behavior consistent
- **Priority**: Medium

**Location**: [`R/plot_timeseries.R:1033-1039`](R/plot_timeseries.R:1033)
```r
# TODO At the moment, the model parameters are still being put into columns.
# I want to change this to be a model object in a slot that can be extracted by
# a generic summary.planktonr_dat() call. I will need a different model object per variable.
# It should update each time it is plotted and subset so it is accuate for the data contained
# in the data frame. Consider using tidymodels to do it.

# TODO Should I be only processing 1 station/Bioregion?
```
- **Impact**: Architecture design
- **Action**: This is a significant architectural decision. Consider implementing tidymodels approach for model storage
- **Priority**: Low (future enhancement)

### ENHANCEMENTS

#### 1.5 Performance Improvements

**Location**: [`R/utils_plankton.R:482`](R/utils_plankton.R:482)
```r
#TODO quicker to change to Local now that it exists.
```
- **Impact**: Performance
- **Action**: Use local time directly instead of UTC conversion in day/night calculations
- **Priority**: Low

**Location**: [`R/utils_sat.R:153`](R/utils_sat.R:153)
```r
#TODO add progress bars with purrr
```
- **Impact**: User experience
- **Action**: Add progress bars for long-running satellite data downloads
- **Priority**: Low (nice-to-have)

#### 1.6 Minor Cleanups

**Location**: [`R/pr_rename.R:13`](R/pr_rename.R:13)
```r
##TODO - Check and remvoe any uneeded renames - note the capitals are needed for files grabbed from S3
```
- **Impact**: Code maintainability
- **Action**: Audit rename mappings and remove unused ones
- **Priority**: Low
- **Note**: Typo "remvoe" → "remove"

**Location**: [`R/utils.R:384`](R/utils.R:384)
```r
#TODO - managing order when NRS and SOTS combined, is there a better way.
```
- **Impact**: Code clarity
- **Action**: Refactor station ordering logic for NRS+SOTS cases
- **Priority**: Low

**Location**: [`R/utils_indices.R:162`](R/utils_indices.R:162)
```r
#TODO added for consistency but uses etc timezones - do we changes these to the more familiar names or leave?
```
- **Impact**: User experience
- **Action**: Decide on timezone naming convention (ETC vs familiar names)
- **Priority**: Low

**Location**: [`R/plot_timeseries.R:1007`](R/plot_timeseries.R:1007)
```r
# TODO Could make this an argument.
```
- **Impact**: API flexibility
- **Action**: Consider making point size a parameter in [`pr_plot_EOVs()`](R/plot_timeseries.R:974)
- **Priority**: Low

**Location**: [`R/utils_larvalfish.R:222`](R/utils_larvalfish.R:222)
```r
#TODO
# Non-interactive not available at the moment
```
- **Impact**: Feature completeness
- **Action**: Implement non-interactive mode for larval fish plotting (if needed)
- **Priority**: Low

---

## 2. Commented-Out Code Analysis



### 2.2 Commented Debugging/Development Code

**Locations with commented optional messages**:
- [`R/planktonr_dat.R:69, 77, 84`](R/planktonr_dat.R:69) - Optional user messages about coercion
  - **Action**: Keep as optional debugging aids

**Locations with commented code alternatives**:
- [`R/utils_sat.R:335-336, 474-481`](R/utils_sat.R:335) - Default parameter messages
  - **Action**: Remove if not needed, or implement as warnings



---

## 3. Potential Bugs and Edge Cases

### 3.1 Critical Issues

#### Issue 1: Inconsistent depth handling for SOTS data
**Location**: Multiple functions in plotting and analysis
**Problem**: SOTS data has variable depths (0-20m and 20-34.5m) but not all functions handle this consistently
**Evidence**:
- [`pr_plot_Trends()`](R/plot_timeseries.R:210) handles deeper samples
- [`pr_model_data()`](R/utils_analysis.R:62) warns but doesn't account for depth
- Some functions average across depths, others don't

**Impact**: Potential data misinterpretation
**Recommendation**: 
- Add explicit depth handling parameter
- Document depth aggregation behavior
- Add validation warnings when depth varies


### 3.2 Data Quality Issues

#### Issue 3: Potential integer overflow in outlier removal
**Location**: [`R/utils.R:587-630`](R/utils.R:587)
**Problem**: In [`pr_remove_outliers()`](R/utils.R:587), the check at line 623:
```r
if(unique(added$SampleDepth_m == "integrated")){
```
Should probably be:
```r
if(all(added$SampleDepth_m == "integrated")){
```
**Impact**: Logic error if multiple depth values exist
**Recommendation**: Use `all()` instead of `unique()`

#### Issue 4: Hardcoded file filtering without explanation
**Location**: [`R/utils_bgc.R:530`](R/utils_bgc.R:530)
```r
dplyr::filter(!.data$file_id %in% c(2117, 2184, 2186, 2187))
```
**Impact**: Data completeness - unclear why these files are excluded
**Recommendation**: Add comment explaining why these specific files are filtered

---

## 4. Code Style and Consistency Issues


### 4.2 Inconsistent Commenting Style

**Issues**:
- Mix of `#` and `##` for comments
- Inconsistent roxygen2 formatting
- Some inline comments too verbose, others too terse

**Recommendation**: Establish and enforce commenting guidelines

### 4.3 Variable Naming Inconsistency

**Examples**:
- Station identifiers: `StationCode`, `StationName`, `site`, `Station`

**Recommendation**: Choose consistent conventions and apply throughout

---

## 5. Function Complexity Analysis

### 5.1 High Complexity Functions (>100 lines)

1. **[`pr_plot_EOVs()`](R/plot_timeseries.R:974)** - 213 lines
   - Creates three-panel EOV plots
   - **Recommendation**: Split into helper functions for each panel
   
2. **[`pr_plot_Trends()`](R/plot_timeseries.R:210)** - 202 lines
   - Handles multiple trend types and edge cases
   - **Recommendation**: Extract model handling and SOTS logic into helpers

3. **[`pr_plot_Climatology()`](R/plot_timeseries.R:469)** - 138 lines
   - Multiple conditional branches
   - **Recommendation**: Simplify conditional logic

4. **[`pr_plot_tsfg()`](R/plot_timeseries.R:733)** - 169 lines
   - Complex data transformation and plotting
   - **Recommendation**: Extract data transformation steps

5. **[`pr_relabel()`](R/pr_relabel.R:15)** - 305 lines (mostly data)
   - Large lookup table embedded in function
   - **Recommendation**: Move lookup table to data file or sysdata

### 5.2 Deeply Nested Conditionals

**Locations**:
- [`pr_plot_Trends()`](R/plot_timeseries.R:210) - nested if/else for survey types and trends
- [`pr_plot_tsfg()`](R/plot_timeseries.R:733) - nested conditionals for plot configuration
- [`pr_reorder()`](R/utils.R:380) - multiple survey-specific blocks

**Recommendation**: Refactor using lookup tables or strategy pattern where appropriate

---

## 6. Documentation Issues

### 6.1 Excellent Documentation

Most functions have:
- ✅ Complete roxygen2 headers
- ✅ Parameter descriptions
- ✅ Return value documentation
- ✅ Examples
- ✅ Detailed details sections
- ✅ Cross-references with `@seealso`

### 6.2 Areas Needing Improvement

1. **Missing `@returns` tags**: Some functions use older `@return` instead of `@returns`
   - **Action**: Standardize on `@returns` (current roxygen2 convention)

2. **Vignette coverage**: Only one main vignette mentioned in README
   - **Status**: README lists 6 vignettes, need to verify all exist
   - **Action**: Ensure all mentioned vignettes are included

3. **Internal function documentation**:
   - Helper functions like [`pr_check_type()`](R/planktonr_dat.R:167) lack roxygen2 headers
   - **Action**: Add `@noRd` and basic description for internal functions

4. **Package-level documentation**: 
   - No `package-level.R` file with `@keywords internal`
   - **Action**: Add package-level documentation

---

## 7. Testing Recommendations

### 7.1 Current State

- Test infrastructure exists ([`tests/testthat.R`](tests/testthat.R))
- Package has codecov integration
- Need to review actual test coverage

### 7.2 Priority Areas for Testing

1. **S3 Method Tests**:
   - Test all verb methods (filter, mutate, etc.) preserve attributes
   - Test `planktonr_dat` constructor edge cases
   
2. **Data Retrieval Functions**:
   - Test error handling for network failures
   - Test data structure consistency

3. **Input Validation**:
   - Test all validation messages are triggered correctly
   - Test edge cases (NULL, NA, empty dataframes)

4. **Plotting Functions**:
   - Test plot generation doesn't error
   - Test parameter combinations

5. **Critical Calculations**:
   - Test outlier removal logic
   - Test model fitting
   - Test index calculations

---

## 8. Architecture Considerations

### 8.1 Strengths

1. **S3 Class System**: Well-implemented `planktonr_dat` class
   - Properly extends tibble
   - Preserves attributes through dplyr/tidyr operations
   - Good use of generics

2. **Function Organization**: Logical file structure
   - `utils_*.R` - data retrieval and manipulation
   - `plot_*.R` - visualization functions
   - `pr_*.R` - package-specific operations

3. **Dependency Management**: Good use of modern tidyverse packages

### 8.2 Potential Improvements

1. **Model Storage**: Consider implementing tidymodels approach for model objects (as noted in TODO)

2. **Data Caching**: Consider implementing caching for remote data access to improve performance

3. **Progressbars**: Add progress indicators for long-running operations (satellite data, large downloads)

4. **Parallel Processing**: Document and optimize use of future/furrr for parallel processing

---

## 9. Priority Matrix

### High Priority (Do First)

| Issue | Location | Impact | Effort |
|-------|----------|--------|--------|
| Fix missing return in `pr_get_CPRTrips()` | [`R/utils_cpr.R:162`](R/utils_cpr.R:162) | High | Low |
| Fix outlier removal logic | [`R/utils.R:623`](R/utils.R:623) | High | Low |
| Document CTD file filtering | [`R/utils_bgc.R:530`](R/utils_bgc.R:530) | High | Low |
| Fix flag handling for lowercase | [`R/utils_bgc.R:529`](R/utils_bgc.R:529) | High | Medium |
| Add input validation to key functions | Various | Medium | Medium |
| Resolve all high-priority TODOs | Various | Medium | Medium |

### Medium Priority (Do Soon)

| Issue | Location | Impact | Effort |
|-------|----------|--------|--------|
| Clean up all commented code | Various | Medium | Medium |
| Verify "Bonney Coast" workaround still needed | [`R/utils.R:113`](R/utils.R:113) | Medium | Low |
| Standardize variable naming | Throughout | Medium | High |
| Add package-level documentation | New file needed | Medium | Low |
| Decide on model forcing behavior | [`R/plot_timeseries.R:278,288`](R/plot_timeseries.R:278) | Medium | Low |

### Low Priority (Nice to Have)

| Issue | Location | Impact | Effort |
|-------|----------|--------|--------|
| Refactor complex plotting functions | Various | Low | High |
| Move `pr_relabel` lookup to data | [`R/pr_relabel.R`](R/pr_relabel.R) | Low | Medium |
| Add progress bars | [`R/utils_sat.R`](R/utils_sat.R) | Low | Medium |
| Implement tidymodels approach | Architecture | Low | High |
| Add non-interactive plotting mode | [`R/utils_larvalfish.R`](R/utils_larvalfish.R) | Low | Medium |

---

## 10. Recommended Cleanup Workflow

### Phase 1: Critical Fixes (Week 1)
1. ✅ Fix missing return in `pr_get_CPRTrips()`
2. ✅ Fix outlier removal logic
3. ✅ Add missing input validation
4. ✅ Resolve high-priority TODOs
5. ✅ Document why specific CTD files are filtered

### Phase 2: Code Cleanup (Week 2)
1. ✅ Remove or implement all commented-out functions
2. ✅ Clean up debugging comments
3. ✅ Standardize error messages
4. ✅ Verify temporary workarounds still needed
5. ✅ Add package-level documentation

### Phase 3: Consistency Improvements (Week 3)
1. ✅ Standardize variable naming conventions
2. ✅ Apply consistent input validation pattern
3. ✅ Standardize commenting style
4. ✅ Update all `@return` to `@returns`
5. ✅ Review and improve function signatures

### Phase 4: Refactoring (Week 4+)
1. ✅ Split complex plotting functions
2. ✅ Simplify nested conditionals
3. ✅ Extract lookup tables to data
4. ✅ Optimize performance bottlenecks
5. ✅ Expand test coverage

---

## 11. Specific Code Examples

### Example 1: Fix Missing Return

**Current** ([`R/utils_cpr.R:156`](R/utils_cpr.R:156)):
```r
pr_get_CPRTrips <- function(...){
  CPRTrips <- pr_get_s3("cpr_samp") %>%
    planktonr_dat(Type = NULL, Survey = "CPR", Variable = NULL) %>%
    pr_rename() %>%
    pr_add_Bioregions(...) %>%
    pr_apply_Time()
}
```

**Fixed**:
```r
pr_get_CPRTrips <- function(...){
  CPRTrips <- pr_get_s3("cpr_samp") %>%
    planktonr_dat(Type = NULL, Survey = "CPR", Variable = NULL) %>%
    pr_rename() %>%
    pr_add_Bioregions(...) %>%
    pr_apply_Time()
  
  return(CPRTrips)
}
```

### Example 2: Improve Outlier Logic

**Current** ([`R/utils.R:623`](R/utils.R:623)):
```r
if(unique(added$SampleDepth_m == "integrated")){
  added <- added %>% dplyr::select(-"SampleDepth_m")
}
```

**Fixed**:
```r
if(all(added$SampleDepth_m == "integrated")){
  added <- added %>% dplyr::select(-"SampleDepth_m")
}
```

### Example 3: Add Input Validation

**Current** ([`R/utils.R:587`](R/utils.R:587)):
```r
pr_remove_outliers <- function(df, x){
  Survey <- pr_get_survey(df)
  Type <- pr_get_type(df)
  # ... rest of function
}
```

**Improved**:
```r
pr_remove_outliers <- function(df, x){
  # Input validation
  assertthat::assert_that(
    inherits(df, "planktonr_dat"),
    msg = "'df' must be a planktonr_dat object."
  )
  
  assertthat::assert_that(
    is.numeric(x) && length(x) == 1 && x > 0,
    msg = "'x' must be a single positive number specifying standard deviations."
  )
  
  Survey <- pr_get_survey(df)
  Type <- pr_get_type(df)
  # ... rest of function
}
```

---

## 12. Long-term Recommendations

### 12.1 Testing Strategy
- Aim for >80% code coverage
- Focus on critical calculation functions
- Add integration tests for data retrieval
- Test all plotting functions for errors

### 12.2 Documentation
- Create contributing guide
- Add developer documentation
- Create troubleshooting guide
- Expand vignettes with real-world examples

### 12.3 Performance
- Profile satellite data functions
- Implement caching where appropriate
- Add parallelization support documentation
- Optimize large dataframe operations

### 12.4 User Experience
- Add progress bars to long operations
- Improve error messages with actionable advice
- Create quick-start guide
- Add "common workflows" vignette

---

## 13. Summary of Findings

### Code Quality Score: 7.5/10

**Breakdown**:
- Documentation: 9/10 ⭐
- Code Organization: 8/10 ⭐
- Input Validation: 7/10
- Testing: 6/10 (needs assessment)
- Consistency: 7/10
- Maintainability: 7/10

### Immediate Actions Required:
1. Fix `pr_get_CPRTrips()` return value
2. Fix outlier removal logic
3. Resolve high-priority TODOs
4. Add missing input validation

### No Critical Blocking Issues Found ✅

The package is production-ready but would benefit from the cleanup activities outlined above.

---

## Appendix A: Complete File Inventory

### R Source Files (27 files)
- data.R
- planktonr_dat.R ⭐ (core S3 class)
- plot_enviro.R
- plot_info.R
- plot_maps.R
- plot_relationships.R
- plot_timeseries.R ⭐ (most complex)
- pr_add_bioregions.R
- pr_export_larvalfish.R
- pr_relabel.R ⭐ (large lookup table)
- pr_rename.R
- print.R
- sysdata.rda
- theme_pr.R
- utils_analysis.R ⭐
- utils_bgc.R ⭐
- utils_cpr.R ⭐
- utils_indices.R ⭐
- utils_larvalfish.R (many commented functions)
- utils_nrs.R ⭐
- utils_plankton.R ⭐
- utils_plotting.R
- utils_policy.R
- utils_sat.R
- utils_sots.R
- utils-pipe.R
- utils.R ⭐ (core utilities)
- verbs.R ⭐ (S3 methods)
- zzz.R

**Legend**: ⭐ = Core/critical file

---

## Appendix B: Contact Points for Decisions

Several TODOs require decisions from specific people or teams:

1. **Claire Davies** (co-author) - CTD file filtering decision ([`R/utils_bgc.R:530`](R/utils_bgc.R:530))
2. **IMOS Data Team** - Data quality issues ([`R/utils_bgc.R:464`](R/utils_bgc.R:464))
3. **Architecture Lead** - Model storage decision ([`R/plot_timeseries.R:1033`](R/plot_timeseries.R:1033))

---

**End of Analysis**

*This document should be reviewed and updated as cleanup progresses.*