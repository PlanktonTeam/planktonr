# Deprecated functions
#
# This file contains functions that have been deprecated in favour of newer,
# more unified interfaces. These functions are maintained for backward
# compatibility but will emit deprecation warnings when used.
#
# Users should migrate to the recommended replacement functions.

#' Import NRS sampling trip metadata
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSTrips()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_trips()] which provides a unified interface for both NRS and CPR
#' trip metadata.
#'
#' @return A dataframe with NRS trip information.
#'
#' @seealso [pr_get_trips()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSTrips()
#'
#' # Use:
#' dat <- pr_get_trips(Survey = "NRS")
#' }
pr_get_NRSTrips <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSTrips()",
    with = "pr_get_trips()"
  )
  pr_get_trips(Survey = "NRS")
}


#' Get CPR sampling trip metadata
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_CPRTrips()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_trips()] which provides a unified interface for both NRS and CPR
#' trip metadata.
#'
#' @param ... Additional arguments passed to [pr_get_trips()], which passes them
#'   to [pr_add_Bioregions()]. Currently supports:
#'   * `near_dist_km` - Distance in kilometres to pad bioregion boundaries
#'
#' @return A dataframe with CPR trip information.
#'
#' @seealso [pr_get_trips()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_CPRTrips()
#'
#' # Use:
#' dat <- pr_get_trips(Survey = "CPR")
#'
#' # With bioregion padding:
#' dat <- pr_get_trips(Survey = "CPR", near_dist_km = 250)
#' }
pr_get_CPRTrips <- function(...) {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_CPRTrips()",
    with = "pr_get_trips()"
  )
  pr_get_trips(Survey = "CPR", ...)
}


# =============================================================================
# Deprecated data retrieval functions - replaced by pr_get_data()
# =============================================================================

#' Import NRS plankton data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSData()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @param Type The data of interest: `"Phytoplankton"` or `"Zooplankton"`
#' @param Variable Variable: `"abundance"` or `"biovolume"`
#' @param Subset Data level: `"raw"`, `"htg"`, `"genus"`, `"species"`, or `"copepods"`
#'
#' @return A dataframe with NRS plankton data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
#'
#' # Use:
#' dat <- pr_get_data(Survey = "NRS", Type = "Phytoplankton",
#'                    Variable = "abundance", Subset = "raw")
#' }
pr_get_NRSData <- function(Type = "Phytoplankton", Variable = "abundance", Subset = "raw") {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSData()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "NRS", Type = Type, Variable = Variable, Subset = Subset)
}


#' Import CPR plankton data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_CPRData()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @param Type The data of interest: `"Phytoplankton"` or `"Zooplankton"`
#' @param Variable Variable: `"abundance"` or `"biovolume"`
#' @param Subset Data level: `"raw"`, `"htg"`, `"genus"`, `"species"`, or `"copepods"`
#'
#' @return A dataframe with CPR plankton data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_CPRData(Type = "Phytoplankton", Variable = "abundance", Subset = "raw")
#'
#' # Use:
#' dat <- pr_get_data(Survey = "CPR", Type = "Phytoplankton",
#'                    Variable = "abundance", Subset = "raw")
#' }
pr_get_CPRData <- function(Type = "Phytoplankton", Variable = "abundance", Subset = "raw") {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_CPRData()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "CPR", Type = Type, Variable = Variable, Subset = Subset)
}


#' Load NRS chemistry data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSChemistry()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @return A dataframe with NRS chemistry data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSChemistry()
#'
#' # Use:
#' dat <- pr_get_data(Survey = "NRS", Type = "Chemistry")
#' }
pr_get_NRSChemistry <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSChemistry()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "NRS", Type = "Chemistry")
}


#' Load NRS pigments data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSPigments()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @param Format Output format: `"all"` or `"binned"`
#'
#' @return A dataframe with NRS pigments data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSPigments(Format = "binned")
#'
#' # Use:
#' dat <- pr_get_data(Survey = "NRS", Type = "Pigments", Format = "binned")
#' }
pr_get_NRSPigments <- function(Format = "all") {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSPigments()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "NRS", Type = "Pigments", Format = Format)
}


#' Load NRS picophytoplankton data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSPico()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @return A dataframe with NRS picophytoplankton data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSPico()
#'
#' # Use:
#' dat <- pr_get_data(Survey = "NRS", Type = "Pico")
#' }
pr_get_NRSPico <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSPico()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "NRS", Type = "Pico")
}


#' Load microbial data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSMicro()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @param Survey Survey: `"NRS"`, `"Coastal"`, or `"GO-SHIP"`
#'
#' @return A dataframe with microbial data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSMicro(Survey = "GO-SHIP")
#'
#' # Use:
#' dat <- pr_get_data(Survey = "GO-SHIP", Type = "Micro")
#' }
pr_get_NRSMicro <- function(Survey = "NRS") {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSMicro()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = Survey, Type = "Micro")
}


#' Load NRS Total Suspended Solids data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSTSS()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @return A dataframe with NRS TSS data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSTSS()
#'
#' # Use:
#' dat <- pr_get_data(Survey = "NRS", Type = "TSS")
#' }
pr_get_NRSTSS <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSTSS()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "NRS", Type = "TSS")
}


#' Load NRS CTD profile data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_NRSCTD()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @return A dataframe with NRS CTD data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_NRSCTD()
#'
#' # Use:
#' dat <- pr_get_data(Survey = "NRS", Type = "CTD")
#' }
pr_get_NRSCTD <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_NRSCTD()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "NRS", Type = "CTD")
}


#' Load Coastal Seas chemistry and nutrient data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pr_get_CSChem()` was deprecated in planktonr 0.7.0 in favour of
#' [pr_get_data()] which provides a unified interface for all data types.
#'
#' @return A dataframe with Coastal Seas chemistry data.
#'
#' @seealso [pr_get_data()] for the preferred interface
#'
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' # Instead of:
#' dat <- pr_get_CSChem()
#'
#' # Use:
#' dat <- pr_get_data(Survey = "Coastal", Type = "Chemistry")
#' }
pr_get_CSChem <- function() {
  lifecycle::deprecate_warn(
    when = "0.7.0",
    what = "pr_get_CSChem()",
    with = "pr_get_data()"
  )
  pr_get_data(Survey = "Coastal", Type = "Chemistry")
}
