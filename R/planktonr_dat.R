# R/planktonr_dat.R

# Internal constructor (low-level, no validation)
new_planktonr_dat <- function(x, ...) {
  # All arguments in '...' become attributes
  # `x` is the underlying tibble (ensured by planktonr_dat constructor)
  tibble::new_tibble(x, ..., class = "planktonr_dat")
}

#' Define the constructor function for the planktonr data class
#'
#' This function creates a `planktonr_dat` object, which is a tibble
#' with additional custom attributes for metadata. It can accept
#' either a `data.frame` or a `tibble`, coercing `data.frame` inputs to `tibble`.
#'
#' @param .data The data `data.frame` or `tibble` to be converted to the `planktonr_dat` class.
#' @param Type The data type. Must be one of "Microbes", "Phytoplankton",
#'   "Zooplankton", "Water", or "EOV".
#' @param Survey The survey of the data. E.g., "NRS", "CPR", "LTM", "GO-SHIP", "Coastal".
#' @param Variable What variable is being described or subsetted by this data.
#' @param Model Optional model associated with the data.
#' @param ... Additional attributes to be stored with the `planktonr_dat` object.
#'   These will be stored as attributes on the object.
#'
#' @returns An object of class `planktonr_dat`.
#' @export
#'
#' @examples
#' # Create from a tibble (existing behavior)
#' tibble_data <- tibble::tibble(
#'   time = as.Date(c("2023-01-01", "2023-01-02")),
#'   value = c(10, 20)
#' )
#' pr_obj_tibble <- planktonr_dat(
#'   .data = tibble_data,
#'   Type = "Phytoplankton",
#'   Survey = "NRS",
#'   Variable = "Chlorophyll_a"
#' )
#' print(pr_obj_tibble)
#'
#' # Create from a base R data.frame (newly supported)
#' df_data <- data.frame(
#'   time = as.Date(c("2023-01-03", "2023-01-04")),
#'   value = c(30, 40)
#' )
#' pr_obj_df <- planktonr_dat(
#'   .data = df_data,
#'   Type = "Zooplankton",
#'   Survey = "CPR",
#'   Variable = "Biomass"
#' )
#' print(pr_obj_df)
#' class(pr_obj_df) # Still a planktonr_dat (which inherits from tibble)
planktonr_dat <- function(.data,
                          Type = NULL,
                          Survey = NULL,
                          Variable = NULL,
                          Model = NULL,
                          ...) {

  # Input Checks
  # 1. Ensure it's at least a data frame
  assertthat::assert_that(is.data.frame(.data), msg = "'.data' must be a data frame or tibble.")

  # 2. Coerce to tibble if it's not already one
  if (!inherits(.data, "tbl_df")) {
    .data <- tibble::as_tibble(.data)
    # message("'.data' coerced from data.frame to tibble.") # Optional: inform the user
  }

  # 3. Remove 'spec_tbl_df' class and 'spec' attribute if present
  # This ensures the underlying tibble is always a standard 'tbl_df'
  if (inherits(.data, "spec_tbl_df")) {
    class(.data) <- setdiff(class(.data), "spec_tbl_df")
    attr(.data, "spec") <- NULL
    # message("Removed 'spec_tbl_df' class and 'spec' attribute from .data for consistent printing.")
  }

  # 4. Remove 'problems' attribute if present
  # This attribute is often from readr and not relevant for custom S3 objects
  if (!is.null(attr(.data, "problems"))) {
    attr(.data, "problems") <- NULL
    # message("Removed 'problems' attribute from .data.")
  }

  # Validate metadata arguments (unchanged)
  assertthat::assert_that(is.null(Type) || is.character(Type), msg = "'Type' must be a character string or NULL.")
  assertthat::assert_that(is.null(Survey) || is.character(Survey), msg = "'Survey' must be a character string or NULL.")
  assertthat::assert_that(is.null(Variable) || is.character(Variable), msg = "'Variable' must be a character string or NULL.")
  assertthat::assert_that(is.null(Model) || is.character(Model), msg = "'Model' must be a character string or NULL.")

  if (!is.null(Type)) {
    Type <- pr_check_type(Type) # Use your helper to standardize
    Type <- rlang::arg_match0(Type, values = c("Microbes", "Phytoplankton", "Zooplankton", "Fish", "Water", "EOV",
                                                "Chemistry", "Pigments", "Pico", "TSS", "CTD", "Micro"),
                              arg_nm = "Type")
  }
  if (!is.null(Survey)) {
    Survey <- rlang::arg_match0(Survey, values = c("NRS", "CPR", "LTM", "GO-SHIP", "Coastal", "SOTS"),
                                # Assuming NRS may include SOTS where that matches, but SOTS is for SOTS data where it is different from NRS
                                arg_nm = "Survey")
  }

  # Capture additional attributes passed via ...
  extra_attrs <- list(...)

  # Create the new planktonr_dat object using the internal constructor
  obj <- new_planktonr_dat(
    x = .data, # .data is now guaranteed to be a tibble
    Type = Type,
    Survey = Survey,
    Variable = Variable,
    Model = Model,
    !!!extra_attrs # Splice in any extra attributes
  )

  obj
}

#' Check if an object is of class `planktonr_dat`
#'
#' This function tests if the given object `x` inherits from the `planktonr_dat` S3 class.
#' It's a standard way to verify if an object is of your custom data type.
#'
#' @param x An R object to check.
#' @returns A logical value: `TRUE` if `x` is a `planktonr_dat` object, otherwise `FALSE`.
#' @export
#'
#' @examples
#' # Assuming planktonr_dat constructor exists:
#' my_data <- planktonr_dat(tibble::tibble(a = 1:3, b = 4:6),
#'                          Type = "Phytoplankton",
#'                          Survey = "NRS")
#'
#' # Check if it's a planktonr_dat object
#' is_planktonr_dat(my_data) # Returns TRUE
#'
#' # Check a regular tibble
#' is_planktonr_dat(tibble::tibble(a = 1:3)) # Returns FALSE
#'
#' # Check a data.frame
#' is_planktonr_dat(data.frame(x = 1)) # Returns FALSE
is_planktonr_dat <- function(x) {
  inherits(x, "planktonr_dat")
}

# Example of a constructor function to make it easy to create data
# (No changes needed here as it calls planktonr_dat, which now handles coercion)
planktonr_data <- function(...) {
  x <- tibble::tibble(...) # This always creates a tibble internally
  # Here, we're setting 'created_at' and 'source' as direct attributes
  planktonr_dat(
    .data = x,
    Type = "Phytoplankton", # Provide a default Type for this helper
    Survey = "NRS",        # Provide a default survey for this helper
    notes = "Example data set created by planktonr_data helper" # Example of an extra attribute
  )
}


#' Check and update Type as required
#'
#' Standardises Type strings for plankton data. Handles case-insensitive 
#' matching for Phytoplankton and Zooplankton. Other types are returned unchanged.
#'
#' @param Type Data type
#'
#' @returns Type as a standardised string
#' @noRd
pr_check_type <- function(Type){

  # Return unchanged if not a single string (validation happens elsewhere)
  if (!is.character(Type) || length(Type) != 1) {
    return(Type)
  }

  Type_lower <- stringr::str_to_lower(Type)

  if (Type_lower %in% c("p", "phyto", "phytoplankton")){
    Type <- "Phytoplankton"
  } else if (Type_lower %in% c("z", "zoop", "zooplankton")){
    Type <- "Zooplankton"
  } else if (Type_lower %in% c("w", "water")){
    Type <- "Water"
  } else if (Type_lower %in% c("eov")){
    Type <- "EOV"
  } else if (Type_lower %in% c("m", "microbes")){
    Type <- "Microbes"
  } else if (Type_lower %in% c("f", "fish")){
    Type <- "Fish"
  }
  # Otherwise return Type unchanged (e.g., Chemistry, Pigments, Pico, TSS, CTD, Micro)

  return(Type)
}


# R/getters.R

#' Generic function to get the Type attribute
#'
#' @param x A `planktonr_dat` object.
#' @returns The `Type` attribute from the object.
#' @export
pr_get_type <- function(x){
  UseMethod("pr_get_type")
}

#' @rdname pr_get_type
#' @export
pr_get_type.planktonr_dat <- function(x){
  attr(x, "Type")
}

#' Generic function to get the Survey attribute
#'
#' @param x A `planktonr_dat` object.
#' @returns The `Survey` attribute from the object.
#' @export
pr_get_survey <- function(x){
  UseMethod("pr_get_survey")
}

#' @rdname pr_get_survey
#' @export
pr_get_survey.planktonr_dat <- function(x){
  attr(x, "Survey")
}

#' Generic function to get the Variable attribute
#'
#' @param x A `planktonr_dat` object.
#' @returns The `Variable` attribute from the object.
#' @export
pr_get_variable <- function(x){
  UseMethod("pr_get_variable")
}

#' @rdname pr_get_variable
#' @export
pr_get_variable.planktonr_dat <- function(x){
  attr(x, "Variable")
}

#' Extract fitted model objects from planktonr data with trend analysis
#'
#' @param x A `planktonr_dat` object.
#' @returns The `Model` attribute from the object.
#' @export
pr_get_model <- function(x){
  UseMethod("pr_get_model")
}

#' @rdname pr_get_model
#' @export
pr_get_model.planktonr_dat <- function(x){
  attr(x, "Model")
}
