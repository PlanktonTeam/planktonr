# R/planktonr_dat.R

# Internal constructor (low-level, no validation)
new_planktonr_dat <- function(x, ...) {
  # All arguments in '...' become attributes
  # `x` is the underlying tibble
  tibble::new_tibble(x, ..., class = "planktonr_dat")
}

#' Define the constructor function for the planktonr data class
#'
#' This function creates a `planktonr_dat` object, which is a tibble
#' with additional custom attributes for metadata.
#'
#' @param .data The data tibble to be converted to the `planktonr_dat` class.
#' @param type The data type. Must be one of "Microbes", "Phytoplankton",
#'   "Zooplankton", "Water", or "EOV".
#' @param survey The survey of the data. E.g., "NRS", "CPR", "LTM", "GO-SHIP", "Coastal".
#' @param variable What variable is being described or subsetted by this data.
#' @param model Optional model associated with the data.
#' @param ... Additional attributes to be stored with the `planktonr_dat` object.
#'   These will be stored as attributes on the object.
#'
#' @returns An object of class `planktonr_dat`.
#' @export
#'
#' @examples
#' data_tibble <- tibble::tibble(
#'   time = as.Date(c("2023-01-01", "2023-01-02")),
#'   value = c(10, 20)
#' )
#' pr_obj <- planktonr_dat(
#'   .data = data_tibble,
#'   type = "Phytoplankton",
#'   survey = "NRS",
#'   variable = "Chlorophyll_a",
#'   notes = "Example data set"
#' )
#' print(pr_obj)
planktonr_dat <- function(.data,
                          type = NULL,
                          survey = NULL,
                          variable = NULL,
                          model = NULL,
                          ...) {

  # Input Checks using assertthat
  # Input Checks using assertthat
  assertthat::assert_that(is.data.frame(.data), msg = "'.data' must be a data frame.")
  assertthat::assert_that(inherits(.data, "tbl_df"), msg = "'.data' must be a tibble (tbl_df).")
  assertthat::assert_that(is.null(type) || is.character(type), msg = "'type' must be a character string or NULL.")
  assertthat::assert_that(is.null(survey) || is.character(survey), msg = "'survey' must be a character string or NULL.")
  assertthat::assert_that(is.null(variable) || is.character(variable), msg = "'variable' must be a character string or NULL.")
  assertthat::assert_that(is.null(model) || is.character(model), msg = "'model' must be a character string or NULL.")
  if (!is.null(type)) {
    type <- pr_check_type(type) # Use your helper to standardize
    type <- rlang::arg_match0(type, values = c("Microbes", "Phytoplankton", "Zooplankton", "Fish", "Water", "EOV"),
                              arg_nm = "type") # Added arg_nm for better error message
  }
  if (!is.null(survey)) {
    survey <- rlang::arg_match0(survey, values = c("NRS", "CPR", "LTM", "GO-SHIP", "Coastal"),
                                arg_nm = "survey") # Added arg_nm for better error message
  }

  # Capture additional attributes passed via ...
  extra_attrs <- list(...)

  # Create the new planktonr_dat object using the internal constructor
  # All metadata are passed as attributes
  obj <- new_planktonr_dat(
    x = .data,
    Type = type,
    Survey = survey,
    Variable = variable,
    Model = model,
    !!!extra_attrs # Splice in any extra attributes
  )

  obj
}

#' Check if an object is of class `planktonr_dat`
#'
#' This function tests if the given object `x` inherits from the `planktonr_dat` class.
#'
#' @param x An R object.
#' @returns `TRUE` if `x` is a `planktonr_dat` object, otherwise `FALSE`.
#' @export
#' @examples
#' # Assuming planktonr_dat constructor exists:
#' my_data <- planktonr_dat(tibble::tibble(a = 1:3), type = "Phytoplankton")
#' is_planktonr_dat(my_data) # Returns TRUE
#' is_planktonr_dat(tibble::tibble(a = 1:3)) # Returns FALSE
is_planktonr_dat <- function(x) {
  inherits(x, "planktonr_dat")
}

#' Create a `planktonr_dat` object with example data
#'
#' This is a convenience constructor function that wraps `planktonr_dat()` to
#' easily create a `planktonr_dat` object from a tibble, providing default
#' values for `type` and `survey`, and adding a 'notes' attribute.
#' This is useful for quickly generating sample `planktonr_dat` objects for testing or examples.
#'
#' @param ... Arguments passed to `tibble::tibble()` to create the underlying data.
#' @returns An object of class `planktonr_dat`.
#' @export
#' @examples
#' # Create an example planktonr_dat object
#' pr_example_data <- planktonr_data(
#'   species = c("copepod", "diatom"),
#'   count = c(100, 50)
#' )
#' print(pr_example_data)
#'
#' # Accessing custom attributes
#' attr(pr_example_data, "type")
#' attr(pr_example_data, "survey")
#' attr(pr_example_data, "notes")
planktonr_data <- function(...) {
  x <- tibble::tibble(...)
  # Here, we're setting 'created_at' and 'source' as direct attributes
  planktonr_dat(
    .data = x,
    type = "Phytoplankton", # Provide a default type for this helper
    survey = "NRS",        # Provide a default survey for this helper
    notes = "Example data set created by planktonr_data helper" # Example of an extra attribute
  )
}


#' Check and update Type as required
#'
#' @param Type Data type
#'
#' @returns Type as a standardised string
#' @noRd
pr_check_type <- function(Type){

  Type = stringr::str_to_lower(Type)

  if (Type %in% c("p", "phyto", "phytoplankton")){Type = "Phytoplankton"}
  if (Type %in% c("z", "zoop", "zooplankton")){Type = "Zooplankton"}
  if (Type %in% c("w", "water")){Type = "Water"}
  if (Type %in% c("eov")){Type = "EOV"} # Added EOV based on your constructor comment
  if (Type %in% c("m", "microbes")){Type = "Microbes"} # Added Microbes
  if (Type %in% c("f", "fish")){Type = "Fish"}

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

#' Generic function to get the Model attribute
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
