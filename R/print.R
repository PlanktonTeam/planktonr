
# R/print.R (Updated)

#' print method for planktonr_dat
#'
#' @param x The planktonr_dat object.
#' @param ... Not used.
#'
#' @export
print.planktonr_dat <- function(x, ...) {
  cat("A planktonr_dat object\n")

  # Define standard attributes that dplyr or R base functions might manage.
  # We should NOT print these as "custom attributes" as they are internal/system-managed.
  standard_attrs_to_exclude <- c(
    "names", "row.names", "class", # Base R attributes
    "groups", "vars", "drop",     # dplyr grouped_df attributes
    "Type", "Survey", "Variable", "Model" # Your *known* custom attributes
  )

  # Get all attributes and filter for truly custom/unknown ones
  all_attrs <- attributes(x)
  known_attrs_to_print <- c("Survey", "Type", "Variable", "Model") # Order you want them printed

  # Print known custom attributes first
  for (attr_name in known_attrs_to_print) {
    attr_value <- attr(x, attr_name)
    if (!is.null(attr_value)) {
      cat(sprintf("%s: %s\n", attr_name, format(attr_value)))
    }
  }

  # Dynamically print any other custom attributes not explicitly handled
  other_custom_attr_names <- setdiff(names(all_attrs), c(standard_attrs_to_exclude, known_attrs_to_print))
  if (length(other_custom_attr_names) > 0) {
    cat("Other Custom Attributes:\n")
    for (attr_name in other_custom_attr_names) {
      attr_value <- attr(x, attr_name)
      cat(sprintf("  %s: %s\n", attr_name, format(attr_value)))
    }
  }

  cat("---\n") # Separator

  # Crucial: Call NextMethod to let dplyr's print.grouped_df or print.tbl_df handle the data display
  NextMethod("print")
}

