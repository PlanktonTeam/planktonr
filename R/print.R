#' @title Print method for planktonr_dat objects
#'
#' @description
#' Provides a custom print method for `planktonr_dat` objects.
#' It first displays the custom attributes associated with the `planktonr_dat` object,
#' such as `Type`, `Survey`, `Variable`, and `Model`, before printing
#' the underlying tibble data. This provides immediate context for the data.
#'
#' The `Model` attribute is specially handled: if it contains `stats::lm` objects,
#' a concise summary of the number of models is printed instead of the full model output.
#'
#' @param x A `planktonr_dat` object to print.
#' @param ... Additional arguments passed to the `print.tbl_df` method,
#'   such as `n` for the number of rows or `width` for output width.
#' @param n <[`integer`][base::integer]> Number of rows to print. If `NULL`,
#'   the default from `tibble` options will be used.
#' @param width <[`integer`][base::integer]> Width of the output. If `NULL`,
#'   the default from `tibble` options will be used.
#'
#' @returns The `planktonr_dat` object `x`, invisibly.
#' @export
#' @importFrom utils capture.output str
print.planktonr_dat <- function(x, ..., n = NULL, width = NULL) {
  # ... (function body remains exactly the same as previously provided) ...
  # (Copy the full function body from the previous response here)

  # 1. Extract and prepare custom attributes for printing
  standard_attrs_to_exclude <- c(
    "names", "row.names", "class",
    "groups", "vars", "drop",
    "index", "comment",
    "spec"
  )

  all_object_attrs <- attributes(x)
  attrs_to_print_formatted <- list()

  custom_attr_names <- setdiff(names(all_object_attrs), standard_attrs_to_exclude)

  core_planktonr_attrs_order <- c("Type", "Survey", "Variable", "Model")
  ordered_attr_names <- unique(c(core_planktonr_attrs_order, custom_attr_names))

  for (attr_name in ordered_attr_names) {
    attr_value <- all_object_attrs[[attr_name]]


    # If the attribute is NULL, skip it entirely and proceed to the next attribute.
    if (is.null(attr_value)) {
      next
    }

    if (attr_name == "Model") {
      if (is.list(attr_value) && length(attr_value) > 0 && all(sapply(attr_value, inherits, "lm"))) {
        attrs_to_print_formatted[[attr_name]] <- paste0(
          "List of ", length(attr_value), " 'lm' model object(s). ",
          "Access with `attr(x, 'Model')`."
        )
      } else if (is.list(attr_value) && length(attr_value) > 0) {
        attrs_to_print_formatted[[attr_name]] <- paste0(
          "List of ", length(attr_value), " generic object(s). ",
          "Access with `attr(x, 'Model')`."
        )
      } else if (is.atomic(attr_value) && length(attr_value) == 1 && is.null(names(attr_value))) {
        attrs_to_print_formatted[[attr_name]] <- as.character(attr_value)
      } else {
        attrs_to_print_formatted[[attr_name]] <- paste(
          capture.output(str(attr_value, max.level = 1, give.attr = FALSE)),
          collapse = "\n    "
        )
      }
    } else if (is.atomic(attr_value) && length(attr_value) == 1 && is.null(names(attr_value))) {
      attrs_to_print_formatted[[attr_name]] <- as.character(attr_value)
    } else {
      attrs_to_print_formatted[[attr_name]] <- paste(
        capture.output(str(attr_value, max.level = 1, give.attr = FALSE)),
        collapse = "\n    "
      )
    }
  }

  # 2. Print all collected and formatted attributes
  if (length(attrs_to_print_formatted) > 0) {
    cat("--- planktonr_dat Attributes ---\n")
    for (attr_name in names(attrs_to_print_formatted)) {
      cat(paste0("  ", attr_name, ": ", attrs_to_print_formatted[[attr_name]], "\n"))
    }
    cat("\n")
  }

  # 3. Print the underlying tibble
  NextMethod("print", x, ..., n = n, width = width)

  invisible(x)
}
