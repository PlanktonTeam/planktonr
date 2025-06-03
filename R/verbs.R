# R/verbs.R

#' Internal helper to extract custom attributes from a planktonr_dat object
#'
#' This function identifies and collects all attributes attached to a
#' `planktonr_dat` object that are not standard R, tibble, or dplyr attributes.
#' This allows these custom attributes to be preserved across data manipulation
#' operations.
#'
#' @param .data A `planktonr_dat` object.
#' @returns A named list of custom attributes.
#' @keywords internal
get_custom_attributes <- function(.data) {
  # Define standard attributes that dplyr or R base functions might manage.
  # We should NOT copy these, as dplyr's NextMethod will handle them or they are fundamental.
  standard_attrs_to_exclude <- c(
    "names", "row.names", "class", # Base R attributes
    "groups", "vars", "drop"      # dplyr attributes for grouped_df
  )
  all_attrs <- attributes(.data)
  custom_attr_names <- setdiff(names(all_attrs), standard_attrs_to_exclude)

  # Store the custom attributes in a list for easy reapplication
  all_attrs[custom_attr_names]
}

# --- tidyr methods ---

#' Reshape data from wide to long format for planktonr_dat objects
#'
#' This is an S3 method for `tidyr::pivot_longer` that ensures
#' `planktonr_dat` attributes are preserved when reshaping the data.
#'
#' @param data A `planktonr_dat` object.
#' @param ... Arguments passed on to `tidyr::pivot_longer`.
#' @returns A `planktonr_dat` object with data reshaped to long format,
#'   preserving original attributes.
#' @export
#' @importFrom tidyr pivot_longer
pivot_longer.planktonr_dat <- function(data, ...) {
  original_attrs <- get_custom_attributes(data)

  # Call the next method (tidyr::pivot_longer.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

#' Reshape data from long to wide format for planktonr_dat objects
#'
#' This is an S3 method for `tidyr::pivot_wider` that ensures
#' `planktonr_dat` attributes are preserved when reshaping the data.
#'
#' @param data A `planktonr_dat` object.
#' @param ... Arguments passed on to `tidyr::pivot_wider`.
#' @returns A `planktonr_dat` object with data reshaped to wide format,
#'   preserving original attributes.
#' @export
#' @importFrom tidyr pivot_wider
pivot_wider.planktonr_dat <- function(data, ...) {
  original_attrs <- get_custom_attributes(data)

  # Call the next method (tidyr::pivot_wider.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

#' Drop rows containing missing values for planktonr_dat objects
#'
#' This is an S3 method for `tidyr::drop_na` that ensures
#' `planktonr_dat` attributes are preserved.
#'
#' @param data A `planktonr_dat` object.
#' @param ... Arguments passed on to `tidyr::drop_na`.
#' @returns A `planktonr_dat` object with rows containing `NA` values dropped,
#'   preserving original attributes.
#' @export
#' @importFrom tidyr drop_na
drop_na.planktonr_dat <- function(data, ...) {
  original_attrs <- get_custom_attributes(data)

  # Call the next method (tidyr::drop_na.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

# --- dplyr methods ---

#' Group a planktonr_dat object by one or more variables
#'
#' This is an S3 method for `dplyr::group_by` that ensures
#' `planktonr_dat` attributes are preserved and the object correctly
#' becomes a `grouped_df` while retaining its `planktonr_dat` class.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Variables to group by.
#' @param .add When `TRUE`, add to the existing groups, otherwise start with new groups.
#' @param .drop When `TRUE`, empty groups are dropped.
#' @returns A `planktonr_dat` object that is also a `grouped_df`,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr group_by group_by_drop_default
group_by.planktonr_dat <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {

  original_attrs <- get_custom_attributes(.data)

  # Call the next method (dplyr::group_by.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  # Crucial: Prepend "planktonr_dat" to preserve "grouped_df" class
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

#' Summarise data in a planktonr_dat object
#'
#' This is an S3 method for `dplyr::summarise` that ensures
#' `planktonr_dat` attributes are preserved during summarisation.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Name-value pairs of summary statistics.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional grouping variables.
#' @param .groups Control the grouping structure of the result.
#' @returns A `planktonr_dat` object with summarised data,
#'   preserving original attributes. The grouping structure depends on `.groups`.
#' @export
#' @importFrom dplyr summarise
#' @importFrom rlang enquo inject
summarise.planktonr_dat <- function(.data, ..., .by = NULL, .groups = "drop") {

  original_attrs <- get_custom_attributes(.data)

  # CRUCIAL FIX: Call NextMethod() without explicit arguments.
  # This ensures that `.data`, `...` (the data-masking expressions),
  # `.by` (the tidy-select expression), and `.groups` are all passed
  # correctly to `dplyr::summarise.tbl_df` in their original evaluation context.
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }

  result
}

#' Create or modify columns in a planktonr_dat object
#'
#' This is an S3 method for `dplyr::mutate` that ensures
#' `planktonr_dat` attributes are preserved when adding or modifying columns.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Name-value pairs of expressions.
#' @param .before,.after,.keep,.width Control the placement and selection of columns.
#'   See [dplyr::mutate()] for details.
#' @returns A `planktonr_dat` object with modified or new columns,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr mutate
mutate.planktonr_dat <- function(.data, ..., .before = NULL, .after = NULL, .keep = "all", .width = NULL) {
  original_attrs <- get_custom_attributes(.data)

  # Call the next method (dplyr::mutate.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

#' Filter rows of a planktonr_dat object
#'
#' This is an S3 method for `dplyr::filter` that ensures
#' `planktonr_dat` attributes are preserved when filtering rows.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Logical predicates to filter by.
#' @returns A `planktonr_dat` object with filtered rows,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr filter
filter.planktonr_dat <- function(.data, ...) {
  original_attrs <- get_custom_attributes(.data)

  # Call the next method (dplyr::filter.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

#' Select, rename, or reorder columns in a planktonr_dat object
#'
#' This is an S3 method for `dplyr::select` that ensures
#' `planktonr_dat` attributes are preserved when manipulating columns.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to select, rename, or reorder.
#' @returns A `planktonr_dat` object with selected columns,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr select
select.planktonr_dat <- function(.data, ...) {
  original_attrs <- get_custom_attributes(.data)

  # Call the next method (dplyr::select.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}

#' Arrange rows of a planktonr_dat object
#'
#' This is an S3 method for `dplyr::arrange` that ensures
#' `planktonr_dat` attributes are preserved when arranging rows.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Columns to arrange by.
#' @param .by_group If `TRUE`, will arrange first by the grouping variables.
#'   See [dplyr::arrange()] for details.
#' @returns A `planktonr_dat` object with arranged rows,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr arrange
arrange.planktonr_dat <- function(.data, ..., .by_group = FALSE) {
  original_attrs <- get_custom_attributes(.data)

  # Call the next method (dplyr::arrange.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}


#' Convert a planktonr_dat object to a rowwise data frame
#'
#' This is an S3 method for `dplyr::rowwise` that ensures
#' `planktonr_dat` attributes are preserved and the object correctly
#' becomes a `rowwise_df` while retaining its `planktonr_dat` class.
#'
#' @param data A `planktonr_dat` object.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> Columns to group by.
#'   `rowwise()` groups by all columns by default.
#' @param .rows Not currently used by `dplyr::rowwise()`, but included for
#'   compatibility with future `dplyr` updates.
#' @returns A `planktonr_dat` object that is also a `rowwise_df`,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr rowwise
rowwise.planktonr_dat <- function(data, ..., .rows = NULL) {
  original_attrs <- get_custom_attributes(data)

  # Call the next method (dplyr::rowwise.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  # Crucial: Prepend "planktonr_dat" to preserve "rowwise_df" class
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}


#' Relocate columns in a planktonr_dat object
#'
#' This is an S3 method for `dplyr::relocate` that ensures
#' `planktonr_dat` attributes are preserved when moving columns.
#'
#' @param .data A `planktonr_dat` object.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to relocate.
#' @param .before <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to move other columns before.
#' @param .after <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to move other columns after.
#' @returns A `planktonr_dat` object with columns reordered,
#'   preserving original attributes.
#' @export
#' @importFrom dplyr relocate
relocate.planktonr_dat <- function(.data, ..., .before = NULL, .after = NULL) {
  original_attrs <- get_custom_attributes(.data)

  # Call the next method (dplyr::relocate.tbl_df)
  result <- NextMethod()

  # Re-apply the planktonr_dat class and custom attributes
  class(result) <- unique(c("planktonr_dat", class(result)))
  for (attr_name in names(original_attrs)) {
    attr(result, attr_name) <- original_attrs[[attr_name]]
  }
  result
}



