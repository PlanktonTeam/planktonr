# Setup for test suite
# This file runs before all tests

# Set options for testing
options(
  planktonr.test_mode = TRUE,
  warn = 1  # Show warnings immediately
)

# Check for optional test dependencies
optional_packages <- c("vegan", "ncdf4", "RCurl", "thredds")
available_packages <- sapply(optional_packages, requireNamespace, quietly = TRUE)

if (!all(available_packages)) {
  missing <- optional_packages[!available_packages]
  message(
    "Note: Some optional test dependencies are not available: ",
    paste(missing, collapse = ", "),
    "\nSome integration tests may be skipped."
  )
}

# Helper function to skip tests when offline
skip_if_offline <- function() {
  if (!curl::has_internet()) {
    testthat::skip("No internet connection available")
  }
}

# Helper function to skip slow integration tests on CRAN
skip_if_not_extensive <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "false")) {
    testthat::skip("Skipping extensive test on CRAN")
  }
}

# Set seed for reproducible random data in fixtures
set.seed(42)

# Print test session info (useful for debugging)
if (interactive()) {
  message("Test suite setup complete")
  message("R version: ", R.version.string)
  message("planktonr location: ", find.package("planktonr"))
}
