# ==============================================================================
# Unit tests for print.planktonr_dat method
# ==============================================================================

testthat::test_that("print.planktonr_dat displays basic planktonr_dat object", {
  # Create a simple planktonr_dat object
  test_data <- data.frame(
    StationCode = c("NSI", "PHB", "MAI"),
    Values = c(10.5, 20.3, 15.7),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  # Capture the print output
  output <- capture.output(print(pr_data))
  
  # Check that attributes section is printed
  testthat::expect_true(any(grepl("planktonr_dat Attributes", output)))
  
  # Check that Type and Survey are printed
  testthat::expect_true(any(grepl("Type.*Phytoplankton", output)))
  testthat::expect_true(any(grepl("Survey.*NRS", output)))
  
  # Check that the data is printed (should show StationCode column)
  testthat::expect_true(any(grepl("StationCode", output)))
})

testthat::test_that("print.planktonr_dat displays object with Variable attribute", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS", "Abundance")
  
  output <- capture.output(print(pr_data))
  
  # Check that Variable is printed
  testthat::expect_true(any(grepl("Variable.*Abundance", output)))
})

testthat::test_that("print.planktonr_dat handles NULL attributes gracefully", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  # Set Variable to NULL explicitly
  attr(pr_data, "Variable") <- NULL
  
  output <- capture.output(print(pr_data))
  
  # Should not print Variable if NULL
  testthat::expect_false(any(grepl("Variable:", output)))
  
  # Should still print Type and Survey
  testthat::expect_true(any(grepl("Type", output)))
  testthat::expect_true(any(grepl("Survey", output)))
})

testthat::test_that("print.planktonr_dat displays Model attribute as list summary for lm objects", {
  test_data <- data.frame(
    x = 1:10,
    y = 1:10 + rnorm(10, 0, 0.1),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  # Add lm models
  model1 <- lm(y ~ x, data = test_data)
  model2 <- lm(y ~ poly(x, 2), data = test_data)
  attr(pr_data, "Model") <- list(model1, model2)
  
  output <- capture.output(print(pr_data))
  
  # Should show concise summary for lm objects
  testthat::expect_true(any(grepl("Model.*List of 2 'lm' model object", output)))
  testthat::expect_true(any(grepl("Access with.*attr.*'Model'", output)))
})

testthat::test_that("print.planktonr_dat displays Model attribute as list summary for generic objects", {
  test_data <- data.frame(
    x = 1:5,
    y = 6:10,
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  # Add generic list objects
  attr(pr_data, "Model") <- list(a = 1, b = 2, c = 3)
  
  output <- capture.output(print(pr_data))
  
  # Should show generic list summary
  testthat::expect_true(any(grepl("Model.*List of 3 generic object", output)))
})

testthat::test_that("print.planktonr_dat displays single atomic Model attribute directly", {
  test_data <- data.frame(
    x = 1:5,
    y = 6:10,
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  # Add single atomic value
  attr(pr_data, "Model") <- "simple_model"
  
  output <- capture.output(print(pr_data))
  
  # Should show the value directly
  testthat::expect_true(any(grepl("Model.*simple_model", output)))
})

testthat::test_that("print.planktonr_dat excludes standard tibble attributes", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  output <- capture.output(print(pr_data))
  
  # Should NOT print standard attributes
  testthat::expect_false(any(grepl("^\\s*names:", output)))
  testthat::expect_false(any(grepl("^\\s*row.names:", output)))
  testthat::expect_false(any(grepl("^\\s*class:", output)))
})

testthat::test_that("print.planktonr_dat returns object invisibly", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  # The print method should return the object invisibly
  result <- withVisible(print(pr_data))
  
  testthat::expect_false(result$visible)
  testthat::expect_identical(result$value, pr_data)
})

testthat::test_that("print.planktonr_dat respects n parameter for row limit", {
  test_data <- data.frame(
    StationCode = rep(c("NSI", "PHB", "MAI"), 20),
    Values = runif(60),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  # Print with limited rows
  output <- capture.output(print(pr_data, n = 5))
  
  # Should indicate there are more rows
  # The tibble print method typically shows "... with X more rows" message
  combined_output <- paste(output, collapse = "\n")
  testthat::expect_true(grepl("more rows|rows total", combined_output, ignore.case = TRUE))
})

testthat::test_that("print.planktonr_dat handles empty planktonr_dat object", {
  test_data <- data.frame(
    StationCode = character(0),
    Values = numeric(0),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  output <- capture.output(print(pr_data))
  
  # Should still print attributes
  testthat::expect_true(any(grepl("planktonr_dat Attributes", output)))
  testthat::expect_true(any(grepl("Type", output)))
  
  # Should show 0 rows (tibble formats this as "0 x 2" or similar)
  combined_output <- paste(output, collapse = " ")
  testthat::expect_true(grepl("0.*(x|×)", combined_output))
})

testthat::test_that("print.planktonr_dat displays custom attributes in correct order", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS", "Abundance")
  
  # Add custom attribute
  attr(pr_data, "CustomAttr") <- "CustomValue"
  
  output <- capture.output(print(pr_data))
  
  # Find positions of attributes in output
  type_line <- which(grepl("Type:", output))[1]
  survey_line <- which(grepl("Survey:", output))[1]
  variable_line <- which(grepl("Variable:", output))[1]
  custom_line <- which(grepl("CustomAttr:", output))[1]
  
  # Core attributes should come before custom attributes
  testthat::expect_true(!is.na(type_line))
  testthat::expect_true(!is.na(survey_line))
  testthat::expect_true(!is.na(variable_line))
  testthat::expect_true(!is.na(custom_line))
  
  # Type, Survey, Variable should come before CustomAttr
  testthat::expect_true(type_line < custom_line)
  testthat::expect_true(survey_line < custom_line)
  testthat::expect_true(variable_line < custom_line)
})

testthat::test_that("print.planktonr_dat handles complex nested attributes", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  # Add complex nested attribute
  attr(pr_data, "Metadata") <- list(
    source = "AODN",
    version = "1.0",
    details = list(qc = "passed", flags = c(1, 2))
  )
  
  output <- capture.output(print(pr_data))
  
  # Should print some representation of the nested structure
  testthat::expect_true(any(grepl("Metadata", output)))
})

testthat::test_that("print.planktonr_dat works with planktonr_dat that has many columns", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Latitude = c(-27.33, -34.09),
    Longitude = c(153.55, 151.22),
    SampleTime_Local = as.POSIXct(c("2022-01-01", "2022-01-15")),
    Parameters = c("Temperature", "Salinity"),
    Values = c(10.5, 20.3),
    Year_Local = c(2022, 2022),
    Month_Local = c(1, 1),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  output <- capture.output(print(pr_data))
  
  # Should print without error and show attributes
  testthat::expect_true(any(grepl("planktonr_dat Attributes", output)))
  testthat::expect_true(length(output) > 0)
})

testthat::test_that("print.planktonr_dat displays attributes section before data", {
  test_data <- data.frame(
    StationCode = c("NSI", "PHB"),
    Values = c(10.5, 20.3),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  output <- capture.output(print(pr_data))
  
  # Find the line with attributes header
  attr_line <- which(grepl("planktonr_dat Attributes", output))[1]
  
  # Find the line with column names (tibble header)
  # Typically has "# A tibble:" or shows column names with types
  tibble_line <- which(grepl("tibble:|StationCode", output))[1]
  
  # Attributes should come before tibble data
  testthat::expect_true(!is.na(attr_line))
  testthat::expect_true(!is.na(tibble_line))
  testthat::expect_true(attr_line < tibble_line)
})

testthat::test_that("print.planktonr_dat handles Model with single lm object in list", {
  test_data <- data.frame(
    x = 1:10,
    y = 1:10 + rnorm(10, 0, 0.1),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  # Add single lm model in a list
  model <- lm(y ~ x, data = test_data)
  attr(pr_data, "Model") <- list(model)
  
  output <- capture.output(print(pr_data))
  
  # Should show list with 1 lm object
  testthat::expect_true(any(grepl("Model.*List of 1 'lm' model object", output)))
})

testthat::test_that("print.planktonr_dat handles mixed list of lm and non-lm objects", {
  test_data <- data.frame(
    x = 1:10,
    y = 1:10,
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  # Add mixed list (not all lm objects)
  model <- lm(y ~ x, data = test_data)
  attr(pr_data, "Model") <- list(model, "not_a_model", 123)
  
  output <- capture.output(print(pr_data))
  
  # Should NOT identify as pure lm list, should use generic
  testthat::expect_true(any(grepl("Model.*List of 3 generic object", output)))
})

testthat::test_that("print.planktonr_dat passes additional arguments to tibble print", {
  test_data <- data.frame(
    StationCode = rep(c("NSI", "PHB"), 10),
    Values = runif(20),
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Phytoplankton", "NRS")
  
  # Test that n parameter works (already tested above, but confirming with ...)
  testthat::expect_no_error(print(pr_data, n = 3))
  
  # Test that width parameter is accepted
  testthat::expect_no_error(print(pr_data, width = 50))
})

testthat::test_that("print.planktonr_dat handles named atomic Model attribute", {
  test_data <- data.frame(
    x = 1:5,
    y = 6:10,
    stringsAsFactors = FALSE
  )
  
  pr_data <- planktonr_dat(test_data, "Water", "NRS")
  
  # Add named atomic value
  attr(pr_data, "Model") <- c(ModelName = "TestModel")
  
  output <- capture.output(print(pr_data))
  
  # Should use str() for named vectors (not simple printing)
  testthat::expect_true(any(grepl("Model", output)))
})
