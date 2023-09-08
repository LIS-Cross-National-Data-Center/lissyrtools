# test-print.R

library(testthat)
library(lissyrtools)

.run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")


# ** percentiles ----------------------------------------------------------

test_that("print_percentiles returns expected percentiles", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3)),
                      file2 = data.frame(var1 = c(4, 5, 6)))

  result <- print_percentiles(lissy_files, "var1", breaks = c(0.25, 0.5, 0.75))
  expect_is(result, "data.frame")
  expect_equal(result$percentile, c(0.25, 0.5, 0.75))
})

test_that("print_percentiles handles NA values correctly", {

  suppressWarnings({
    lissy_files <- list(file1 = data.frame(var1 = c(1:50, NA)),
                        file2 = data.frame(var1 = c(51:100, NA)))

    result <- print_percentiles(lissy_files, variable = "var1", na.rm = FALSE)
    expect_true(any(is.na(result$value_file1)))
    expect_true(any(is.na(result$value_file2)))

    result <- print_percentiles(lissy_files, variable = "var1", na.rm = TRUE)
    expect_true(all(!is.na(result$value_file1)))
    expect_true(all(!is.na(result$value_file2)))
  })
})

test_that("print_percentiles handles all NA or zero values in variable", {
  lissy_files <- list(file1 = data.frame(var1 = c(NA_integer_, NA_integer_, NA_integer_)),
                      file2 = data.frame(var1 = c(0, 0, 0)))

  result <- print_percentiles(lissy_files, "var1")
  expect_true(all(is.na(result$value_file1)))
  expect_true(all(is.na(result$value_file2)))
})

test_that("print_percentiles handles weight parameter in compute_percentiles", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3), weight = c(1, 1, 1)),
                      file2 = data.frame(var1 = c(4, 5, 6), weight = c(1, 1, 1)))

  result <- print_percentiles(lissy_files, "var1")
  expect_is(result, "data.frame")
  expect_equal(result$percentile, seq(0, 1, 0.1))
})

test_that("print_percentiles handles weight parameter", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3), weight = c(1, 1, 1)),
                      file2 = data.frame(var1 = c(4, 5, 6), weight = c(1, 1, 1)))

  result <- print_percentiles(lissy_files, "var1", weight = "weight")
  expect_is(result, "data.frame")
  expect_equal(result$percentile, seq(0, 1, 0.1))
})

test_that("print_percentiles applies weights", {
  lissy_files <- list(file1 = data.frame(var1 = 1:100, weight = rep(1, 100)),
                      file2 = data.frame(var1 = 1:100, weight = seq(1,100, 1) ))

  result <- print_percentiles(lissy_files, "var1", weight = "weight")
  bool <- any(result$value_file1 != result$value_file2)
  expect_equal(bool, TRUE)
})

test_that("print_percentiles handles NA values in weight variable", {
  suppressWarnings({
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3), weight = c(1, NA, 1)),
                      file2 = data.frame(var1 = c(4, 5, 6), weight = c(1, 1, 1)))

  result <- print_percentiles(lissy_files, "var1", weight = "weight", na.rm = FALSE)
  expect_true(any(is.na(result$value_file1)))
  expect_true(all(!is.na(result$value_file2)))

  result <- print_percentiles(lissy_files, "var1", weight = "weight", na.rm = TRUE)
  expect_true(all(!is.na(result$value_file1)))
  expect_true(all(!is.na(result$value_file2)))
  })
})


# print_indicator ---------------------------------------------------------

test_that("print_indicator handles different types of indicators ", {
  # h-level file
  lissy_files_h <- list(file1 = data.frame(dhi = c(1, 2, 3), hwgt = c(1, 1, 1)))

  result <- print_indicator(lissy_files_h, "dhi", "mean")
  expect_equal(result, c(file1 = 2))

  result <- print_indicator(lissy_files_h, "dhi", "median")
  expect_equal(result, c(file1 = 2))

  result <- suppressWarnings(print_indicator(lissy_files_h, "dhi", "ratio"))
  expect_equal(result, c(file1 = 2.333333), tolerance = 0.1)

  result <- print_indicator(lissy_files_h, "dhi", "gini")
  expect_equal(result, c(file1 = 0.2222222), tolerance = 0.1)

  result <- print_indicator(lissy_files_h, "dhi", "atkinson", epsilon = 1)
  expect_equal(result, c(file1 = 0.0914397), tolerance = 0.1)

  # p-level file
  lissy_files_p <- list(file1 = data.frame(pi11 = c(1, 2, 3), pwgt = c(1, 1, 1)))

  result <- print_indicator(lissy_files_p, "pi11", "mean")
  expect_equal(result, c(file1 = 2))

  result <- print_indicator(lissy_files_p, "pi11", "median")
  expect_equal(result, c(file1 = 2))

  result <- suppressWarnings(print_indicator(lissy_files_p, "pi11", "ratio"))
  expect_equal(result, c(file1 = 2.333333), tolerance = 0.1)

  result <- print_indicator(lissy_files_p, "pi11", "gini")
  expect_equal(result, c(file1 = 0.2222222), tolerance = 0.1)

  result <- print_indicator(lissy_files_p, "pi11", "atkinson", epsilon = 1)
  expect_equal(result, c(file1 = 0.0914397), tolerance = 0.1)
})

test_that("print_indicator handles weight parameter", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3), weight = c(1, 1, 1)))

  result <- print_indicator(lissy_files, "var1", "mean", weight = "weight")
  expect_equal(result, c(file1 = 2))
})

test_that("print_indicator handles na.rm parameter", {
  lissy_files <- list(file1 = data.frame(dhi = c(1, 1, 1, NA), hwgt = c(1, 1, 1, NA)))

  result <- print_indicator(lissy_files, "dhi", "mean", na.rm = TRUE)
  expect_equal(result, c(file1 = 1))
})

test_that("print_indicator handles ratio and epsilon parameters", {
  lissy_files <- list(file1 = data.frame(dhi = c(1, 2, 3), hwgt = c(1, 1, 1)))

  result <- print_indicator(lissy_files, "dhi", "ratio", ratio = c(0.9, 0.1))
  expect_equal(result, c(file1 = 2.333333), tolerance = 0.01)

  result <- print_indicator(lissy_files, "dhi", "atkinson", epsilon = 0.5)
  expect_equal(result, c(file1 = 0.04491621), tolerance = 0.01)
})


test_that("print_indicator correctly handles files_level and variable_level", {
  lissy_files_h <- list(file1 = data.frame(hvar = c(1, 2, 3), hwgt = c(1, 1, 1)))

  lissy_files_p <- list(file1 = data.frame(pvar = c(1, 2, 3), pwgt = c(1, 0, 0)))


  # Test with files_level set to 'household' and variable_level set to 'person'
  result <- print_indicator(lissy_files_p, "pvar", "mean", files_level = "person", variable_level = "person")
  expect_equal(result, c(file1 = 1))

  # Test with files_level set to 'person' and variable_level set to 'household'
  result <- print_indicator(lissy_files_h, "hvar", "mean", files_level = "person", variable_level = "household")
  expect_equal(result, c(file1 = 2))

  # Test with files_level set to NULL (should use default) and variable_level set to 'person'
  result <- print_indicator(lissy_files_p, "pvar", "mean", files_level = NULL, variable_level = "person")
  expect_equal(result, c(file1 = 1))

})



# determine_weight --------------------------------------------------------

ih_list <- list(aa11ih = tibble::tibble(hid = 1:5),
                bb22ih = tibble::tibble(hid = 1:5))

ip_list <- list(aa11ip = tibble::tibble(hid = 1:5,
                                pid = 1),
                bb22ip = tibble::tibble(hid = 1:5,
                                pid = 1))

attr(ih_list, "database") <- "i"
attr(ip_list, "database") <- "i"

attr(ih_list, "level") <- "h"
attr(ip_list, "level") <- "p"


test_that("determine_weight handles missing files_level", {
  lissy_files <- list()
  variable <- "pi11"
  variable_level <- "person"

  expect_equal(determine_weight(lissy_files, variable, NULL, variable_level), "pwgt")
})

test_that("determine_weight handles missing variable_level", {
  lissy_files <- list()
  variable <- "pi11"
  files_level <- "person"

  expect_equal(determine_weight(lissy_files, variable, files_level, NULL), "pwgt")
})

test_that("determine_weight handles both files_level and variable_level", {
  lissy_files <- list()
  variable <- "pi11"
  files_level <- "person"
  variable_level <- "person"

  expect_equal(determine_weight(lissy_files, variable, files_level, variable_level), "pwgt")
})

test_that("determine_weight handles household level", {
  lissy_files <- list()
  variable <- "dhi"
  files_level <- "household"
  variable_level <- "household"

  expect_equal(determine_weight(lissy_files, variable, files_level, variable_level), "hwgt")
})

test_that("determine_weight throws error for invalid variable_level", {
  lissy_files <- list()
  variable <- "pi11"
  files_level <- "person"
  variable_level <- "invalid"

  expect_error(determine_weight(lissy_files, variable, files_level, variable_level),
               "Argument 'variable_level' can only take 'person', 'p', 'household' or 'h' as values.")
})





# ** print_gini -----------------------------------------------------------------

test_that("print_gini computes the Gini coefficient correctly", {

  # Mock data
  file1 <- data.frame(dhi = c(8,5,1,3,5,6,7,6,3), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute Gini using print_gini
  result <- print_gini(lissy_files = lissy_files, variable = "dhi")

  # Expected Gini coefficient
  expected_gini <- 0.2373737

  # Test
  expect_equal(result[["file1"]], expected_gini, tolerance = 0.0001)

})

test_that("print_gini computes the Gini coefficient correctly with weights", {

  # Mock data with weights
  file1 <- data.frame(dhi = c(8,5,1,3,5,6,7,6,3), hwgt = seq(0.1, 0.9, by = 0.1))
  lissy_files <- list(file1 = file1)

  # Compute Gini using print_gini
  result <- print_gini(lissy_files = lissy_files, variable = "dhi")

  # Expected Gini coefficient with weights
  expected_gini <- 0.2065239551478077

  # Test
  expect_equal(result[["file1"]], expected_gini, tolerance = 0.0001)

})

test_that("print_gini handles missing values correctly when na.rm = FALSE", {

  # Mock data with NA
  file1 <- data.frame(dhi = c(8,5,1,3,5,6,7,6,3, NA), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute Gini using print_gini
  result <- suppressWarnings(print_gini(lissy_files = lissy_files, variable = "dhi", na.rm = FALSE))

  # Test
  expect_true(is.na(result[["file1"]]))

})

test_that("print_gini handles missing values correctly when na.rm = TRUE", {

  # Mock data with NA
  file1 <- data.frame(dhi = c(8,5,1,3,5,6,7,6,3, NA), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute Gini using print_gini
  result <- print_gini(lissy_files = lissy_files, variable = "dhi", na.rm = TRUE)

  # Expected Gini coefficient
  expected_gini <- 0.2373737

  # Test
  expect_equal(result[["file1"]], expected_gini, tolerance = 0.0001)

})

test_that("print_gini handles different file levels correctly", {

  # Mock data for person-level and household-level
  file_person <- data.frame(pi11 = c(8,5,1,3,5,6,7,6,3), pwgt = 1)
  file_household <- data.frame(dhi = c(8,5,1,3,5,6,7,6,3), hwgt = 1, nhhmem = 1) # Added nhhmem

  lissy_files_person <- list(file_person = file_person)
  lissy_files_household <- list(file_household = file_household)

  # Compute Gini using print_gini for person-level
  result_person <- print_gini(lissy_files = lissy_files_person, variable = "pi11", files_level = "person")

  # Compute Gini using print_gini for household-level
  result_household <- print_gini(lissy_files = lissy_files_household, variable = "dhi", files_level = "household")

  # Expected Gini coefficient
  expected_gini <- 0.2373737

  # Test for person-level
  expect_equal(result_person[["file_person"]], expected_gini, tolerance = 0.0001)

  # Test for household-level
  expect_equal(result_household[["file_household"]], expected_gini, tolerance = 0.0001)

})


# ** print_atkinson -------------------------------------------------------

test_that("print_atkinson computes the Atkinson index correctly with different epsilon", {

  # Mock data
  file1 <- data.frame(dhi = c(8,5,1,3,5), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Test
  expect_equal(print_atkinson(lissy_files = lissy_files, variable = "dhi", epsilon = 1)[["file1"]], 0.183083677559, tolerance = 0.0001)
  expect_equal(print_atkinson(lissy_files = lissy_files, variable = "dhi", epsilon = 0.8)[["file1"]], 0.14249378376024, tolerance = 0.0001)
  expect_equal(print_atkinson(lissy_files = lissy_files, variable = "dhi", epsilon = 1.2)[["file1"]], 0.2248733447899 , tolerance = 0.0001)

})

test_that("print_atkinson computes the Atkinson index correctly with weights", {

  # Mock data with weights
  file1 <- data.frame(dhi = c(8,5,1,3,5), hwgt = seq(0.1, 0.5, by = 0.1))
  lissy_files <- list(file1 = file1)

  # Compute Atkinson using print_atkinson
  result <- print_atkinson(lissy_files = lissy_files, variable = "dhi", epsilon = 0.5)

  expected_atkinson <- 0.07232493

  # Test
  expect_equal(result[["file1"]], expected_atkinson, tolerance = 0.0001)

})

test_that("print_atkinson handles missing values correctly when na.rm = FALSE", {

  # Mock data with NA
  file1 <- data.frame(dhi = c(8,5,1,3,5,6,7,6,3, NA), hwgt = 1)
  lissy_files <- list(file1 = file1)

    # Compute Atkinson using print_atkinson
  result <- suppressWarnings(print_atkinson(lissy_files = lissy_files, variable = "dhi", epsilon = 1, na.rm = FALSE))

  # Test
  expect_true(is.na(result[["file1"]]))

})

test_that("print_atkinson handles missing values correctly when na.rm = TRUE", {

  # Mock data with NA
  file1 <- data.frame(dhi = c(8,5,1,3,5, NA), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute Atkinson using print_atkinson
  result <- print_atkinson(lissy_files = lissy_files, variable = "dhi", epsilon = 1, na.rm = TRUE)

  expected_atkinson <- 0.183083677559

  # Test
  expect_equal(result[["file1"]], expected_atkinson, tolerance = 0.0001)

})

test_that("print_atkinson handles different file levels correctly", {

  # Mock data for person-level and household-level
  file_person <- data.frame(pi11 = c(8,5,1,3,5), pwgt = 1)
  file_household <- data.frame(dhi = c(8,5,1,3,5), hwgt = 1, nhhmem = 1) # Added nhhmem

  lissy_files_person <- list(file_person = file_person)
  lissy_files_household <- list(file_household = file_household)

  # Compute Atkinson using print_atkinson for person-level
  result_person <- print_atkinson(lissy_files = lissy_files_person, variable = "pi11", epsilon = 1, files_level = "person")

  # Compute Atkinson using print_atkinson for household-level
  result_household <- print_atkinson(lissy_files = lissy_files_household, variable = "dhi", epsilon = 1, files_level = "household")

  expected_atkinson <- 0.183083677559

  # Test for person-level
  expect_equal(result_person[["file_person"]], expected_atkinson, tolerance = 0.0001)

  # Test for household-level
  expect_equal(result_household[["file_household"]], expected_atkinson, tolerance = 0.0001)

})


# print_ratio -------------------------------------------------------------

test_that("print_ratio computes the ratio correctly", {

  # Mock data
  file1 <- data.frame(dhi = c(0:10), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute ratio using print_ratio
  result <- print_ratio(lissy_files = lissy_files, variable = "dhi", ratio = c(0.9, 0.1))

  # Test
  expect_equal(result[["file1"]], 9, tolerance = 0.0001)

})

test_that("print_ratio computes the ratio correctly with weights", {

  # Mock data with weights
  file1 <- data.frame(dhi = c(0:10, 0:10), hwgt = c(rep(0, 11), rep(0.5, 11)))
  lissy_files <- list(file1 = file1)

  # Compute ratio using print_ratio
  result <- print_ratio(lissy_files = lissy_files, variable = "dhi", ratio = c(0.9, 0.1))

  # Test
  expect_equal(result[["file1"]], 9, tolerance = 0.0001)

})

test_that("print_ratio handles missing values correctly when na.rm = FALSE", {

  # Mock data with NA
  file1 <- data.frame(dhi = c(0:10, NA), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute ratio using print_ratio
  result <- suppressWarnings(print_ratio(lissy_files = lissy_files, variable = "dhi", ratio = c(0.9, 0.1), na.rm = FALSE))

  # Test
  expect_true(is.na(result[["file1"]]))

})

test_that("print_ratio handles missing values correctly when na.rm = TRUE", {

  # Mock data with NA
  file1 <- data.frame(dhi = c(0:10, NA), hwgt = 1)
  lissy_files <- list(file1 = file1)

  # Compute ratio using print_ratio
  result <- print_ratio(lissy_files = lissy_files, variable = "dhi", ratio = c(0.9, 0.1), na.rm = TRUE)

  # Test
  expect_equal(result[["file1"]], 9, tolerance = 0.0001)

})

test_that("print_ratio handles different file levels correctly", {

  # Mock data for person-level and household-level
  file_person <- data.frame(pi11 = 0:10, pwgt = 1)
  file_household <- data.frame(dhi = 0:10, hwgt = 1, nhhmem = 1) # Added nhhmem

  lissy_files_person <- list(file_person = file_person)
  lissy_files_household <- list(file_household = file_household)

  # Compute ratio using print_ratio for person-level
  result_person <- print_ratio(lissy_files = lissy_files_person, variable = "pi11", ratio = c(0.9, 0.1), files_level = "person")

  # Compute ratio using print_ratio for household-level
  result_household <- print_ratio(lissy_files = lissy_files_household, variable = "dhi", ratio = c(0.9, 0.1), files_level = "household")

  # Test for person-level
  expect_equal(result_person[["file_person"]], 9, tolerance = 0.0001)

  # Test for household-level
  expect_equal(result_household[["file_household"]], 9, tolerance = 0.0001)

})


