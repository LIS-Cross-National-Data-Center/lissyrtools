# test-print.R

library(testthat)
library(lissyrtools)

.run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")



# ** gini -----------------------------------------------------------------

test_that("print_gini returns expected Gini coefficients", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3), weight = c(1, 1, 1)),
                      file2 = data.frame(var1 = c(4, 5, 6), weight = c(1, 1, 1)))

  result <- print_gini(lissy_files, "var1", na.rm = FALSE)
  expect_equal(class(result), "numeric")
  expect_named(result, c("file1", "file2"))
})

test_that("print_gini handles NA values correctly", {

  suppressWarnings({
    lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3, NA), weight = c(1, 1, 1, 1)),
                        file2 = data.frame(var1 = c(4, 5, 6, NA), weight = c(1, 1, 1, 1)))

    result <- print_gini(lissy_files, "var1", na.rm = FALSE)
    expect_true(any(is.na(result)))

    result <- print_gini(lissy_files, "var1", na.rm = TRUE)
    expect_true(all(!is.na(result)))
  })

})

test_that("print_gini handles all NA or zero values in variable", {
  lissy_files <- list(file1 = data.frame(var1 = c(NA_integer_, NA_integer_, NA_integer_), weight = c(1, 1, 1)),
                      file2 = data.frame(var1 = c(0, 0, 0), weight = c(1, 1, 1)))

  result <- print_gini(lissy_files, "var1", na.rm = FALSE)
  expect_true(all(is.na(result)))
})


# ** atkinson -------------------------------------------------------------

# test_that("print_indicator uses the right weight when computing atkinson with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing atkinson with a h-level file", {
#
#
#
#
# })


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

# ** average --------------------------------------------------------------

# test_that("print_indicator uses the right weight when computing mean with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing mean with a h-level file", {
#
#
#
#
# })


# ** median ---------------------------------------------------------------


# test_that("print_indicator uses the right weight when computing median with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing median with a h-level file", {
#
#
#
#
# })




# ** relative poverty -----------------------------------------------------






# ** using p-level files -------------------------------------------------

# **** mean ---------------------------------------------------------------

# prints a warning if ratio or epsilon arguments are passed

# **** median -------------------------------------------------------------
# prints a warning if ratio or epsilon arguments are passed


# **** ratio --------------------------------------------------------------

# prints a warning if epsilon argument is passed


# **** gini ---------------------------------------------------------------

# prints a warning if ratio or epsilon arguments are passed

# **** atkinson -----------------------------------------------------------

# prints a warning if ratio argument is passed



# ** using hh-level files -------------------------------------------------


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
