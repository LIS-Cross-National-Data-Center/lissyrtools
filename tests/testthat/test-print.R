# test-print.R

library(testthat)
library(lissyrtools)

.run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")


# print_indicator ---------------------------------------------------------

# copy tests from 'test-transformations' for
# test when variable has only NAS

# try also with single file list

# ** gini -----------------------------------------------------------------

library(lissyrtools)
context("Helper functions for computing estimates")
set.seed(4)


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

# test_that("print_indicator uses the right weight when computing percentiles with a p-level file", {
#
#
#
#
# })
#
#
# test_that("print_indicator uses the right weight when computing percentiles with a h-level file", {
#
#
#
#
# })

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


