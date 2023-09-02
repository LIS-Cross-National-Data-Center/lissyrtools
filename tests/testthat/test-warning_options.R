# # test-warning_options.R
#
# library(lissyrtools)
# library(testthat)
#
#
#
# test_that("missing_values_in_variable_warning does not throw a warning if there are no NAs in variable", {
#
#   file_1 <- tibble::tibble(dhi = 1:10)
#
#   expect_equal(missing_values_in_variable_warning(file_1, "zz55ih", "dhi"),
#                NULL)
#
#   expect_warning(missing_values_in_variable_warning(file_1, "zz55ih", "dhi"),
#                  regexp = NA)
#
# })
#
#
# test_that("missing_values_in_variable_warning does not store an option if there are no NAs in variable", {
#
#   file_1 <- tibble::tibble(dhi = 1:10)
#
#   missing_values_in_variable_warning(file_1, "zz55ih", "dhi")
#
#   expect_equal(getOption("zz55ih_warning_NAs_dhi"), NULL)
#
# })
#
#
# test_that("missing_values_in_variable_warning throws a warning (only) if there are missing values", {
#
#   file_1 <- tibble::tibble(dhi = 1:10)
#
#   file_2 <- tibble::tibble(dhi = c(1:9, NA))
#
#   expect_warning(missing_values_in_variable_warning(file_1, "zz55ih", "dhi"),
#                  regexp = NA)
#
#   expect_warning(missing_values_in_variable_warning(file_2, "zz55ih", "dhi"),
#                  regexp = "The variable 'dhi' contains missing values in 'zz55ih'.",
#                  fixed = TRUE)
#
# })
#
#
# test_that("missing_values_in_variable_warning stores an option if the warning is triggered", {
#
#   file_1 <- tibble::tibble(pi11 = c(1:9, NA))
#
#   missing_values_in_variable_warning(file_1, "zz55ip", "pi11")
#
#   expect_equal(getOption("zz55ip_warning_NAs_pi11"), TRUE)
#
# })
#
#
# test_that("missing_values_in_variable_warning does not throw a warning if an option exists", {
#
#   file_1 <- tibble::tibble(pi12 = c(1:9, NA))
#
#   missing_values_in_variable_warning(file_1, "zz55ip", "pi12")
#
#   expect_equal(getOption("zz55ip_warning_NAs_pi12"), TRUE)
#
#   expect_warning(missing_values_in_variable_warning(file_1, "zz55ip", "pi12"),
#                  regexp = NA)
#
# })
