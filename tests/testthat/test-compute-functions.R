# library(lissyrtools)
# context("Functions for computing estimates")
#
# set.seed(4)
#
# .run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")
#
#
# if(.run_local_tests){
#
# options(JULIA_HOME = "/home/josep/.local/bin/julia/bin/")
#
# }
#
# # compute_percentiles -----------------------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("compute_percentiles works well with minimum specified arguments", {
#
#   file_ <- tibble::tibble(var_ = c(seq(0, 10)))
#
#   expect_equal(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_")[["value"]],
#                seq(0, 10))
#
# })
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("compute_percentiles works well without with na.rm specified to TRUE", {
#
#   file_ <- tibble::tibble(var_ = c(seq(0, 10)))
#
#   expect_equal(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = TRUE)[["value"]],
#                seq(0, 10))
#
# })
#
#
#
# test_that("compute_percentiles ignores NAs in variable if na.rm = TRUE", {
#
#   file_ <- tibble::tibble(var_ = c(NA, seq(0, 10), NA))
#
#   expect_equal(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = TRUE)[["value"]],
#                seq(0, 10))
#
#   expect_equal(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = TRUE)[["percentile"]],
#                seq(0, 1, 0.1))
#
# })
#
#
# test_that("compute_percentiles works well with specified weight variable", {
#
#   file_ <- tibble::tibble(var_ = c(50, seq(0, 10)), my_weight = c(0, rep(1, 11)))
#
#   expect_equal(compute_percentiles(file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "my_weight",
#                                    na.rm = TRUE)[["value"]],
#                seq(0, 10))
#
# })
#
#
# test_that("compute_percentiles ignores NAs in weight if na.rm = TRUE and weight is specified", {
#
#   file_ <- tibble::tibble(var_ = c(50, seq(0, 10)),
#                           pwgt = c(NA, rep(1, 11)))
#
#   expect_equal(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "pwgt",
#                                    na.rm = TRUE)[["value"]],
#                seq(0, 10))
#
# })
#
#
# test_that("compute_percentiles works well when weights are small or don't have mean equal to 1", {
#
#   file_1_a <- tibble::tibble(var_ = seq(0, 10),
#                              my_weight = c(seq(1, 11)))
#   file_1_b <- tibble::tibble(var_ = seq(0, 10),
#                              my_weight = c(seq(1, 11))*0.00001)
#   file_1_c <- tibble::tibble(var_ = seq(0, 10),
#                              my_weight = c(seq(1, 11))*10000)
#
#   expect_equal(compute_percentiles(file_1_a,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "my_weight",
#                                    na.rm = TRUE)[["value"]],
#                compute_percentiles(file_1_b,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "my_weight",
#                                    na.rm = TRUE)[["value"]])
#
#   expect_equal(compute_percentiles(file_1_a,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "my_weight",
#                                    na.rm = TRUE)[["value"]],
#                compute_percentiles(file_1_c,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "my_weight",
#                                    na.rm = TRUE)[["value"]])
#
#
# })
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("compute_percentiles throws a warning and returns NAs if there are NAs and na.rm = FALSE", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, seq(0,10), NA))
#
#   file_2 <- tibble::tibble(var_ = seq(0,10),
#                            pwgt = c(NA, rep(1, 10)))
#
#
#   expect_warning(compute_percentiles(file_1,
#                                    file_name = "aa55i",
#                                   variable = "var_",
#                                   breaks = seq(0, 1, 0.1),
#                                   na.rm = FALSE),
#                "'var_' in 'aa55i' contains NAs. Use na.rm = TRUE to ignore them.",
#                fixed = TRUE)
#
#   expect_warning(compute_percentiles(file_2,
#                                      file_name = "aa55i",
#                                   variable = "var_",
#                                   breaks = seq(0, 1, 0.1),
#                                   weight = "pwgt",
#                                   na.rm = FALSE),
#                  "There are NAs in the weighting variable 'pwgt' in 'aa55i'. Use na.rm = TRUE to ignore them.")
#
#
#   expect_equal(suppressWarnings(compute_percentiles(file_1,
#                                      file_name = "aa55i",
#                                      variable = "var_",
#                                      breaks = seq(0, 1, 0.1),
#                                      na.rm = FALSE)),
#                  NA)
#
#   expect_equal(suppressWarnings(compute_percentiles(file_2,
#                                      file_name = "aa55i",
#                                      variable = "var_",
#                                      breaks = seq(0, 1, 0.1),
#                                      weight = "pwgt",
#                                      na.rm = FALSE)),
#                NA)
#
# })
#
#
#
# test_that("compute_percentiles returns NA if all values in 'variable' are NAs", {
#
#   file_ <- tibble::tibble(var_ = rep(NA_real_, 3), hwgt = 1)
#
#   expect_equal(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = TRUE)[["value"]],
#                rep(NA_real_, 11))
#
# })
#
#
# # ** wrong or missing parameters ------------------------------------------
#
# test_that("compute_percentiles throws an error if 'variable' is a character vector", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, letters[1:10], NA), hwgt = 1)
#   file_2 <- tibble::tibble(var_ = letters[1:10], hwgt = 1)
#
#   expect_error(compute_percentiles(file = file_1,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = FALSE),
#                "The argument 'variable' needs to be numeric. 'var_' in 'aa55i' is not numeric.",
#                fixed = TRUE)
#
#   expect_error(compute_percentiles(file = file_2,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = FALSE),
#                "The argument 'variable' needs to be numeric. 'var_' in 'aa55i' is not numeric.",
#                fixed = TRUE)
#
# })
#
#
#
# test_that("compute_percentiles throws error if 'file' is an empty data frame with 0 rows", {
#
#   file_ <- tibble::tibble(var_ = numeric(0), hwgt = numeric(0))
#
#   expect_error(compute_percentiles(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = FALSE),
#                regexp = "Argument 'file' in 'aa55i' should be have 1 or more rows.",
#                fixed = TRUE)
#
# })
#
#
#
# test_that("compute_percentiles fails when 'variable' or 'weight' are not in dataset", {
#
#   file_1 <- tibble::tibble(hwgt = rep(1, times = 3))
#
#   file_2 <- tibble::tibble(var_ = c(seq(0,10)))
#
#   expect_error(compute_percentiles(file_1,
#                                    file_name = "aa55ih",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    na.rm = FALSE),
#                regexp = "'var_' was not found in 'aa55ih'.",
#                fixed = TRUE)
#
#   expect_error(compute_percentiles(file_2,
#                                    file_name = "aa55ih",
#                                    variable = "var_",
#                                    breaks = seq(0, 1, 0.1),
#                                    weight = "hwgt",
#                                    na.rm = FALSE),
#                regexp = "'hwgt' was not found in 'aa55ih'.",
#                fixed = TRUE)
#
# })
#
#
#
# test_that("compute_percentiles throws an eror NA if all values in 'weight' are NAs or 0s", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = rep(NA, 11))
#
#   file_2 <- tibble::tibble(var_ = seq(0, 10), hwgt = rep(0, 11))
#
#   expect_error(
#     compute_percentiles(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     "The variable for weights ('hwgt') contains only NAs and/or 0s. The equivalised percentiles could not be computed.",
#     fixed = TRUE)
#
#   expect_error(
#     compute_percentiles(file = file_1,
#                         file_name = "zz55i",
#                         variable = "var_",
#                         weight = "hwgt",
#                         na.rm = FALSE),
#     "The variable for weights ('hwgt') contains only NAs and/or 0s. The equivalised percentiles could not be computed.",
#     fixed = TRUE)
#
#   expect_error(
#     compute_percentiles(file = file_2,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     "The variable for weights ('hwgt') contains only NAs and/or 0s. The equivalised percentiles could not be computed.",
#     fixed = TRUE)
#
# })
#
#
# # compute_gini ------------------------------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("compute_gini works well with minimum specified arguments", {
#
#   expect_equal(compute_gini(file = mock_dataset,
#                             file_name = "zz55i",
#                             variable = "mock_variable"),
#                0.3339987,
#                tolerance = 0.001)
#
# })
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("compute_gini works well without with na.rm specified to TRUE", {
#
#   file_ <- tibble::tibble(var_ = c(seq(0, 10)))
#
#   expect_equal(compute_gini(file = mock_dataset,
#                                    file_name = "zz55i",
#                                    variable = "mock_variable_NAs",
#                                    na.rm = TRUE),
#                0.3332986,
#                tolerance = 0.001)
#
# })
#
#
#
#
# # add to test:
# # more breaks than data points?
#
# # https://github.com/tidyverse/dplyr/blob/master/tests/testthat/test-rank.R
# # test_that("ntile() does not overflow (#4186)", {
# #   res <- tibble(a = 1:1e5) %>%
# #     mutate(b = ntile(n = 1e5)) %>%
# #     count(b) %>%
# #     pull()
# #
# #   expect_true(all(res == 1L))
# # })
#
#
# # puts large groups first
# # with a warning!
#
#
#
# ## test that compute_lorenz_curve works well when there are repeated quantiles (e.g. fr84)
#
#
# # compute_lorenz_curve ----------------------------------------------------
# #
# # test_that("compute_lorenz returns the expected result for a simple vector with no NAs nor repeated percentiles", {
# #
# #   expect_equal(compute_lorenz_curve(file = mock_dataset,
# #                                     file_name = "zz55",
# #                                   variable = "mock_variable",
# #                              n_percentiles = 100,
# #                              na.rm = FALSE)[["lorenz_curve_values"]],
# #               results_mock_dataset_100)
# #
# #   expect_equal(compute_lorenz_curve(file = mock_dataset,
# #                                     file_name = "zz55",
# #                                     variable = "mock_variable",
# #                                     n_percentiles = 50,
# #                                     na.rm = FALSE)[["lorenz_curve_values"]],
# #               results_mock_dataset_50)
# #
# # })
#
#
# #
# # test_that("compute_lorenz properly deals with leading 0s in percentiles", {
# #
# #   expect_equal(compute_lorenz_curve(file = mock_dataset,
# #                                     file_name = "zz55",
# #                                     variable = "mock_variable_leading_0s",
# #                                     n_percentiles = 100,
# #                                     na.rm = FALSE)[1:5,"lorenz_curve_values", drop = TRUE],
# #               c(rep(0, 5))
# #               )
# #
# #   expect_equal(compute_lorenz_curve(file = mock_dataset,
# #                                     file_name = "zz55",
# #                                     variable = "mock_variable_leading_0s",
# #                                     n_percentiles = 50,
# #                                     na.rm = FALSE)[1:2,"lorenz_curve_values", drop = TRUE],
# #               c(rep(0, 2))
# #               )
# #
# # })
#
#
# # test_that("compute_lorenz returns properly deals with repeated percentiles in negative values", {
# #
# #
# # })
#
#
#
# # test_that("compute_lorenz returns NA if there are missing variables and na.rm = FALSE", {
# #
# #   expect_equal(compute_lorenz_curve(file = mock_dataset,
# #                                     file_name = "zz55",
# #                                     variable = "mock_variable_NAs",
# #                                     n_percentiles = 100,
# #                                     na.rm = FALSE), NA
# #               )
# #
# #   expect_warning(compute_lorenz_curve(file = mock_dataset,
# #                                       file_name = "zz55",
# #                                     variable = "mock_variable_NAs",
# #                                     n_percentiles = 50,
# #                                     na.rm = FALSE)
# #                         )
# # })
#
#
#
# ## TO DO: thest that compute_lorenz_curve computes the requested number of percentiles
#
#
#
# # Compute standard indicators ---------------------------------------------
#
#
# # ** compute_mean ---------------------------------------------------------
#
# test_that("compute_mean returns the correct output when weight argument is NULL", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#
#   expect_equal(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             weight = NULL,
#                             na.rm = FALSE),
#                5)
#
# })
#
#
# test_that("compute_mean throws an error when weight variable is missing", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10))
#
#   expect_error(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             weight = "hwgt",
#                             na.rm = FALSE),
#                "'hwgt' was not found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("compute_mean throws an error when 'variable' is missing", {
#
#   file_1 <- tibble::tibble(var2_ = seq(0, 10))
#
#   expect_error(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             na.rm = FALSE),
#                "'var_' was not found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("compute_mean returns the correct output when weights are constant and not constant", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#   file_2 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(2, 0.5, 2, 0.5,
#                                        2, 1, 2, 0.5,
#                                        2, 0.5, 2))
#   file_3 <- tibble::tibble(var_ = seq(0, 10),
#                            pwgt = c(2, 0.5, 2, 0.5, # change name of weight variable
#                                     2, 1, 2, 0.5,
#                                     2, 0.5, 2))
#   file_4 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(rep(1, 6), rep(2, 5)))
#
#   expect_equal(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_mean(file = file_2,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_mean(file = file_3,
#                             file_name = "zz55i",
#                             weight = "pwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_mean(file = file_4,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5.9375)
#
# })
#
#
# test_that("compute_mean works well when weights are small or don't have mean equal to 1", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1*0.0000001)
#   file_2 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(2, 0.5, 2, 0.5,
#                                        2, 1, 2, 0.5,
#                                        2, 0.5, 2)*0.00000001)
#   file_3 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(rep(1, 6), rep(2, 5))*0.00000001)
#
#   expect_equal(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_mean(file = file_2,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_mean(file = file_3,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5.9375)
#
# })
#
#
# test_that("compute_mean returns NA if there are missing variables and na.rm = FALSE", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, seq(1, 10)), hwgt = 1)
#   file_2 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(NA, rep(1, 10)))
#   file_3 <- tibble::tibble(var_ = c(NA, seq(1, 10)),
#                               hwgt = c(NA, rep(1, 10)))
#
#   expect_equal(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                NA_real_)
#
#   expect_equal(compute_mean(file = file_2,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                NA_real_)
#
#   expect_equal(compute_mean(file = file_3,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                NA_real_)
#
#   expect_equal(compute_mean(file = file_1,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = TRUE),
#                5.5)
#
#   expect_equal(compute_mean(file = file_2,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = TRUE),
#                5.5)
#
#   expect_equal(compute_mean(file = file_3,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             na.rm = TRUE),
#                5.5)
#
# })
#
#
# # ** compute_median -------------------------------------------------------
#
# test_that("compute_median returns the correct output when weight argument is NULL", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#
#   expect_equal(compute_median(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_median(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             weight = NULL,
#                             na.rm = FALSE),
#                5)
#
# })
#
#
# test_that("compute_median throws an error when weight variable is missing", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10))
#
#   expect_error(compute_median(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             weight = "hwgt",
#                             na.rm = FALSE),
#                "'hwgt' was not found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("compute_median throws an error when 'variable' is missing", {
#
#   file_1 <- tibble::tibble(var2_ = seq(0, 10))
#
#   expect_error(compute_median(file = file_1,
#                             file_name = "zz55i",
#                             variable = "var_",
#                             na.rm = FALSE),
#                "'var_' was not found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
# test_that("compute_median returns the correct numeric output when weights are constant and not constant", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#   file_2 <- tibble::tibble(var_ = seq(0, 10), pwgt = 1)
#   file_3 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(2, 0.5, 2, 0.5,
#                                        2, 1, 2, 0.5,
#                                        2, 0.5, 2))
#   file_4 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(rep(1, 6), rep(2, 5)))
#
#   expect_equal(compute_median(file = file_1,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_median(file = file_2,
#                               file_name = "zz55i",
#                               weight = "pwgt",
#                               variable = "var_",
#                               na.rm = FALSE),
#                5)
#
#   expect_equal(compute_median(file = file_3,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                5)
#
#   expect_equal(compute_median(file = file_4,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                6.5)
#
# })
#
#
# test_that("compute_median returns NA if there are missing variables and na.rm = FALSE", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, seq(1, 10)),
#                            hwgt = 1)
#   file_2 <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(NA, rep(1, 10)))
#   file_3 <- tibble::tibble(var_ = c(NA, seq(1, 10)),
#                               hwgt = c(NA, rep(1, 10)))
#
#   expect_equal(compute_median(file = file_1,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                NA)
#
#   expect_equal(compute_median(file = file_2,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                NA)
#
#   expect_equal(compute_median(file = file_3,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = FALSE),
#                NA)
#
#   expect_equal(compute_median(file = file_1,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = TRUE),
#                5.5)
#
#   expect_equal(compute_median(file = file_2,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = TRUE),
#                5.5)
#
#   expect_equal(compute_median(file = file_3,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                             variable = "var_",
#                             na.rm = TRUE),
#                5.5)
#
# })
#
#
# test_that("compute_median works well when weights are small or don't have mean equal to 1", {
#
#   file_1_a <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#   file_1_b <- tibble::tibble(var_ = seq(0, 10), hwgt = 1*0.0000001)
#
#   file_2_a <- tibble::tibble(var_ = seq(0, 10),
#                                 hwgt = c(2, 0.5, 2, 0.5,
#                                          2, 1, 2, 0.5,
#                                          2, 0.5, 2))
#   file_2_b <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(2, 0.5, 2, 0.5,
#                                        2, 1, 2, 0.5,
#                                        2, 0.5, 2)*0.00000001)
#
#   file_3_a <- tibble::tibble(var_ = seq(0, 10),
#                               hwgt = c(rep(1, 6), rep(2, 5)))
#   file_3_b <- tibble::tibble(var_ = seq(0, 10),
#                                 hwgt = c(rep(1, 6), rep(2, 5))*0.00000001)
#
#   expect_equal(compute_median(file = file_1_a,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                               variable = "var_",
#                               na.rm = FALSE),
#                compute_median(file = file_1_b,
#                               file_name = "zz55i",
#                               weight = "hwgt",
#                               variable = "var_",
#                               na.rm = FALSE))
#
#   expect_equal(compute_median(file = file_2_a,
#                               file_name = "zz55i",
#                               variable = "var_",
#                               weight = "hwgt",
#                               na.rm = FALSE),
#                compute_median(file = file_2_b,
#                               file_name = "zz55i",
#                               variable = "var_",
#                               weight = "hwgt",
#                               na.rm = FALSE))
#
#   expect_equal(compute_median(file = file_3_a,
#                               file_name = "zz55i",
#                               variable = "var_",
#                               weight = "hwgt",
#                               na.rm = FALSE),
#                compute_median(file = file_3_b,
#                               file_name = "zz55i",
#                               variable = "var_",
#                               weight = "hwgt",
#                               na.rm = FALSE))
#
# })
#
#
# # compute_ratio --------------------------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("compute_ratio returns the correct output when weight argument is NULL", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              variable = "var_",
#                              ratio = c(0.9, 0.1),
#                              na.rm = FALSE),
#                9/1)
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              variable = "var_",
#                              weight = NULL,
#                              ratio = c(0.9, 0.1),
#                              na.rm = FALSE),
#                9/1)
#
# })
#
#
# test_that("compute_ratio returns the correct output when ratio is not specified", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              variable = "var_",
#                              na.rm = FALSE),
#                9/1)
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              variable = "var_",
#                              weight = NULL,
#                              na.rm = FALSE),
#                9/1)
#
# })
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("compute_ratio returns the correct numeric output when weights are constant and not constant", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#   file_2 <- tibble::tibble(var_ = seq(0, 10), pwgt = 1)
#   file_3 <- tibble::tibble(var_ = seq(0, 10),
#                            hwgt = c(2, 0.5, 2, 0.5,
#                                     2, 1, 2, 0.5,
#                                     2, 0.5, 2))
#   file_4 <- tibble::tibble(var_ = seq(0, 10),
#                            hwgt = c(rep(1, 6), rep(2, 5)))
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.1),
#                              na.rm = FALSE),
#                9/1)
#
#   expect_equal(compute_ratio(file = file_2,
#                              file_name = "zz55i",
#                              weight = "pwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.1),
#                              na.rm = FALSE),
#                9/1)
#
#   expect_equal(compute_ratio(file = file_3,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                1.666667,
#                tolerance = 0.001)
#
#   expect_equal(compute_ratio(file = file_4,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                1.428571,
#                tolerance = 0.001)
#
# })
#
#
# test_that("compute_ratio works well when weights are small or don't have mean equal to 1", {
#
#   file_1_a <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#   file_1_b <- tibble::tibble(var_ = seq(0, 10), hwgt = 1*0.0000001)
#
#   file_2_a <- tibble::tibble(var_ = seq(0, 10),
#                              hwgt = c(2, 0.5, 2, 0.5,
#                                       2, 1, 2, 0.5,
#                                       2, 0.5, 2))
#   file_2_b <- tibble::tibble(var_ = seq(0, 10),
#                              hwgt = c(2, 0.5, 2, 0.5,
#                                       2, 1, 2, 0.5,
#                                       2, 0.5, 2)*0.00000001)
#   file_2_c <- tibble::tibble(var_ = seq(0, 10),
#                              hwgt = c(2, 0.5, 2, 0.5,
#                                       2, 1, 2, 0.5,
#                                       2, 0.5, 2)*1000000)
#
#   file_3_a <- tibble::tibble(var_ = seq(0, 10),
#                              hwgt = c(rep(1, 6), rep(2, 5)))
#   file_3_b <- tibble::tibble(var_ = seq(0, 10),
#                              hwgt = c(rep(1, 6), rep(2, 5))*0.00000001)
#
#   expect_equal(compute_ratio(file = file_1_a,
#                             file_name = "zz55i",
#                             weight = "hwgt",
#                             variable = "var_",
#                             ratio = c(0.9, 0.5),
#                             na.rm = FALSE),
#                compute_ratio(file = file_1_b,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#               tolerance = 0.001)
#
#   expect_equal(compute_ratio(file = file_2_a,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                compute_ratio(file = file_2_b,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                tolerance = 0.001)
#
#
#   expect_equal(compute_ratio(file = file_2_a,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                compute_ratio(file = file_2_c,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                tolerance = 0.001)
#
#   expect_equal(compute_ratio(file = file_3_a,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                compute_ratio(file = file_3_b,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9, 0.5),
#                              na.rm = FALSE),
#                tolerance = 0.001)
#
# })
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("compute_ratio throws a warning and returns NAs if there are NAs in variable and na.rm = FALSE (default)", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, seq(1, 10)),
#                            hwgt = c(2, 0.5, 2, 0.5,
#                                     2, 1, 2, 0.5,
#                                     2, 0.5, 2))
#
#   expect_warning(
#     compute_ratio(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = FALSE),
#     "'var_' in 'zz55i' contains NAs. Use na.rm = TRUE to ignore them."
#                  )
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              variable = "var_",
#                              weight = "hwgt",
#                              na.rm = FALSE), NA)
#
#
# })
#
#
# test_that("compute_ratio throws a warning and returns NAs if there are NAs in weights and na.rm = FALSE", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10),
#                            hwgt = c(NA, 0.5, 2, 0.5,
#                                     2, 1, 2, 0.5,
#                                     2, 0.5, 2))
#
#   expect_warning(
#     compute_ratio(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = FALSE),
#     "There are NAs in the weighting variable 'hwgt' in 'zz55i'. Use na.rm = TRUE to ignore them."
#   )
#
#   expect_equal(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              variable = "var_",
#                              weight = "hwgt",
#                              na.rm = FALSE), NA)
#
# })
#
#
# test_that("compute_ratio excludes NAs in variable when na.rm = TRUE", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, seq(0, 10)), hwgt = 1)
#
#   expect_equal(
#     compute_ratio(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     9)
# })
#
#
# test_that("compute_ratio excludes NAs in weights when na.rm = TRUE", {
#
#   file_1 <- tibble::tibble(var_ = c(999, seq(0, 10)), hwgt = c(NA, rep(1, 11)))
#
#   expect_equal(
#     compute_ratio(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     9)
# })
#
#
# test_that("compute_ratio returns NA if all values in 'variable' are NAs", {
#
#   file_1 <- tibble::tibble(var_ = rep(NA_real_, 10), hwgt = 1)
#
#   expect_equal(
#     compute_ratio(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     NA_real_)
#
# })
#
# test_that("compute_ratio throws an eror NA if all values in 'weight' are NAs or 0s", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = rep(NA, 11))
#
#   file_2 <- tibble::tibble(var_ = seq(0, 10), hwgt = rep(0, 11))
#
#   expect_error(
#     compute_ratio(file = file_1,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     "The variable for weights ('hwgt') contains only NAs and/or 0s. The equivalised percentiles could not be computed.",
#     fixed = TRUE)
#
#   expect_error(
#     compute_ratio(file = file_2,
#                   file_name = "zz55i",
#                   variable = "var_",
#                   weight = "hwgt",
#                   na.rm = TRUE),
#     "The variable for weights ('hwgt') contains only NAs and/or 0s. The equivalised percentiles could not be computed.",
#     fixed = TRUE)
#
# })
#
#
# # ** wrong or missing parameters ------------------------------------------
#
# test_that("compute_ratio throws an error when weight variable is missing", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10))
#
#   expect_error(compute_ratio(file = file_1,
#                               file_name = "zz55i",
#                               variable = "var_",
#                               weight = "hwgt",
#                              ratio = c(0.9, 0.1),
#                               na.rm = FALSE),
#                "'hwgt' was not found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("compute_ratio throws an error when 'variable' is missing", {
#
#   file_1 <- tibble::tibble(var2_ = seq(0, 10))
#
#   expect_error(compute_ratio(file = file_1,
#                               file_name = "zz55i",
#                               variable = "var_",
#                              ratio = c(0.9, 0.1),
#                               na.rm = FALSE),
#                "'var_' was not found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("compute_ratio throws an error if values of ratio are incorrect", {
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10), hwgt = 1)
#
#   expect_error(
#     compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(0.9),
#                              na.rm = FALSE),
#     "'ratio' should be a vector with length 2."
#                              )
#
#   expect_error(compute_ratio(file = file_1,
#                              file_name = "zz55i",
#                              weight = "hwgt",
#                              variable = "var_",
#                              ratio = c(1.1, 0.5),
#                              na.rm = FALSE),
#                "Values in 'ratio' must be between 0 and 1.")
#
# })
#
#
# # compute_atkinson -----------------------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("compute_atkinson works well with minimum specified arguments", {
#
#   file_ <- tibble::tibble(var_ = c(seq(1, 100)))
#
#   # epsilon < 1
#   expect_equal(compute_atkinson(file = file_,
#                                    file_name = "aa55i",
#                                    variable = "var_",
#                                 epsilon = 0.9),
#                0.2164242,
#                tolerance = 0.0001)
#
#   # epsilon == 1
#   expect_equal(compute_atkinson(file = file_,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 1),
#                0.2476695,
#                tolerance = 0.0001)
#
#   # epsilon > 1
#   expect_equal(compute_atkinson(file = file_,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 1.1),
#                0.2805721,
#                tolerance = 0.0001)
#
# })
#
#
# test_that("compute_atkinson returns the correct output when variable includes 0s and negative values", {
#
#   file_1 <- tibble::tibble(var_ = c(seq(0, 500)))
#
#   # epsilon < 1
#   expect_equal(compute_atkinson(file = file_1,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 0.9),
#                0.2395666,
#                tolerance = 0.0001)
#
#   # epsilon == 1
#   expect_equal(compute_atkinson(file = file_1,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 1),
#                0.2597727,
#                tolerance = 0.0001)
#
#   # epsilon > 1
#   expect_equal(compute_atkinson(file = file_1,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 1.1),
#                0.2961865,
#                tolerance = 0.0001)
# })
#
#
# test_that("compute_atkinson throws a warning if 0s are included in the variable and epsilon >= 1", {
#
#   file_ <- tibble::tibble(var_ = c(seq(0, 100)))
#
#   # epsilon == 1
#   expect_warning(compute_atkinson(file = file_,
#                                   file_name = "aa55i",
#                                   variable = "var_",
#                                   epsilon = 1),
#                  "0s in 'var_' were removed before computing the Atkinson index for 'aa55i'. 0s are not allowed if epsilon >= 1")
#
#   # epsilon > 1
#   expect_warning(compute_atkinson(file = file_,
#                                   file_name = "aa55i",
#                                   variable = "var_",
#                                   epsilon = 1.1),
#                  "0s in 'var_' were removed before computing the Atkinson index for 'aa55i'. 0s are not allowed if epsilon >= 1")
#
# })
#
#
#
#
# # ** user-specified arguments ---------------------------------------------
#
# # weights
#
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("compute_atkinson throws a warning and returns NAs if there are NAs in variable and na.rm = FALSE (default)", {
#
#   file_1 <- tibble::tibble(var_ = c(NA, seq(1, 10)),
#                            hwgt = c(2, 0.5, 2, 0.5,
#                                     2, 1, 2, 0.5,
#                                     2, 0.5, 2))
#
#   expect_equal(compute_atkinson(file = file_1,
#                                   file_name = "zz55i",
#                                   weight = "hwgt",
#                                   variable = "var_",
#                                   epsilon = 1,
#                                   na.rm = FALSE),
#                  NA_real_)
#
#   expect_equal(compute_atkinson(file = file_1,
#                                   file_name = "zz55i",
#                                   weight = "hwgt",
#                                   variable = "var_",
#                                   epsilon = 1),
#                NA_real_)
#
#   expect_warning(compute_atkinson(file = file_1,
#                                 file_name = "zz55i",
#                                 weight = "hwgt",
#                                 variable = "var_",
#                                 epsilon = 1,
#                                 na.rm = FALSE),
#                "'var_' in 'zz55i' contains NAs. Use na.rm = TRUE to ignore them.")
#
#   expect_warning(compute_atkinson(file = file_1,
#                                 file_name = "zz55i",
#                                 weight = "hwgt",
#                                 variable = "var_",
#                                 epsilon = 1),
#                "'var_' in 'zz55i' contains NAs. Use na.rm = TRUE to ignore them.")
#
#
# })
#
#
# test_that("throws a warning and returns NAs if there are NAs in weights and na.rm = FALSE",{
#
#   file_1 <- tibble::tibble(var_ = seq(0, 10),
#                            hwgt = c(NA, 0.5, 2, 0.5,
#                                     2, 1, 2, 0.5,
#                                     2, 0.5, 2))
#
#   expect_equal(compute_atkinson(file = file_1,
#                                   file_name = "zz55i",
#                                   weight = "hwgt",
#                                   variable = "var_",
#                                   epsilon = 1,
#                                   na.rm = FALSE),
#                NA_real_)
#
#   expect_equal(compute_atkinson(file = file_1,
#                                   file_name = "zz55i",
#                                   weight = "hwgt",
#                                   variable = "var_",
#                                   epsilon = 1),
#                NA_real_)
#
#   expect_warning(compute_atkinson(file = file_1,
#                                 file_name = "zz55i",
#                                 weight = "hwgt",
#                                 variable = "var_",
#                                 epsilon = 1,
#                                 na.rm = FALSE),
#                "There are NAs in the weighting variable 'hwgt' in 'zz55i'. Use na.rm = TRUE to ignore them.")
#
#   expect_warning(compute_atkinson(file = file_1,
#                                 file_name = "zz55i",
#                                 weight = "hwgt",
#                                 variable = "var_",
#                                 epsilon = 1),
#                "There are NAs in the weighting variable 'hwgt' in 'zz55i'. Use na.rm = TRUE to ignore them.")
# })
#
#
# # ** wrong or missing parameters ------------------------------------------
#
# test_that("throws an error if there are negative values in variable",{
#
#   file_1 <- tibble::tibble(var_ = c(seq(-5, 500)))
#
#   # epsilon < 1
#   expect_error(compute_atkinson(file = file_1,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 0.9),
#                "There were negative values in 'var_' 'aa55i'. Negative values are not allowed in 'compute_atkinson()'.",
#                fixed = TRUE)
#
#   # epsilon == 1
#   expect_error(compute_atkinson(file = file_1,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 1),
#                "There were negative values in 'var_' 'aa55i'. Negative values are not allowed in 'compute_atkinson()'.",
#                fixed = TRUE)
#
#   # epsilon > 1
#   expect_error(compute_atkinson(file = file_1,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 1.1),
#                "There were negative values in 'var_' 'aa55i'. Negative values are not allowed in 'compute_atkinson()'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("throws an error if epsilon is 0 or negative",{
#
#   file_ <- tibble::tibble(var_ = c(seq(-5, 500)))
#
#   expect_error(compute_atkinson(file = file_,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = 0),
#                "'epsilon' needs to be larger than 0.",
#                fixed = TRUE)
#
#   expect_error(compute_atkinson(file = file_,
#                                 file_name = "aa55i",
#                                 variable = "var_",
#                                 epsilon = -0.1),
#                "'epsilon' needs to be larger than 0.",
#                fixed = TRUE)
#
# })
#
# # TO DO: KEEP COPYING TESTS FROM compute_percentiles and compute_ratio!
# # throws a warning if there are negative values in variable
#
