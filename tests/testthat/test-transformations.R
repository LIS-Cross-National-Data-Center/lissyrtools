# library(testthat)
# library(lissyrtools)
# context("'transform' functions for data manipulation")
#
# .run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")
#
# set.seed(4)
#
#
#
# # implement_equivalise ----------------------------------------------------
#
# test_that("implement_equivalise works as expected", {
#
#   file_1 <- tibble::tibble(dhi = rep(100,10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = rep(1:2, 5))
#
#   file_2 <- tibble::tibble(dhi = rep(100, 10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = 1:10)
#
#   # expect no 0s
#   expect_equal(implement_equivalise(file = file_1,
#                                     file_name = "zz55p",
#                                     variable = "dhi")[["dhi"]],
#                rep(c(100, 70.71068), 5),
#                tolerance = 0.0001)
#
#
#   expect_equal(implement_equivalise(file = file_2,
#                                     file_name = "zz55p",
#                                     variable = "dhi")[["dhi"]],
#                100/(1:10)^0.5,
#                tolerance = 0.0001)
#
#
#   # non-default eq_scale parameter
#   expect_equal(implement_equivalise(file = file_2,
#                                     file_name = "zz55p",
#                                     variable = "dhi",
#                                     eq_scale = 0.7)[["dhi"]],
#                100/(1:10)^0.7,
#                tolerance = 0.0001)
#
# })
#
#
# test_that("implement_equivalise throws informative errors", {
#
#   file_1 <- tibble::tibble(mhi = rep(100,10), # 'mhi' instead of 'dhi'
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = rep(1:2, 5))
#
#   file_2 <- tibble::tibble(dhi = rep(100,10),
#                            hwgt = 1,
#                            hid = 1:10)
#
#   expect_error(implement_equivalise(file = file_1,
#                                     file_name = "zz55p",
#                                     variable = "dhi"),
#                "Variable 'dhi' could not be found in 'zz55p'.")
#
#   expect_error(implement_equivalise(file = file_2,
#                                     file_name = "zz55p",
#                                     variable = "dhi"),
#                "'nhhmem' could not be found in 'zz55p'.")
#
# })
#
#
# test_that("implement_equivalise throws warning if applied to a person-level variable", {
#
#   file_1 <- tibble::tibble(pi11 = rep(100,10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            pid = 1:10,
#                            nhhmem = rep(1:2, 5))
#
#
#   expect_warning(implement_equivalise(file = file_1,
#                                       file_name = "zz55p",
#                                       variable = "pi11"),
#                  "'pi11' is a person-level variable and might not need to be equivalised!")
#
#
#
# })
#
#
#
#
# # transform_equivalise ----------------------------------------------------
#
# test_that("transform_equivalise add tests for this function", {
#
#   list_1 <- list(file_1 = tibble::tibble(dhi = rep(100,10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = rep(1:2, 5)),
#                  file_2 = tibble::tibble(dhi = rep(100, 10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "h"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(transform_equivalise(lissy_files = list_1,
#                                     variable = "dhi")[["file_1"]][["dhi"]],
#                rep(c(100, 70.71068), 5),
#                tolerance = 0.0001)
#
#   expect_equal(transform_equivalise(lissy_files = list_1,
#                                     variable = "dhi")[["file_2"]][["dhi"]],
#                100/(1:10)^0.5,
#                tolerance = 0.0001)
#
#   #with non-default eq_scale
#   expect_equal(transform_equivalise(lissy_files = list_1,
#                                     variable = "dhi",
#                                     eq_scale = 0.7)[["file_2"]][["dhi"]],
#                100/(1:10)^0.7,
#                tolerance = 0.0001)
#
# })
#
#
# test_that("transform_equivalise does not drop the lissy attributes", {
#
#   list_1 <- list(file_1 = tibble::tibble(dhi = rep(100,10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = rep(1:2, 5)),
#                  file_2 = tibble::tibble(dhi = rep(100, 10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "h"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_equivalise(list_1,
#                                                          "dhi")),
#                list(level = "h", merged_levels = FALSE, database = "i"))
#
# })
#
#
# test_that("transform_equivalise throws a warning if person-level variable", {
#   # E.g. pi11 for LIS (lissyrtools::lis_person_variables) or "pasil" for LWS lissyrtools::lws_person_variables
#
#   list_1 <- list(file_1 = tibble::tibble(pi11 = rep(100,10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          pid = 1:10,
#                                          nhhmem = rep(1:2, 5)),
#                  file_2 = tibble::tibble(pi11 = rep(100, 10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          pid = 1:10,
#                                          nhhmem = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_warning(transform_equivalise(list_1,
#                                       "pi11"),
#                  "'pi11' is a person-level variable and might not need to be equivalised!")
#
# })
#
#
#
# test_that("transform_equivalise throws informative errors", {
#
#   list_1 <- list(file_1 = tibble::tibble(dhi = rep(100,10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = rep(1:2, 5)),
#                  file_2 = tibble::tibble(mhi = rep(100, 10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = 1:10))
#
#   list_2 <- list(file_1 = tibble::tibble(dhi = rep(100,10),
#                                          hwgt = 1,
#                                          hid = 1:10,
#                                          nhhmem = rep(1:2, 5)),
#                  file_2 = tibble::tibble(dhi = rep(100, 10),
#                                          hwgt = 1,
#                                          hid = 1:10))
#
#
#
#   expect_error(transform_equivalise(list_1,
#                                     variable = "dhi"),
#                "Variable 'dhi' could not be found in 'file_2'.")
#
#   expect_error(transform_equivalise(list_2,
#                                     variable = "dhi"),
#                "'nhhmem' could not be found in 'file_2'.")
#
#
# })
#
#
#
#
# # implement_equivalise_oecd ----------------------------------------------------
#
# test_that("implement_equivalise_oecd works as expected", {
#
#   file_1 <- tibble::tibble(dhi = rep(100,10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = rep(1:2, 5),
#                            nhhmem17 = rep(0, 10))
#
#   file_2 <- tibble::tibble(dhi = rep(100, 10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = 2,
#                            nhhmem17 = rep(c(0, 1), 5))
#
#   # expect no 0s
#   expect_equal(implement_equivalise_oecd(file = file_1,
#                                     file_name = "zz55p",
#                                     variable = "dhi")[["dhi"]],
#                rep(c(100, 100/1.7), 5),
#                tolerance = 0.0001)
#
#
#   expect_equal(implement_equivalise_oecd(file = file_2,
#                                     file_name = "zz55p",
#                                     variable = "dhi")[["dhi"]],
#                rep(c(100/1.7, 100/1.5), 5),
#                tolerance = 0.0001)
#
#
#   # non-default eq_scale parameter
#   expect_equal(implement_equivalise_oecd(file = file_2,
#                                     file_name = "zz55p",
#                                     variable = "dhi",
#                                     value_other_adults = 0.5,
#                                     value_children = 0.3
#                                     )[["dhi"]],
#                rep(c(100/1.5, 100/1.3), 5),
#                tolerance = 0.0001)
#
# })
#
#
# test_that("implement_equivalise_oecd throws informative errors", {
#
#   file_1 <- tibble::tibble(mhi = rep(100,10), # 'mhi' instead of 'dhi'
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = rep(1:2, 5),
#                            nhhmem17 = rep(0, 10))
#
#   file_2 <- tibble::tibble(dhi = rep(100,10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem17 = rep(0, 10))
#
#   file_3 <- tibble::tibble(dhi = rep(100,10), # 'mhi' instead of 'dhi'
#                            hwgt = 1,
#                            hid = 1:10,
#                            nhhmem = rep(1:2, 5))
#
#   expect_error(implement_equivalise_oecd(file = file_1,
#                                     file_name = "zz55p",
#                                     variable = "dhi"),
#                "Variable 'dhi' could not be found in 'zz55p'.")
#
#   expect_error(implement_equivalise_oecd(file = file_2,
#                                     file_name = "zz55p",
#                                     variable = "dhi"),
#                "'nhhmem' and 'nhhmem17' need to be in 'zz55p'.")
#
#   expect_error(implement_equivalise_oecd(file = file_3,
#                                     file_name = "zz55p",
#                                     variable = "dhi"),
#                "'nhhmem' and 'nhhmem17' need to be in 'zz55p'.")
#
# })
#
# test_that("implement_equivalise_oecd throws warning if applied to a person-level variable", {
#
#   file_1 <- tibble::tibble(pi11 = rep(100,10),
#                            hwgt = 1,
#                            hid = 1:10,
#                            pid = 1:10,
#                            nhhmem = rep(1:2, 5),
#                            nhhmem17 = rep(0, 10))
#
#
#   expect_warning(implement_equivalise_oecd(file = file_1,
#                                       file_name = "zz55p",
#                                       variable = "pi11"),
#                  "'pi11' is a person-level variable and might not need to be equivalised!")
#
# })
#
#
#
#
#
# # implement_zeros_to_na ---------------------------------------------------
#
# test_that("implement_zeros_to_na works as expected", {
#
#   file_1 <- tibble::tibble(pi11 = rep(0,10),
#                               hwgt = 1,
#                               relation = 1000)
#
#   file_2 <- tibble::tibble(pi11 = c(rep(100, 9), 0),
#                               hwgt = 1,
#                               relation = 1000)
#
#                 # expect no 0s
#   expect_equal(sum(implement_zeros_to_na(file = file_1,
#                         file_name = "zz55p",
#                         variable = "pi11")[["pi11"]] == 0, na.rm = TRUE),
#                0)
#
#   expect_equal(sum(implement_zeros_to_na(file = file_2,
#                                          file_name = "zz55p",
#                                          variable = "pi11")[["pi11"]] == 0, na.rm = TRUE),
#                0)
#
# })
#
#
# test_that("implement_zeros_to_na throws informative errors", {
#
#   file_1 <- tibble::tibble(pi12 = rep(0,10),
#                               hwgt = 1,
#                               relation = 1000)
#
#   expect_error(implement_zeros_to_na(file = file_1,
#                                      file_name = "zz55p",
#                                      variable = "pi11"),
#                "Variable 'pi11' could not be found in 'zz55p'.",
#                fixed = TRUE)
#
# })
#
#
#
#
# # transform_zeros_to_na ---------------------------------------------------
#
# test_that("transform_zeros_to_na works as expected", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(pi11 = c(rep(100, 9), 0),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#
#   expect_equal(
#     sum(transform_zeros_to_na(list_1, "pi11")[["dataset_1"]][["pi11"]] == 0, na.rm = TRUE),
#     0
#     )
#
#
#   expect_equal(
#     sum(transform_zeros_to_na(list_1, "pi11")[["dataset_2"]][["pi11"]] == 0, na.rm = TRUE),
#     0
#   )
#
# })
#
#
# test_that("transform_zeros_to_na does not drop the lissy attributes", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(pi11 = c(rep(100, 9), 0),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_zeros_to_na(list_1, "pi11")),
#                list(level = "p", merged_levels = FALSE, database = "i"))
#
# })
#
#
# test_that("transform_zeros_to_na does works well if there are no lissy attributes", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(pi11 = c(rep(100, 9), 0),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#
#   expect_equal(
#     sum(transform_zeros_to_na(list_1, "pi11")[["dataset_1"]][["pi11"]] == 0, na.rm = TRUE),
#     0
#   )
#
#
#   expect_equal(
#     sum(transform_zeros_to_na(list_1, "pi11")[["dataset_2"]][["pi11"]] == 0, na.rm = TRUE),
#     0
#   )
#
#
#   expect_equal(
#     get_lissy_attributes(transform_zeros_to_na(list_1, "pi11")),
#     NULL
#   )
#
# })
#
#
#
# test_that("transform_zeros_to_na throws informative errors", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(pi12 = c(rep(100, 9), 0),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#   expect_error(
#     transform_zeros_to_na(list_1, "pi11"),
#     "Variable 'pi11' could not be found in 'dataset_2'.",
#     fixed = TRUE
#   )
#
#
# })
#
#
#
# # import_lisppp_data ------------------------------------------------------
#
# test_that("import_lisppp_data correctly imports data from 'lissyrtools' package", {
#
#   expect_type(import_lisppp_data(path_to_ppp_file = "lissyrtools"), "list")
#
#   expect_equal(names(import_lisppp_data(path_to_ppp_file = "lissyrtools")), c("file", "lisppp"))
#
# })
#
# test_that("import_lisppp_data throws an error if the local machine is not lissy and path_to_ppp_file is set to 'lissy' or default (has also 'lissy' as argument)", {
#
#   if(!is_lissy_machine()){
#
#     expect_error(import_lisppp_data(path_to_ppp_file = "lissy"),
#                  msg = "path_to_ppp_file = 'lissy' can only be specified when using LISSY")
#
#     expect_error(import_lisppp_data(),
#                  msg = "path_to_ppp_file = 'lissy' can only be specified when using LISSY")
#
#   }
#
# })
#
#
#
# # get_lws_file_income_reference_year --------------------------------------
#
# test_that("get_lws_file_income_reference_year returns the correct value with all possible 'dataset_name' formats", {
#
#   expect_equal(get_lws_file_income_reference_year("ca16"), 2015)
#
#   expect_equal(get_lws_file_income_reference_year("ca2016"), 2015)
#
#   expect_equal(get_lws_file_income_reference_year("ca16h"), 2015)
#
#   expect_equal(get_lws_file_income_reference_year("ca2016h"), 2015)
#
#   expect_equal(get_lws_file_income_reference_year("ca16wh"), 2015)
#
#   expect_equal(get_lws_file_income_reference_year("ca2016wh"), 2015)
#
#   expect_equal(get_lws_file_income_reference_year("no10"), 2010)
#
# })
#
#
# # get_file_lisppp ---------------------------------------------------------
#
# test_that("get_file_lisppp correctly retrieves data for LIS files", {
#
#   expect_equal(get_file_lisppp("at10",  database = "lis"),
#                deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("fi04", database = "lis"),
#                deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp  ignores argument 'variable' for LIS files", {
#
#   expect_equal(get_file_lisppp("at10",  database = "lis", variable = "dhi"),
#                deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("fi04", database = "lis", variable = "should_be_ignored"),
#                deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp correctly retrieves data for LWS files and non-income variables, not specifying 'income_variable'", {
#
#   expect_equal(get_file_lisppp(file_name = "it10",  database = "lws", variable = "pir"),
#                deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("ca16", database = "lws", variable = "pir"),
#                deflators[deflators$iso2 == "ca" & deflators$year == "2016", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("at11",  database = "lws", variable = "pir"),
#                deflators[deflators$iso2 == "at" & deflators$year == "2011", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp correctly retrieves data for LWS files and non-income variables, specifying 'income_variable'", {
#
#   expect_equal(get_file_lisppp(file_name = "it10",  database = "lws", variable = "pir", income_variable = FALSE),
#                deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("ca16", database = "lws", variable = "pir", income_variable = FALSE),
#                deflators[deflators$iso2 == "ca" & deflators$year == "2016", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("at11",  database = "lws", variable = "pir", income_variable = FALSE),
#                deflators[deflators$iso2 == "at" & deflators$year == "2011", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp correctly retrieves data for LWS files and income variables, not specifying 'income_variable'", {
#
#   expect_equal(get_file_lisppp(file_name = "it10",  database = "lws", variable = "dhi"),
#                deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp(file_name = "ca16", database = "lws", variable = "dhi"),
#                deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp(file_name = "at11",  database = "lws", variable = "dhi"),
#                deflators[deflators$iso2 == "at" & deflators$year == "2009", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp correctly retrieves data for LWS files and income variables, specifying 'income_variable'", {
#
#   expect_equal(get_file_lisppp("it10",  database = "lws", variable = "dhi", income_variable = TRUE),
#                deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("ca16", database = "lws", variable = "dhi", income_variable = TRUE),
#                deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("at11",  database = "lws", variable = "dhi", income_variable = TRUE),
#                deflators[deflators$iso2 == "at" & deflators$year == "2009", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp works correctly by passing a correctly formated 'ppp_data'", {
#
#   expect_equal(get_file_lisppp("it10",  database = "lws", variable = "dhi",
#                                income_variable = TRUE,
#                                ppp_data = import_lisppp_data(path_to_ppp_file = "lissyrtools")),
#                deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("ca16", database = "lws", variable = "dhi",
#                                income_variable = TRUE,
#                                ppp_data = import_lisppp_data(path_to_ppp_file = "lissyrtools")),
#                deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("at11",  database = "lws", variable = "dhi",
#                                income_variable = TRUE,
#                                ppp_data = import_lisppp_data(path_to_ppp_file = "lissyrtools")),
#                deflators[deflators$iso2 == "at" & deflators$year == "2009", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("get_file_lisppp throws an error when passing works correctly by passing an incorrecly formated 'ppp_data'", {
#
#   incorrect_test_ppp_data <- tibble::tibble(my_file = c("it10", "ca16", "ca15"), my_lisppp = c(0.9, 1.1, 1.5))
#
#   expect_error(get_file_lisppp("it10",  database = "lws", variable = "dhi",
#                                income_variable = TRUE, ppp_data = incorrect_test_ppp_data),
#                "The dataset with deflators passed to argument 'ppp_data' must have columns named 'file' and 'lisppp' from which the function will retreive the deflator.",
#                fixed = TRUE)
#
#   expect_error(get_file_lisppp("ca16", database = "lws", variable = "dhi",
#                                income_variable = TRUE, ppp_data = incorrect_test_ppp_data),
#                "The dataset with deflators passed to argument 'ppp_data' must have columns named 'file' and 'lisppp' from which the function will retreive the deflator.",
#                fixed = TRUE)
#
#   expect_error(get_file_lisppp("at11",  database = "lws", variable = "dhi",
#                                income_variable = TRUE, ppp_data = incorrect_test_ppp_data),
#                "The dataset with deflators passed to argument 'ppp_data' must have columns named 'file' and 'lisppp' from which the function will retreive the deflator.",
#                fixed = TRUE)
#
# })
#
#
# test_that("get_file_lisppp throws an error when it doesn't have enough information to discern if the variable is an income variable", {
#
#   expect_error(get_file_lisppp("it10",  database = "lws", variable = "new_variable"),
#                "The function could not figure out if 'new_variable' is an income variable or not")
#
#   expect_error(get_file_lisppp("ca16",  database = "lws", variable = "new_variable"),
#                "The function could not figure out if 'new_variable' is an income variable or not")
#
#   expect_error(get_file_lisppp("at11",  database = "lws", variable = "new_variable"),
#                "The function could not figure out if 'new_variable' is an income variable or not")
#
# })
#
#
# test_that("get_file_lisppp does not throw an error in LIS, when it doesn't have enough information to discern if the variable is an income variable", {
#
#   expect_equal(get_file_lisppp("at10",  database = "lis"),
#                deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(get_file_lisppp("fi04", database = "lis"),
#                deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE],
#                tolerance = .0001)
# })
#
#
#
#
#
#
# # implement_adjust_by_lisppp ----------------------------------------------
#
#
# test_that("implement_adjust_by_lisppp works as expected with LIS datasets", {
#
#   file_ <- tibble::tibble(dhi = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "at10",
#                                           database = "lis",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "fi04",
#                                           database = "lis",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("implement_adjust_by_lisppp works as expected with LWS files, income variable", {
#
#   file_ <- tibble::tibble(dhi = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "at11",
#                                           database = "lws",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2009", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
# test_that("implement_adjust_by_lisppp works as expected with LWS files, income variable when database is specified as 'w' instead of 'lws'", {
#
#   file_ <- tibble::tibble(dhi = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "at11",
#                                           database = "w",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2009", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#
# })
#
#
# test_that("implement_adjust_by_lisppp works as expected with LWS files, non-income variable", {
#
#   file_ <- tibble::tibble(pir = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "pir")[["pir"]][1],
#                1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "pir")[["pir"]][1],
#                1/deflators[deflators$iso2 == "ca" & deflators$year == "2016", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file_,
#                                           file_name = "at11",
#                                           database = "lws",
#                                           variable = "pir")[["pir"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2011", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
#
#
# test_that("implement_adjust_by_lisppp works as expected with LIS files and income variable not in original file", {
#
#   file_ <- tibble::tibble(new_var = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "at10",
#                                           database = "lis",
#                                           variable = "new_var")[["new_var"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "fi04",
#                                           database = "lis",
#                                           variable = "new_var")[["new_var"]][1],
#                1/deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
#
# test_that("implement_adjust_by_lisppp when income variable not in original LWS file works if information on 'income_variable' is passed", {
#
#   file_ <- tibble::tibble(non_income_var = rep(1, 10),
#                              income_var = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "non_income_var",
#                                           income_variable = FALSE)[["non_income_var"]][1],
#                1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "non_income_var",
#                                           income_variable = FALSE)[["non_income_var"]][1],
#                1/deflators[deflators$iso2 == "ca" & deflators$year == "2016", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file_,
#                                           file_name = "at11",
#                                           database = "lws",
#                                           variable = "non_income_var",
#                                           income_variable = FALSE)[["non_income_var"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2011", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "income_var",
#                                           income_variable = TRUE)[["income_var"]][1],
#                1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "income_var",
#                                           income_variable = TRUE)[["income_var"]][1],
#                1/deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "at11",
#                                           database = "lws",
#                                           variable = "income_var",
#                                           income_variable = TRUE)[["income_var"]][1],
#                1/deflators[deflators$iso2 == "at" & deflators$year == "2009", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
#
# test_that("implement_adjust_by_lisppp when income variable not in original LWS file does not work if information on 'income_variable' is not passed", {
#
#   file_ <- tibble::tibble(new_var = rep(1, 10))
#
#   expect_error(implement_adjust_by_lisppp(file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "new_var")
#   )
#
#   expect_error(implement_adjust_by_lisppp(file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "new_var")
#   )
#
# })
#
#
# test_that("implement_adjust_by_lisppp fails if variable is not in file", {
#
#   file_ <- tibble::tibble(variable_a = rep(1, 10))
#
#   expect_error(implement_adjust_by_lisppp(file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "variable_b")
#   )
#
# })
#
#
#
# test_that("implement_adjust_by_lisppp when income variable not in original LWS file works if known income variable", {
#
#   file_ <- tibble::tibble(dhi = rep(1, 10))
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "dhi")[["dhi"]][1],
#                1/deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE],
#                tolerance = .0001)
#
# })
#
#
#
#
# test_that("implement_adjust_by_lisppp works well when passing a file to path_to_ppp_file", {
#
#   file_ <- tibble::tibble(new_variable = rep(1, 10))
#
#   test_ppp_data <- tibble::tibble(file = c("it10", "ca16", "ca15"), lisppp = c(0.9, 1.1, 1.5))
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "it10",
#                                           database = "lws",
#                                           variable = "new_variable",
#                                           income_variable = TRUE,
#                                           ppp_data = test_ppp_data)[["new_variable"]][1],
#                1/0.9,
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "new_variable",
#                                           income_variable = FALSE,
#                                           ppp_data = test_ppp_data)[["new_variable"]][1],
#                1/1.1,
#                tolerance = .0001)
#
#   expect_equal(implement_adjust_by_lisppp(file = file_,
#                                           file_name = "ca16",
#                                           database = "lws",
#                                           variable = "new_variable",
#                                           income_variable = TRUE,
#                                           ppp_data = test_ppp_data)[["new_variable"]][1],
#                1/1.5,
#                tolerance = .0001)
#
# })
#
#
# if(.run_local_tests){
#
#   test_that("implement_adjust_by_lisppp works well when passing import_lisppp_data() to path_to_ppp_file", {
#
#     file_ <- tibble::tibble(new_variable = rep(1, 10))
#
#     expect_equal(implement_adjust_by_lisppp(file = file_,
#                                             file_name = "it10",
#                                             database = "lws",
#                                             variable = "new_variable",
#                                             income_variable = TRUE,
#                                             ppp_data = import_lisppp_data(path_to_ppp_file = "lissyrtools"))[["new_variable"]][1],
#                  1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                  tolerance = .0001)
#
#   })
#
# }
#
#
# # transform_adjust_by_lisppp ----------------------------------------------
#
# test_that("transform_adjust_by_lisppp works with LIS datasets", {
#
#   test_lissy_files <- list(
#     at10 = tibble::tibble(dhi = rep(1, 10)),
#     fi04 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   expect_equivalent(transform_adjust_by_lisppp(test_lissy_files,
#                                                variable = "dhi",
#                                                database = "lis",
#                                                income_variable = NULL,
#                                                path_to_ppp_file = "lissyrtools") %>%
#                       purrr::map_dbl(~unique(.x[["dhi"]])),
#                     c(1/deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                       1/deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE]),
#                     tolerance = .0001 )
#
# })
#
#
# test_that("transform_adjust_by_lisppp works with LWS datasets and income variables", {
#
#   test_lissy_files <- list(
#     it10 = tibble::tibble(dhi = rep(1, 10)),
#     ca16 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   expect_equivalent(transform_adjust_by_lisppp(test_lissy_files,
#                                                variable = "dhi",
#                                                database = "lws",
#                                                income_variable = TRUE,
#                                                path_to_ppp_file = "lissyrtools") %>%
#                       purrr::map_dbl(~unique(.x[["dhi"]])),
#                     c(1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                       1/deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE]),
#                     tolerance = .0001)
#
# })
#
#
# test_that("transform_adjust_by_lisppp works with LWS datasets and non-income variables", {
#
#   test_lissy_files <- list(
#     it10 = tibble::tibble(pir = rep(1, 10)),
#     ca16 = tibble::tibble(pir = rep(1, 10))
#   )
#
#   expect_equivalent(transform_adjust_by_lisppp(test_lissy_files,
#                                                variable = "pir",
#                                                database = "lws",
#                                                income_variable = FALSE,
#                                                path_to_ppp_file = "lissyrtools") %>%
#                       purrr::map_dbl(~unique(.x[["pir"]])),
#                     c(1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                       1/deflators[deflators$iso2 == "ca" & deflators$year == "2016", "lisppp", drop = TRUE]),
#                     tolerance = .0001)
#
# })
#
#
# test_that("transform_adjust_by_lisppp throws an error if the name of a file is not valid", {
#       ## this error comes from 'lissyrtools::read_file_name_format()'
#
#   test_lissy_files <- list(
#     it10 = tibble::tibble(pir = rep(1, 10)),
#     ca20016 = tibble::tibble(pir = rep(1, 10))
#   )
#
#   expect_error(transform_adjust_by_lisppp(test_lissy_files,
#                                           variable = "pir",
#                                           database = "lws"),
#                "'ca20016' is not a valid file name format.")
#
#
# })
#
#
#
# test_that("transform_adjust_by_lisppp works with LIS datasets when 'database' argument is missing but can be read from attributes", {
#
#   test_lissy_files_i <- list(
#     at10 = tibble::tibble(dhi = rep(1, 10)),
#     fi04 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   attr(test_lissy_files_i, "database") <- "i"
#
#
#   expect_equivalent(transform_adjust_by_lisppp(test_lissy_files_i,
#                                                variable = "dhi",
#                                                income_variable = NULL,
#                                                path_to_ppp_file = "lissyrtools") %>%
#                       purrr::map_dbl(~unique(.x[["dhi"]])),
#                     c(1/deflators[deflators$iso2 == "at" & deflators$year == "2010", "lisppp", drop = TRUE],
#                       1/deflators[deflators$iso2 == "fi" & deflators$year == "2004", "lisppp", drop = TRUE]),
#                     tolerance = .0001 )
#
# })
#
#
# test_that("transform_adjust_by_lisppp works with LWS datasets when 'database' argument is missing", {
#
#   test_lissy_files_w <- list(
#     it10 = tibble::tibble(dhi = rep(1, 10)),
#     ca16 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   attr(test_lissy_files_w, "database") <- "w"
#
#   expect_equivalent(transform_adjust_by_lisppp(test_lissy_files_w,
#                                                variable = "dhi",
#                                                income_variable = TRUE,
#                                                path_to_ppp_file = "lissyrtools") %>%
#                       purrr::map_dbl(~unique(.x[["dhi"]])),
#                     c(1/deflators[deflators$iso2 == "it" & deflators$year == "2010", "lisppp", drop = TRUE],
#                       1/deflators[deflators$iso2 == "ca" & deflators$year == "2015", "lisppp", drop = TRUE]),
#                     tolerance = .0001)
#
# })
#
#
# test_that("transform_adjust_by_lisppp works with ERFLIS datasets when 'database' argument is missing", {
#
#   test_lissy_files_e <- list(
#     eg10 = tibble::tibble(dhi = rep(1, 10)),
#     jo10 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   attr(test_lissy_files_e, "database") <- "e"
#
#   expect_equivalent(transform_adjust_by_lisppp(test_lissy_files_e,
#                                                variable = "dhi",
#                                                income_variable = NULL,
#                                                path_to_ppp_file = "lissyrtools") %>%
#                       purrr::map_dbl(~unique(.x[["dhi"]])),
#                     c(1/deflators[deflators$iso2 == "eg" & deflators$year == "2010", "lisppp", drop = TRUE],
#                       1/deflators[deflators$iso2 == "jo" & deflators$year == "2010", "lisppp", drop = TRUE]),
#                     tolerance = .0001)
#
# })
#
#
#
#
#
# test_that("transform_adjust_by_lisppp throws an error if 'database' argument is missing and can not be guessed from file", {
#
#   test_lissy_files <- list(
#     eg10 = tibble::tibble(dhi = rep(1, 10)),
#     jo10 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   expect_error(transform_adjust_by_lisppp(test_lissy_files,
#                                           variable = "dhi",
#                                           income_variable = NULL,
#                                           path_to_ppp_file = "lissyrtools"),
#                "Attribute 'database' is NULL.")
#
# })
#
#
# test_that("transform_adjust_by_lisppp throws an error if 'database' argument is incorrect", {
#
#   test_lissy_files_1 <- list(
#     eg10 = tibble::tibble(dhi = rep(1, 10)),
#     jo10 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   test_lissy_files_2 <- list(
#     eg10 = tibble::tibble(dhi = rep(1, 10)),
#     jo10 = tibble::tibble(dhi = rep(1, 10))
#   )
#
#   attr(test_lissy_files_1, "database") <- "wrong_database"
#
#   expect_error(transform_adjust_by_lisppp(test_lissy_files_1,
#                                           variable = "dhi",
#                                           income_variable = NULL,
#                                           path_to_ppp_file = "lissyrtools"),
#                "Only 'lis', 'lws', 'erflis', 'i', 'w' and 'e' are valid values for databases. Got 'wrong_database'.")
#
#   expect_error(transform_adjust_by_lisppp(test_lissy_files_2,
#                                           variable = "dhi",
#                                           database = "wrong_database",
#                                           income_variable = NULL,
#                                           path_to_ppp_file = "lissyrtools"),
#                "Only 'lis', 'lws', 'erflis', 'i', 'w' and 'e' are valid values for databases. Got 'wrong_database'.")
#
# })
#
#
#
# test_that("transform_adjust_by_lisppp does not drop lissy attributes", {
#
#   list_1 <- list(it10 = tibble::tibble(dhi = rep(0,10),
#                                             hwgt = 1),
#                  ca16 = tibble::tibble(dhi = c(rep(100, 9), 0),
#                                             hwgt = 1))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_adjust_by_lisppp(list_1, "dhi")),
#                list(level = "p", merged_levels = FALSE, database = "i"))
#
# })
#
#
#
# # transform_negative_values_to_zero ---------------------------------------
#
# test_that("transform_negative_values_to_zero works as expected", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(pi11 = c(rep(100, 9), -2),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#   list_2 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                            hwgt = 1,
#                                            relation = 1000),
#                 dataset_2 = tibble::tibble(pi11 = rep(NA_real_, 10),
#                                            hwgt = 1,
#                                            relation = 1000))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   attr(list_2, "merged_levels") <- FALSE
#   attr(list_2, "level") <- "p"
#   attr(list_2, "database") <- "i"
#
#   expect_equal(transform_negative_values_to_zero(lissy_files = list_1,
#                                                  variable = "pi11")[["dataset_2"]],
#                tibble::tibble(pi11 = c(rep(100, 9), 0),
#                               hwgt = 1,
#                               relation = 1000))
#
#   expect_equal(transform_negative_values_to_zero(lissy_files = list_2,
#                                                  variable = "pi11")[["dataset_2"]],
#                tibble::tibble(pi11 = rep(NA_real_, 10),
#                               hwgt = 1,
#                               relation = 1000))
#
#   })
#
#
# # TO DO: TEST WITH A VARIABLE WITH ALL NAs
#
#
# test_that("transform_negative_values_to_zero throws an informative error if 'variable' can not be found in dataset", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(dhi = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(dhi = rep(100,10),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#
#   expect_error(transform_negative_values_to_zero(lissy_files = list_1,
#                                                  variable = "pi11"),
#                "Variable 'pi11' could not be found in 'dataset_1'.",
#                fixed = TRUE)
#
#
# })
#
#
#
# test_that("transform_negative_values_to_zero does not drop the lissy attributes", {
#
#   list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(0,10),
#                                             hwgt = 1,
#                                             relation = 1000),
#                  dataset_2 = tibble::tibble(pi11 = c(rep(100, 9), -2),
#                                             hwgt = 1,
#                                             relation = 1000))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_negative_values_to_zero(list_1, "pi11")),
#                list(level = "p", merged_levels = FALSE, database = "i"))
#
#
# })
#
#
#
#
# # implement_top_code_with_iqr_pfile ---------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# # "recodes outliers with very high values when level of variable is known"
#
# test_that("implement_top_code_with_iqr_pfile recodes outliers with p-level variable", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11")[1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                            file_name = "zz55ip",
#                                            variable = "pi11",
#                                            times = 2)[1, "pi11", drop = TRUE],
#                     26087.69, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile recodes outliers with h-level variable", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi",
#                                                  times = 2)[1, "dhi", drop = TRUE],
#                26087.69, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile uses 'relation' correctly for h-level variables", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 500000),
#                           hwgt = 1,
#                           relation = c(rep(1000, 12), 2000) )
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi",
#                                                  times = 3)[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile ignores 'relation' for p-level variables", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1,
#                            relation = c(1000, rep(2000, 11)))
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11",
#                                                  times = 3)[1, "pi11", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile uses weight correctly for h-level variables", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 500000),
#                           hwgt = c(rep(1, 12), 0),
#                           relation = 1000)
#
#
#   zz44i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = runif(12),
#                            relation = 1000)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi",
#                                                  times = 3)[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal( isTRUE(all.equal(implement_top_code_with_iqr_pfile(file = zz44i,
#                                                  file_name = "zz44i",
#                                                  variable = "dhi",
#                                                  times = 3)[1, "dhi", drop = TRUE],  30469.59, tolerance = 0.01, check.attributes = FALSE)),
#                FALSE, check.attributes = FALSE)
#
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile uses weight correctly for p-level variables", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500), 500000),
#                            pwgt = c(rep(1, 12), 0) )
#
#   zz44ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = runif(12))
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11",
#                                                  times = 3)[1, "pi11", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal( isTRUE(all.equal(implement_top_code_with_iqr_pfile(file = zz44ip,
#                                                                    file_name = "zz44ip",
#                                                                    variable = "pi11",
#                                                                    times = 3)[1, "pi11", drop = TRUE],
#                                  30469.59, tolerance = 0.01, check.attributes = FALSE)),
#                 FALSE, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile correctly identifies 'relation' variable if imported with 'readstata13'", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = structure(c(1L),
#                                                .Label = c("[1000]head",
#                                                           "[2000]spouse/partner"),
#                                                class = "factor"))
#
#   zz44i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 500000),
#                           hwgt = 1,
#                           relation = structure(c(rep(1L, 12), 2L),
#                                                .Label = c("[1000]head",
#                                                           "[2000]spouse/partner"),
#                                                class = "factor") )
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi",
#                                                  times = 2)[1, "dhi", drop = TRUE],
#                26087.69, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz44i,
#                                                  file_name = "zz44i",
#                                                  variable = "dhi",
#                                                  times = 3)[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("implement_top_code_with_iqr_pfile recodes outliers with specified p-level variable", {
#
#   zz55ip <- tibble::tibble(my_pi11_var = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "my_pi11_var",
#                                                  times = 3,
#                                                  variable_level = "person")[1, "my_pi11_var", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "my_pi11_var",
#                                                  times = 3,
#                                                  variable_level = "p")[1, "my_pi11_var", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile recodes outliers with specified h-level variable", {
#
#   zz55i <- tibble::tibble(my_dhi_variable = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "my_dhi_variable",
#                                                  times = 3,
#                                                  variable_level = "household")[1, "my_dhi_variable", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "my_dhi_variable",
#                                                  times = 3,
#                                                  variable_level = "h")[1, "my_dhi_variable", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile works with ad-hoc weighting variables", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            my_weight = 1)
#
#   zz44i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           my_weight = 1,
#                           relation = 1000)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11",
#                                                  times = 3,
#                                                  weight = "my_weight")[1, "pi11", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_pfile(file = zz44i,
#                                                  file_name = "zz44i",
#                                                  variable = "dhi",
#                                                  times = 3,
#                                                  weight = "my_weight")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# # ** missing requiered parameter values -----------------------------------
#
# test_that("implement_top_code_with_iqr_pfile can not detect variable_level and it is not specified in paramenter", {
#
#   zz55ip <- tibble::tibble(my_pi11_var = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   zz55i <- tibble::tibble(my_dhi_variable = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "my_pi11_var",
#                                                  times = 3)[1, "my_pi11_var", drop = TRUE],
#                "he variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.",
#                fixed = TRUE)
#
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "my_dhi_variable",
#                                                  times = 3)[1, "my_dhi_variable", drop = TRUE],
#                "he variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.",
#                fixed = TRUE)
#
# })
#
#
# # ** missing variables ----------------------------------------------------
#
# test_that("implement_top_code_with_iqr_pfile throws an error if 'variable' is missing", {
#
#   zz55ip <- tibble::tibble(pwgt = 1)
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11"),
#                "Variable 'pi11' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile throws an error if 'relation' is missing for a h-level variable", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1)
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi"),
#                "'relation' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile throws an error if 'weight' is not specified and 'hwgt' is missing for a h-level variable", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           relation = 1000)
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi"),
#                "'hwgt' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile throws an error if 'weight' is not specified and 'pwgt' is missing for a p-level variable", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)))
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11"),
#                "'pwgt' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
# test_that("implement_top_code_with_iqr_pfile throws an error if 'weight' is specified but a variable with that name cannot be found", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   expect_error(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11",
#                                                  weight = "my_weight_var"),
#                "'my_weight_var' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("implement_top_code_with_iqr_pfile throws a warning if there are missings in weighting variable", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                  file_name = "zz55ip",
#                                                  variable = "pi11"),
#                regex = "The variable 'pwgt' contains missing values in 'zz55ip'.",
#                fixed = TRUE)
#
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile throws a warning only once", {
#
#   zz55ip <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_pfile(file = zz55ip,
#                                                    file_name = "zz55ip",
#                                                    variable = "pi11"),
#                  regexp = NA)
#
#   options(zz55ip_warning_NAs_pwgt = NULL)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile throws a warning if there are missings in relation and variable is h-level", {
#
#   zz55i <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = c(rep(1000, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_pfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "dhi"),
#                  regex = "The variable 'relation' contains missing values in 'zz55i'.",
#                  fixed = TRUE)
#
#   options(zz55i_warning_NAs_relation = NULL)
#
# })
#
#
# test_that("implement_top_code_with_iqr_pfile does not throw a warning if there are missings in relation and variable is p-level", {
#
#   zz66i <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1,
#                            relation = c(rep(1000, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_pfile(file = zz66i,
#                                                    file_name = "zz66i",
#                                                    variable = "pi11"),
#                  regexp = NA)
#
# })
#
#
#
# # ** other ----------------------------------------------------------------
#
# test_that("implement_top_code_with_iqr_pfile does not return a named vector", {
#
#   zz55i <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            pwgt = 1,
#                            relation = c(rep(1000, 11), NA))
#
#   expect_equal(length(names(implement_top_code_with_iqr_pfile(file = zz55i,
#                                     file_name = "zz55i",
#                                     variable = "pi11")[["pi11"]])),
#                0)
#
# })
#
#
#
#
# # implement_top_code_with_iqr_hfile ---------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("implement_top_code_with_iqr_hfile recodes outliers", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1)
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi",
#                                                  times = 2)[1, "dhi", drop = TRUE],
#                26087.69, tolerance = .01, check.attributes = FALSE)
#
# })
#
# test_that("implement_top_code_with_iqr_hfile ignores 'relation'", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = 1,
#                           relation = rep(c(1000, 2000), 6))
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# test_that("implement_top_code_with_iqr_hfile uses weight correctly", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 500000),
#                           hwgt = c(rep(1, 12), 0))
#
#   zz44ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           hwgt = runif(12))
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi",
#                                                  times = 3)[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal( isTRUE(all.equal(implement_top_code_with_iqr_hfile(file = zz44ih,
#                                                                    file_name = "zz44ih",
#                                                                    variable = "dhi",
#                                                                    times = 3)[1, "dhi", drop = TRUE],
#                                  30469.59, tolerance = 0.01, check.attributes = FALSE)),
#                 FALSE, check.attributes = FALSE)
#
# })
#
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("implement_top_code_with_iqr_hfile recodes outliers with specified h-level variable", {
#
#   zz55i <- tibble::tibble(my_dhi_variable = c(200000, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "my_dhi_variable",
#                                                  times = 3)[1, "my_dhi_variable", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55i,
#                                                  file_name = "zz55i",
#                                                  variable = "my_dhi_variable",
#                                                  times = 3)[1, "my_dhi_variable", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# test_that("implement_top_code_with_iqr_hfile works with ad-hoc weighting variables", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            my_weight = 1)
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi",
#                                                  times = 3,
#                                                  weight = "my_weight")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# # ** missing variables ----------------------------------------------------
#
# test_that("implement_top_code_with_iqr_hfile throws an error if 'variable' is missing", {
#
#   zz55ih <- tibble::tibble(hwgt = 1)
#
#   expect_error(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi"),
#                "Variable 'dhi' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_hfile throws an error if 'weight' is not specified and 'hwgt' is missing for a h-level variable", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                           relation = 1000)
#
#   expect_error(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi"),
#                "'hwgt' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_hfile throws an error if 'weight' is specified but a variable with that name cannot be found", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = 1)
#
#   expect_error(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi",
#                                                  weight = "my_weight_var"),
#                "'my_weight_var' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("implement_top_code_with_iqr_hfile throws a warning if there are missings in weighting variable", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                    file_name = "zz55ih",
#                                                    variable = "dhi"),
#                  regex = "The variable 'hwgt' contains missing values in 'zz55ih'.",
#                  fixed = TRUE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_hfile throws a warning only once", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                    file_name = "zz55ih",
#                                                    variable = "dhi"),
#                  regexp = NA)
#
#   options(zz55ih_warning_NAs_hwgt = NULL)
#
# })
#
#
#
# # ** other ----------------------------------------------------------------
#
# test_that("implement_top_code_with_iqr_hfile throws a warning if a p-level variable is passed", {
#
#   zz55ih <- tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                    file_name = "zz55ih",
#                                                    variable = "pi11"),
#                  regexp = "The variable 'pi11' is at person-level and the file 'zz55ih' is at household-level. The methods used to top code might not be correct.")
#
# })
#
#
# test_that("implement_top_code_with_iqr_hfile ingnores relation variable", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 12)),
#                            relation = c(2000, rep(1000, 10), NA))
#
#   expect_equal(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi")[1, "dhi", drop = TRUE],
#                30469.59, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_top_code_with_iqr_hfile does not return a named vector", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 12)),
#                            relation = c(2000, rep(1000, 10), NA))
#
#   expect_equal(length(names(implement_top_code_with_iqr_hfile(file = zz55ih,
#                                                               file_name = "zz55ih",
#                                                               variable = "dhi")[["dhi"]])),
#                0)
#
# })
#
#
# # implement_top_code_with_iqr ---------------------------------------------
#
# test_that("implement_top_code_with_iqr throws an error if variable is at person-level and file", {
# # note: this should catch cases where the variable is a user-specified one.
# #   Cases with a standard lis or lws variable should already be cached with
# #   a warning within implement_top_code_with_iqr_hfile.
#
#   zz55ih <- tibble::tibble(my_pi11_var = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 12)))
#
#   expect_error(implement_top_code_with_iqr(file = zz55ih,
#                                            file_name = "zz55ih",
#                                            variable = "my_pi11_var",
#                                            file_level = "household",
#                                            variable_level = "person"),
#                "Household-level files such as 'zz55ih' should only have household-level variables. Variable 'my_pi11_var' was specified as person-level.")
#
#
# })
#
#
# # transform_top_code_with_iqr ---------------------------------------------
#
#
# # ** default arguments ----------------------------------------------------
#
# test_that("transform_top_code_with_iqr recodes outliers with very high values when level of variable is attribute and files are at person-level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                            hwgt = 1,
#                                            relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                            pwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 2)[["zz55i"]][1, "dhi", drop = TRUE],
#                     26087.69, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "pi11", times = 3)[["zz55ip"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "pi11", times = 2)[["zz55ip"]][1, "pi11", drop = TRUE],
#                     26087.69, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr recodes outliers with very high values when level of variable is attribute and files are at household-level", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 3)[["zz55ih"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 2)[["zz55ih"]][1, "dhi", drop = TRUE],
#                     26087.69, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr ignores non-household heads when computing the IQR for household-level variables in person-level files", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 200000),
#                                           hwgt = 1,
#                                           relation =  c(rep(1000, 12), 2000) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 2)[["zz55i"]][1, "dhi", drop = TRUE],
#                     26087.69, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr does not exclude non-household heads when computing the IQR for person-level variables in person-level files", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1,
#                                           relation = c(rep(1000, 10), 2000, 2000) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "pi11", times = 3)[["zz55i"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#  })
#
#
#
# test_that("transform_top_code_with_iqr ignores NAs in var", {
#
#   list_p1 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500), NA),
#                                           pwgt = 1 ))
#
#   list_p2 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), NA),
#                                          hwgt = 1,
#                                          relation = c(rep(1000, 12), 1000) ))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), NA),
#                                           hwgt = 1 ))
#
#   attr(list_p1, "merged_levels") <- FALSE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "pi11", times = 3)[["zz55ip"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 3)[["zz55ih"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr throws an error if NULL/default files_level and wrong 'level' lissy_attribute", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "wrong_level"
#   attr(list_p1, "database") <- "i"
#
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "wrong_level"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1,
#                                                 variable = "dhi",
#                                                 times = 3),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")
#
#   expect_error(transform_top_code_with_iqr(list_h1,
#                                            variable = "dhi",
#                                            times = 3),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")
#
# })
#
#
#
# test_that("transform_top_code_with_iqr throws an error if variable contains negative values", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(-5, seq(15000, 20000, 500)),
#                                          hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "household"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_h1,
#                                            variable = "dhi",
#                                            times = 3),
#                "Error in 'zz55ih'. The variable where top coding with log IQR is applied can not have negative values.",
#                fixed = TRUE)
#
# })
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("transform_top_code_with_iqr recodes outliers with very high values when level of variable is user-specified and files are at person-level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55i = tibble::tibble(my_dhi_var = c(200000, seq(15000, 20000, 500)), # non-standard variable
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p3 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1,
#                                           relation = 1000))
#
#   list_p4 <- list(zz55ip = tibble::tibble(my_pi11_var = c(200000, seq(15000, 20000, 500)), # non-standard variable
#                                           pwgt = 1,
#                                           relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_p3, "merged_levels") <- FALSE
#   attr(list_p3, "level") <- "p"
#   attr(list_p3, "database") <- "i"
#
#   attr(list_p4, "merged_levels") <- FALSE
#   attr(list_p4, "level") <- "p"
#   attr(list_p4, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3, variable_level = "household")[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "my_dhi_var", times = 3, variable_level = "household")[["zz55i"]][1, "my_dhi_var", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p3, variable = "pi11", times = 3, variable_level = "person")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p4, variable = "my_pi11_var", times = 3, variable_level = "person")[["zz55ip"]][1, "my_pi11_var", drop = TRUE],
#                     30469.59, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr recodes outliers with very high values when level of variable is user-specified and files are at household-level", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1))
#
#   list_h2 <- list(zz55ih = tibble::tibble(my_dhi_var = c(200000, seq(15000, 20000, 500)), # non-standard variable
#                                          hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   attr(list_h2, "merged_levels") <- FALSE
#   attr(list_h2, "level") <- "h"
#   attr(list_h2, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 3, variable_level = "household")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h2, variable = "my_dhi_var", times = 3, variable_level = "household")[["zz55ih"]][1, "my_dhi_var", drop = TRUE],
#                     30469.59, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr recodes outliers when level of file is user-specified", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                          pwgt = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3, files_level = "person")[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "pi11", times = 3, files_level = "person")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 3, files_level = "household")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr recodes outliers when level of file is user-specified and overwites the 'lissy attributes'", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "h" # needs to be overwritten in argument
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "h" # needs to be overwritten in argument
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "p" # needs to be overwritten in argument
#   attr(list_h1, "database") <- "i"
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3, files_level = "person")[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "pi11", times = 3, files_level = "person")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 3, files_level = "household")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
# })
#
#
#
# test_that("transform_top_code_with_iqr recodes outliers when both the level of file and variable are user-specified", {
#
#   # plevel file and hlevel variable
#   list_p1 <- list(zz55i = tibble::tibble(my_dhi_var = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   # plevel file and plevel variable
#   list_p2 <- list(zz55ip = tibble::tibble(my_pi11_var = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   # household-level file: hh variable
#   list_h1 <- list(zz55ih = tibble::tibble(my_dhi_var = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   # hh-level file: person-level variable: throws warning
#   list_h2 <- list(zz55ih = tibble::tibble(my_pi11_var = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1, variable = "my_dhi_var",
#                                                 times = 3, files_level = "person",
#                                                 variable_level = "household")[["zz55i"]][1, "my_dhi_var", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2, variable = "my_pi11_var",
#                                                 times = 3, files_level = "person",
#                                                 variable_level = "person")[["zz55ip"]][1, "my_pi11_var", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "my_dhi_var",
#                                                 times = 3, files_level = "household",
#                                                 variable_level = "household")[["zz55ih"]][1, "my_dhi_var", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_error(transform_top_code_with_iqr(list_h1, variable = "my_pi11_var",
#                                                 times = 3, files_level = "household",
#                                                 variable_level = "person")[["zz55ih"]],
#                "Household-level files such as 'zz55ih' should only have household-level variables. Variable 'my_pi11_var' was specified as person-level.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_top_code_with_iqr throws an error if user specifies wrong file level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "dhi",
#                                                 times = 3, files_level = "wrong_level",
#                                                 variable_level = "household"),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")
#
# })
#
#
# test_that("transform_top_code_with_iqr throws an error if user specifies wrong variable level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "h"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "dhi",
#                                            times = 3, files_level = "person",
#                                            variable_level = "wrong_level"),
#                "Argument 'variable_level' can only take 'person', 'p', 'household' or 'h' as values.")
#
#   expect_error(transform_top_code_with_iqr(list_h1, variable = "dhi",
#                                            times = 3, files_level = "household",
#                                            variable_level = "wrong_level"),
#                "Household-level files such as 'zz55ih' should only have household-level variables. Variable 'dhi' was specified as person-level.")
#
# })
#
#
# test_that("transform_top_code_with_iqr removes outliers with user-specified weight", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          my_weight = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           my_weight = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           my_weight = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#
#   expect_equivalent(transform_top_code_with_iqr(list_p1,
#                                                 variable = "dhi",
#                                                 weight = "my_weight")[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_p2,
#                                                 variable = "pi11",
#                                                 weight = "my_weight")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     30469.59, tolerance = .01)
#
#   expect_equivalent(transform_top_code_with_iqr(list_h1,
#                                                 variable = "dhi",
#                                                 weight = "my_weight")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01)
#
# })
#
#
# test_that("transform_top_code_with_iqr throws an error if user-specified weight can't be found", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#
#   expect_error(transform_top_code_with_iqr(list_p1,
#                                                 variable = "dhi",
#                                                 weight = "my_weight"),
#                     "'my_weight' could not be found in 'zz55i'.",
#                     fixed = TRUE)
#
#   expect_error(transform_top_code_with_iqr(list_p2,
#                                                 variable = "pi11",
#                                                 weight = "my_weight"),
#                     "'my_weight' could not be found in 'zz55ip'.",
#                     fixed = TRUE)
#
#   expect_error(transform_top_code_with_iqr(list_h1,
#                                                 variable = "dhi",
#                                                 weight = "my_weight"),
#                     "'my_weight' could not be found in 'zz55ih'.",
#                     fixed = TRUE)
#
# })
#
#
# # ** missing parameters ---------------------------------------------------
#
# test_that("transform_top_code_with_iqr throws an error if 'files_level' is missing and there is no 'level' 'lissy attribute'", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "dhi",
#                                            times = 3),
#                "'lissy_files' should have a 'level' attribute or this should be specified in 'files_level' argument.")
#
#   expect_error(transform_top_code_with_iqr(list_h1, variable = "dhi",
#                                            times = 3),
#                "'lissy_files' should have a 'level' attribute or this should be specified in 'files_level' argument.")
#
# })
#
#
# test_that("transform_top_code_with_iqr throws an error if 'variable_level' is missing (default) and can not be guessed from variable list", {
#
#   list_p1 <- list(zz55i = tibble::tibble(my_dhi_var = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(my_dhi_var = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "my_dhi_var",
#                                            times = 3),
#                "The variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.")
#
#   # household-level files don't need variable_level
#   expect_equivalent(transform_top_code_with_iqr(list_h1, variable = "my_dhi_var", times = 3)[["zz55ih"]][1, "my_dhi_var", drop = TRUE],
#                     30469.59, tolerance = .01)
# })
#
#
# # ** missing_variables ----------------------------------------------------
#
# test_that("transform_top_code_with_iqr throws error if 'variable' is missing", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "my_dhi_var", times = 3),
#                "Variable 'my_dhi_var' could not be found in 'zz55i'.")
#
#   expect_error(transform_top_code_with_iqr(list_h1, variable = "my_dhi_var", times = 3),
#                "Variable 'my_dhi_var' could not be found in 'zz55ih'.")
#
# })
#
#
# test_that("transform_top_code_with_iqr throws error if pwgt is missing in pfile and person level variable", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                          my_weight = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#    expect_error(transform_top_code_with_iqr(list_p1, variable = "pi11", times = 3),
#                 "'pwgt' could not be found in 'zz55i'.",
#                 fixed = TRUE)
#
#    expect_error(transform_top_code_with_iqr(list_p2, variable = "pi11", times = 3),
#                 "'pwgt' could not be found in 'zz55ip'.",
#                 fixed = TRUE)
#
# })
#
#
# test_that("transform_top_code_with_iqr throws error if hwgt is missing in and household-level variable", {
#
#   # try for both plevel file and hlevel file
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500))))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                "'hwgt' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
#   expect_error(transform_top_code_with_iqr(list_h1, variable = "dhi", times = 3),
#                "'hwgt' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_top_code_with_iqr removes outliers if relation is missing in person-level file and household-level variable", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          pwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_error(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                "'relation' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
#
# # ** throw warning missing values in key vars -----------------------------
#
# test_that("transform_top_code_with_iqr throws warning if variable is at person level and pwgt has NAs", {
#
#   list_p1 <- list(zz55ip_na_pwgt = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = c(NA, rep(1, 11))))
#
#   attr(list_p1, "merged_levels") <- FALSE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_warning(transform_top_code_with_iqr(list_p1, variable = "pi11", times = 3),
#                  regexp = "The variable 'pwgt' contains missing values in 'zz55ip_na_pwgt'.",
#                  fixed = TRUE)
#
#   options(zz55ip_na_pwgt_warning_NAs_pwgt = NULL)
#
#   })
#
#
# test_that("transform_top_code_with_iqr does not throw warning if variable is at person level and 'relation' or 'hwgt' have NAs", {
#
#   list_p1 <- list(zz55ip = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1,
#                                           hwgt = c(NA, rep(1, 11)),
#                                           relation = c(NA, rep(1000, 11))))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_warning(transform_top_code_with_iqr(list_p1, variable = "pi11"),
#                  regexp = NA)
#
# })
#
#
# test_that("transform_top_code_with_iqr does throws a warning if pfile, variable is at hh level and relation or hwgt have NAs", {
#
#   list_p1 <- list(zz55i_na_relation = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           pwgt = 1,
#                                           hwgt = 1,
#                                           relation = c(NA, rep(1000, 11))))
#
#   list_p2 <- list(zz55i_na_hwgt = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                                       pwgt = 1,
#                                                       hwgt = c(NA, rep(1, 11)),
#                                                       relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_warning(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                  regexp = "The variable 'relation' contains missing values in 'zz55i_na_relation'.")
#
#   expect_warning(transform_top_code_with_iqr(list_p2, variable = "dhi", times = 3),
#                  regexp = "The variable 'hwgt' contains missing values in 'zz55i_na_hwgt'.")
#
#   options(zz55i_na_relation_warning_NAs_pwgt = NULL)
#
#   options(zz55i_na_hwgt_warning_NAs_pwgt = NULL)
#
# })
#
#
# test_that("transform_top_code_with_iqr does not throw a warning if pfile, variable is at hh level and pwgt has NAs", {
#
#   list_p1 <- list(zz55ip_na_relation = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                                       pwgt = c(NA, rep(1, 11)),
#                                                       hwgt = 1,
#                                                       relation =1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_warning(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                  regexp = NA)
#
# })
#
#
#
# test_that("transform_top_code_with_iqr throws warning if hfile hwgt has NAs", {
#
#   list_h1 <- list(zz55ih_na_hwgt = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = c(NA, rep(1, 11))))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#
#   expect_warning(transform_top_code_with_iqr(list_h1, variable = "dhi"),
#                  regexp = "The variable 'hwgt' contains missing values in 'zz55ih_na_hwgt'.")
#
#   options(zz55ih_na_hwgt_warning_NAs_hwgt = NULL)
#
# })
#
#
#
# test_that("transform_top_code_with_iqr throws warning only once", {
#
#   list_h1 <- list(zz55ih_na_once = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                                   hwgt = c(NA, rep(1, 11))))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_warning(transform_top_code_with_iqr(list_h1, variable = "dhi"),
#                  regexp = "The variable 'hwgt' contains missing values in 'zz55ih_na_once'.")
#
#   expect_warning(transform_top_code_with_iqr(list_h1, variable = "dhi"),
#                  regexp = NA)
#
#   options(zz55ih_na_once_warning_NAs_hwgt = NULL)
#
#   })
#
#
#
# # ** ignoring NAs ---------------------------------------------------------
#
# test_that("transform_top_code_with_iqr ignores rows where 'relation' or 'hwgt' are missing when variable is at 'household' level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 200000),
#                                           hwgt = 1,
#                                           relation = c(rep(1000, 12), NA) ))
#
#   list_p2 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500), 200000),
#                                            hwgt = c(rep(1, 12), NA ),
#                                            relation = c(rep(1000, 12), 1000) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_equal(transform_top_code_with_iqr(list_p1, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(transform_top_code_with_iqr(list_p2, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     30469.59, tolerance = .01, check.attributes = FALSE)
#
#   })
#
#
#
# test_that("transform_top_code_with_iqr returns the same variable if it only has NAs", {
#   # i.e. Returns variable with only NAs
#
#   list_p1 <- list(zz55ip = tibble::tibble(pi11 = rep(NA, 12),
#                                           pwgt = 1))
#
#   attr(list_p1, "merged_levels") <- FALSE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(transform_top_code_with_iqr(list_p1, variable = "pi11", times = 3)[["zz55ip"]][["pi11"]],
#                rep(NA, 12), check.attributes = FALSE)
#
# })
#
#
#
# # ** other ----------------------------------------------------------------
#
# test_that("transform_top_code_with_iqr throws a warning if hfile and p-level variable", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(pi11 = c(200000, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_warning(transform_top_code_with_iqr(list_h1, variable = "pi11"),
#                  regex = "The variable 'pi11' is at person-level and the file 'zz55ih' is at household-level. The methods used to top code might not be correct.",
#                  fixed = TRUE)
#
# })
#
#
# test_that("transform_top_code_with_iqr does not drop lissy attributes", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_top_code_with_iqr(list_p1, variable ="dhi")),
#                list(level = "p", merged_levels = TRUE, database = "i"))
#
# })
#
#
#
#
#
# # implement_bottom_code_with_iqr_pfile ---------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_pfile recodes outliers with p-level variable", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11")[1, "pi11", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11",
#                                                     times = 2)[1, "pi11", drop = TRUE],
#                11531.46, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile recodes outliers with h-level variable", {
#
#   zz55i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi",
#                                                     times = 2)[1, "dhi", drop = TRUE],
#                11531.46, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile uses 'relation' correctly for h-level variables", {
#
#   zz55i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                           hwgt = 1,
#                           relation = c(rep(1000, 12), 2000) )
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi",
#                                                     times = 3)[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile ignores 'relation' for p-level variables", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = 1,
#                            relation = c(1000, rep(2000, 11)))
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11",
#                                                     times = 3)[1, "pi11", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile uses weight correctly for h-level variables", {
#
#   zz55i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                           hwgt = c(rep(1, 12), 0),
#                           relation = 1000)
#
#   zz44i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           hwgt = runif(12),
#                           relation = 1000)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi",
#                                                     times = 3)[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal( isTRUE(all.equal(implement_bottom_code_with_iqr_pfile(file = zz44i,
#                                                                       file_name = "zz44i",
#                                                                       variable = "dhi",
#                                                                       times = 3)[1, "dhi", drop = TRUE],  9828.564, tolerance = 0.01, check.attributes = FALSE)),
#                 FALSE, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile uses weight correctly for p-level variables", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500), 10),
#                            pwgt = c(rep(1, 12), 0) )
#
#   zz44ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = runif(12))
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11",
#                                                     times = 3)[1, "pi11", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal( isTRUE(all.equal(implement_bottom_code_with_iqr_pfile(file = zz44ip,
#                                                                       file_name = "zz44ip",
#                                                                       variable = "pi11",
#                                                                       times = 3)[1, "pi11", drop = TRUE],
#                                  9828.564, tolerance = 0.01, check.attributes = FALSE)),
#                 FALSE, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile recodes 0s if bottom limit is above 0", {
#
#   zz55ip <- tibble::tibble(pi11 = c(0, seq(15000, 20000, 500)),
#                            pwgt = 1 )
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                        file_name = "zz55ip",
#                                        variable = "pi11",
#                                        times = 3)[["pi11"]][[1]] > 0,
#                TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile does not recode 0s if bottom limit is below 0", {
#
#   zz55ip <- tibble::tibble(pi11 = c(0, rep(0.1, 5), rep(30, 5)),
#                            pwgt = 1 )
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                        file_name = "zz55ip",
#                                        variable = "pi11",
#                                        times = 3)[["pi11"]][[1]],
#                0)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile correctly identifies 'relation' variable if imported with 'readstata13'", {
#
#   zz55i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = structure(c(1L),
#                                                .Label = c("[1000]head",
#                                                           "[2000]spouse/partner"),
#                                                class = "factor"))
#
#   zz44i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                           hwgt = 1,
#                           relation = structure(c(rep(1L, 12), 2L),
#                                                .Label = c("[1000]head",
#                                                           "[2000]spouse/partner"),
#                                                class = "factor") )
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi",
#                                                     times = 2)[1, "dhi", drop = TRUE],
#                11531.46, tolerance = .01, check.attributes = FALSE)
#
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz44i,
#                                                     file_name = "zz44i",
#                                                     variable = "dhi",
#                                                     times = 3)[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_pfile recodes outliers with specified p-level variable", {
#
#   zz55ip <- tibble::tibble(my_pi11_var = c(10, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "my_pi11_var",
#                                                     times = 3,
#                                                     variable_level = "person")[1, "my_pi11_var", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "my_pi11_var",
#                                                     times = 3,
#                                                     variable_level = "p")[1, "my_pi11_var", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile recodes outliers with specified h-level variable", {
#
#   zz55i <- tibble::tibble(my_dhi_variable = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "my_dhi_variable",
#                                                     times = 3,
#                                                     variable_level = "household")[1, "my_dhi_variable", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "my_dhi_variable",
#                                                     times = 3,
#                                                     variable_level = "h")[1, "my_dhi_variable", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile works with ad-hoc weighting variables", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            my_weight = 1)
#
#   zz44i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           my_weight = 1,
#                           relation = 1000)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11",
#                                                     times = 3,
#                                                     weight = "my_weight")[1, "pi11", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_pfile(file = zz44i,
#                                                     file_name = "zz44i",
#                                                     variable = "dhi",
#                                                     times = 3,
#                                                     weight = "my_weight")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# # ** missing requiered parameter values -----------------------------------
#
# test_that("implement_bottom_code_with_iqr_pfile can not detect variable_level and it is not specified in paramenter", {
#
#   zz55ip <- tibble::tibble(my_pi11_var = c(10, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   zz55i <- tibble::tibble(my_dhi_variable = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "my_pi11_var",
#                                                     times = 3)[1, "my_pi11_var", drop = TRUE],
#                "he variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.",
#                fixed = TRUE)
#
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "my_dhi_variable",
#                                                     times = 3)[1, "my_dhi_variable", drop = TRUE],
#                "he variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.",
#                fixed = TRUE)
#
# })
#
#
# # ** missing variables ----------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_pfile throws an error if 'variable' is missing", {
#
#   zz55ip <- tibble::tibble(pwgt = 1)
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11"),
#                "Variable 'pi11' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile throws an error if 'relation' is missing for a h-level variable", {
#
#   zz55i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1)
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi"),
#                "'relation' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile throws an error if 'weight' is not specified and 'hwgt' is missing for a h-level variable", {
#
#   zz55i <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           relation = 1000)
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "dhi"),
#                "'hwgt' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile throws an error if 'weight' is not specified and 'pwgt' is missing for a p-level variable", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)))
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11"),
#                "'pwgt' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile throws an error if 'weight' is specified but a variable with that name cannot be found", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = 1)
#
#   expect_error(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                     file_name = "zz55ip",
#                                                     variable = "pi11",
#                                                     weight = "my_weight_var"),
#                "'my_weight_var' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_pfile throws a warning if there are missings in weighting variable", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                       file_name = "zz55ip",
#                                                       variable = "pi11"),
#                  regex = "The variable 'pwgt' contains missing values in 'zz55ip'.",
#                  fixed = TRUE)
#
# })
#
# test_that("implement_bottom_code_with_iqr_pfile throws a warning only once", {
#
#   zz55ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_pfile(file = zz55ip,
#                                                       file_name = "zz55ip",
#                                                       variable = "pi11"),
#                  regexp = NA)
#
#   options(zz55ip_warning_NAs_pwgt = NULL)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile throws a warning if there are missings in relation and variable is h-level", {
#
#   zz55i_nas_relation <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = c(rep(1000, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_pfile(file = zz55i_nas_relation,
#                                                       file_name = "zz55i_nas_relation",
#                                                       variable = "dhi"),
#                  regex = "The variable 'relation' contains missing values in 'zz55i_nas_relation'.",
#                  fixed = TRUE)
#
#   options(zz55i_nas_relation_warning_NAs_relation = NULL)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_pfile does not throw a warning if there are missings in relation and variable is p-level", {
#
#   zz66ip <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            pwgt = 1,
#                            relation = c(rep(1000, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_pfile(file = zz66ip,
#                                                       file_name = "zz66ip",
#                                                       variable = "pi11"),
#                  regexp = NA)
#
# })
#
#
#
# # ** other ----------------------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_pfile does not return a named vector", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 12)),
#                            relation = c(2000, rep(1000, 10), NA))
#
#   expect_equal(length(names(implement_bottom_code_with_iqr_pfile(file = zz55ih,
#                                                               file_name = "zz55ih",
#                                                               variable = "dhi")[["dhi"]])),
#                0)
#
# })
#
#
#
#
# # implement_bottom_code_with_iqr_hfile ---------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_hfile recodes outliers", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = 1)
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                  file_name = "zz55ih",
#                                                  variable = "dhi",
#                                                  times = 2)[1, "dhi", drop = TRUE],
#                11531.46, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile ignores 'relation'", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = 1,
#                            relation = rep(c(1000, 2000), 6))
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# test_that("implement_bottom_code_with_iqr_hfile uses weight correctly", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                            hwgt = c(rep(1, 12), 0))
#
#   zz44ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = runif(12))
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi",
#                                                     times = 3)[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal( isTRUE(all.equal(implement_bottom_code_with_iqr_hfile(file = zz44ih,
#                                                                       file_name = "zz44ih",
#                                                                       variable = "dhi",
#                                                                       times = 3)[1, "dhi", drop = TRUE],
#                                  9828.564, tolerance = 0.01, check.attributes = FALSE)),
#                 FALSE, check.attributes = FALSE)
#
# })
#
#
#
#
#
# test_that("implement_bottom_code_with_iqr_hfile recodes 0s if bottom limit is above 0", {
#
#   zz55ih <- tibble::tibble(dhi = c(0, seq(15000, 20000, 500)),
#                            hwgt = 1 )
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi",
#                                                     times = 3)[["dhi"]][[1]] > 0,
#                TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile does not recode 0s if bottom limit is below 0", {
#
#   zz55ih <- tibble::tibble(dhi = c(0, rep(0.1, 5), rep(30, 5)),
#                            hwgt = 1 )
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi",
#                                                     times = 3)[["dhi"]][[1]],
#                0)
#
# })
# # ** user-specified arguments ---------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_hfile recodes outliers with specified h-level variable", {
#
#   zz55i <- tibble::tibble(my_dhi_variable = c(10, seq(15000, 20000, 500)),
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "my_dhi_variable",
#                                                     times = 3)[1, "my_dhi_variable", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55i,
#                                                     file_name = "zz55i",
#                                                     variable = "my_dhi_variable",
#                                                     times = 3)[1, "my_dhi_variable", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
#
# test_that("implement_bottom_code_with_iqr_hfile works with ad-hoc weighting variables", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            my_weight = 1)
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi",
#                                                     times = 3,
#                                                     weight = "my_weight")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# # ** missing variables ----------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_hfile throws an error if 'variable' is missing", {
#
#   zz55ih <- tibble::tibble(hwgt = 1)
#
#   expect_error(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi"),
#                "Variable 'dhi' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile throws an error if 'weight' is not specified and 'hwgt' is missing for a h-level variable", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            relation = 1000)
#
#   expect_error(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi"),
#                "'hwgt' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile throws an error if 'weight' is specified but a variable with that name cannot be found", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = 1)
#
#   expect_error(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi",
#                                                     weight = "my_weight_var"),
#                "'my_weight_var' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# # ** missing values -------------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_hfile throws a warning if there are missings in weighting variable", {
#
#   options(zz55ih_warning_NAs_hwgt = NULL)
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                       file_name = "zz55ih",
#                                                       variable = "dhi"),
#                  regex = "The variable 'hwgt' contains missing values in 'zz55ih'.",
#                  fixed = TRUE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile throws a warning only once", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                       file_name = "zz55ih",
#                                                       variable = "dhi"),
#                  regexp = NA)
#
#   options(zz55ih_warning_NAs_hwgt = NA)
#
# })
#
#
# # ** other ----------------------------------------------------------------
#
# test_that("implement_bottom_code_with_iqr_hfile throws a warning if a p-level variable is passed", {
#
#   zz55ih <- tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 11), NA))
#
#   expect_warning(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                       file_name = "zz55ih",
#                                                       variable = "pi11"),
#                  regexp = "The variable 'pi11' is at person-level and the file 'zz55ih' is at household-level. The methods used to top code might not be correct.")
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile ingnores relation variable", {
#
#   zz55ih <- tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 12)),
#                            relation = c(2000, rep(1000, 10), NA))
#
#   expect_equal(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                     file_name = "zz55ih",
#                                                     variable = "dhi")[1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("implement_bottom_code_with_iqr_hfile does not return a named vector", {
#
#   zz55ih <- tibble::tibble(dhi = c(200000, seq(15000, 20000, 500)),
#                            hwgt = c(rep(1, 12)),
#                            relation = c(2000, rep(1000, 10), NA))
#
#   expect_equal(length(names(implement_bottom_code_with_iqr_hfile(file = zz55ih,
#                                                               file_name = "zz55ih",
#                                                               variable = "dhi")[["dhi"]])),
#                0)
#
# })
#
#
#
# # transform_bottom_code_with_iqr ------------------------------------------
#
# # ** default arguments ----------------------------------------------------
#
# test_that("transform_bottom_code_with_iqr recodes outliers with very high values when level of variable is attribute and files are at person-level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 2)[["zz55i"]][1, "dhi", drop = TRUE],
#                     11531.46, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "pi11", times = 3)[["zz55ip"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "pi11", times = 2)[["zz55ip"]][1, "pi11", drop = TRUE],
#                     11531.46, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr recodes outliers with very high values when level of variable is attribute and files are at household-level", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 3)[["zz55ih"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 2)[["zz55ih"]][1, "dhi", drop = TRUE],
#                     11531.46, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr ignores non-household heads when computing the IQR for household-level variables in person-level files", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                                          hwgt = 1,
#                                          relation =  c(rep(1000, 12), 2000) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 2)[["zz55i"]][1, "dhi", drop = TRUE],
#                     11531.46, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr does not exclude non-household heads when computing the IQR for person-level variables in person-level files", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                          pwgt = 1,
#                                          relation = c(rep(1000, 10), 2000, 2000) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "pi11", times = 3)[["zz55i"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
#
# test_that("transform_bottom_code_with_iqr ignores NAs in var", {
#
#   list_p1 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500), NA),
#                                           pwgt = 1 ))
#
#   list_p2 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500), NA),
#                                          hwgt = 1,
#                                          relation = c(rep(1000, 12), 1000) ))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500), NA),
#                                           hwgt = 1 ))
#
#   attr(list_p1, "merged_levels") <- FALSE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "pi11", times = 3)[["zz55ip"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 3)[["zz55ih"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws an error if NULL/default files_level and wrong 'level' lissy_attribute", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "wrong_level"
#   attr(list_p1, "database") <- "i"
#
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "wrong_level"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1,
#                                               variable = "dhi",
#                                               times = 3),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")
#
#   expect_error(transform_bottom_code_with_iqr(list_h1,
#                                               variable = "dhi",
#                                               times = 3),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")
#
# })
#
#
#
# test_that("transform_bottom_code_with_iqr throws an error if variable contains negative values", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(-5, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "household"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_h1,
#                                            variable = "dhi",
#                                            times = 3),
#                "Error in 'zz55ih'. The variable where top coding with log IQR is applied can not have negative values.",
#                fixed = TRUE)
#
#
# })
#
#
#
# # ** user-specified arguments ---------------------------------------------
#
# test_that("transform_bottom_code_with_iqr recodes outliers with very high values when level of variable is user-specified and files are at person-level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55i = tibble::tibble(my_dhi_var = c(10, seq(15000, 20000, 500)), # non-standard variable
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p3 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1,
#                                           relation = 1000))
#
#   list_p4 <- list(zz55ip = tibble::tibble(my_pi11_var = c(10, seq(15000, 20000, 500)), # non-standard variable
#                                           pwgt = 1,
#                                           relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_p3, "merged_levels") <- FALSE
#   attr(list_p3, "level") <- "p"
#   attr(list_p3, "database") <- "i"
#
#   attr(list_p4, "merged_levels") <- FALSE
#   attr(list_p4, "level") <- "p"
#   attr(list_p4, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3, variable_level = "household")[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "my_dhi_var", times = 3, variable_level = "household")[["zz55i"]][1, "my_dhi_var", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p3, variable = "pi11", times = 3, variable_level = "person")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p4, variable = "my_pi11_var", times = 3, variable_level = "person")[["zz55ip"]][1, "my_pi11_var", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr recodes outliers with very high values when level of variable is user-specified and files are at household-level", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   list_h2 <- list(zz55ih = tibble::tibble(my_dhi_var = c(10, seq(15000, 20000, 500)), # non-standard variable
#                                           hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   attr(list_h2, "merged_levels") <- FALSE
#   attr(list_h2, "level") <- "h"
#   attr(list_h2, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 3, variable_level = "household")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h2, variable = "my_dhi_var", times = 3, variable_level = "household")[["zz55ih"]][1, "my_dhi_var", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr recodes outliers when level of file is user-specified", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3, files_level = "person")[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "pi11", times = 3, files_level = "person")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 3, files_level = "household")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr recodes outliers when level of file is user-specified and overwites the 'lissy attributes'", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "h" # needs to be overwritten in argument
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "h" # needs to be overwritten in argument
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "p" # needs to be overwritten in argument
#   attr(list_h1, "database") <- "i"
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3, files_level = "person")[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "pi11", times = 3, files_level = "person")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 3, files_level = "household")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr recodes outliers when both the level of file and variable are user-specified", {
#
#   # plevel file and hlevel variable
#   list_p1 <- list(zz55i = tibble::tibble(my_dhi_var = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   # plevel file and plevel variable
#   list_p2 <- list(zz55ip = tibble::tibble(my_pi11_var = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   # household-level file: hh variable
#   list_h1 <- list(zz55ih = tibble::tibble(my_dhi_var = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   # hh-level file: person-level variable: throws warning
#   list_h2 <- list(zz55ih = tibble::tibble(my_pi11_var = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1, variable = "my_dhi_var",
#                                                    times = 3, files_level = "person",
#                                                    variable_level = "household")[["zz55i"]][1, "my_dhi_var", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2, variable = "my_pi11_var",
#                                                    times = 3, files_level = "person",
#                                                    variable_level = "person")[["zz55ip"]][1, "my_pi11_var", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "my_dhi_var",
#                                                    times = 3, files_level = "household",
#                                                    variable_level = "household")[["zz55ih"]][1, "my_dhi_var", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_error(transform_bottom_code_with_iqr(list_h1, variable = "my_pi11_var",
#                                               times = 3, files_level = "household",
#                                               variable_level = "person")[["zz55ih"]],
#                "Household-level files such as 'zz55ih' should only have household-level variables. Variable 'my_pi11_var' was specified as person-level.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws an error if user specifies wrong file level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "dhi",
#                                               times = 3, files_level = "wrong_level",
#                                               variable_level = "household"),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws an error if user specifies wrong variable level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "h"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "dhi",
#                                               times = 3, files_level = "person",
#                                               variable_level = "wrong_level"),
#                "Argument 'variable_level' can only take 'person', 'p', 'household' or 'h' as values.")
#
#   expect_error(transform_bottom_code_with_iqr(list_h1, variable = "dhi",
#                                               times = 3, files_level = "household",
#                                               variable_level = "wrong_level"),
#                "Household-level files such as 'zz55ih' should only have household-level variables. Variable 'dhi' was specified as person-level.")
#
# })
#
#
# test_that("transform_bottom_code_with_iqr removes outliers with user-specified weight", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          my_weight = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           my_weight = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           my_weight = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p1,
#                                                    variable = "dhi",
#                                                    weight = "my_weight")[["zz55i"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_p2,
#                                                    variable = "pi11",
#                                                    weight = "my_weight")[["zz55ip"]][1, "pi11", drop = TRUE],
#                     9828.564, tolerance = .01)
#
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1,
#                                                    variable = "dhi",
#                                                    weight = "my_weight")[["zz55ih"]][1, "dhi", drop = TRUE],
#                     9828.564, tolerance = .01)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws an error if user-specified weight can't be found", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#
#   expect_error(transform_bottom_code_with_iqr(list_p1,
#                                               variable = "dhi",
#                                               weight = "my_weight"),
#                "'my_weight' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
#   expect_error(transform_bottom_code_with_iqr(list_p2,
#                                               variable = "pi11",
#                                               weight = "my_weight"),
#                "'my_weight' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
#   expect_error(transform_bottom_code_with_iqr(list_h1,
#                                               variable = "dhi",
#                                               weight = "my_weight"),
#                "'my_weight' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
# # ** missing parameters ---------------------------------------------------
#
# test_that("transform_bottom_code_with_iqr throws an error if 'files_level' is missing and there is no 'level' 'lissy attribute'", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "dhi",
#                                               times = 3),
#                "'lissy_files' should have a 'level' attribute or this should be specified in 'files_level' argument.")
#
#   expect_error(transform_bottom_code_with_iqr(list_h1, variable = "dhi",
#                                               times = 3),
#                "'lissy_files' should have a 'level' attribute or this should be specified in 'files_level' argument.")
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws an error if 'variable_level' is missing (default) and can not be guessed from variable list", {
#
#   list_p1 <- list(zz55i = tibble::tibble(my_dhi_var = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(my_dhi_var = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "my_dhi_var",
#                                               times = 3),
#                "The variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.")
#
#   # household-level files don't need variable_level
#   expect_equivalent(transform_bottom_code_with_iqr(list_h1, variable = "my_dhi_var", times = 3)[["zz55ih"]][1, "my_dhi_var", drop = TRUE],
#                     9828.564, tolerance = .01)
# })
#
#
# # ** missing_variables ----------------------------------------------------
#
# test_that("transform_bottom_code_with_iqr throws error if 'variable' is missing", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                           hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "my_dhi_var", times = 3),
#                "Variable 'my_dhi_var' could not be found in 'zz55i'.")
#
#   expect_error(transform_bottom_code_with_iqr(list_h1, variable = "my_dhi_var", times = 3),
#                "Variable 'my_dhi_var' could not be found in 'zz55ih'.")
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws error if pwgt is missing in pfile and person level variable", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           my_weight = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- FALSE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "pi11", times = 3),
#                "'pwgt' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
#   expect_error(transform_bottom_code_with_iqr(list_p2, variable = "pi11", times = 3),
#                "'pwgt' could not be found in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws error if hwgt is missing in and household-level variable", {
#
#   # try for both plevel file and hlevel file
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                          relation = 1000))
#
#   list_h1 <- list(zz55ih = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                "'hwgt' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
#   expect_error(transform_bottom_code_with_iqr(list_h1, variable = "dhi", times = 3),
#                "'hwgt' could not be found in 'zz55ih'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr removes outliers if relation is missing in person-level file and household-level variable", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)) ,
#                                          hwgt = 1,
#                                          pwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_error(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                "'relation' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# # ** throw warning missing values in key vars -----------------------------
#
# test_that("transform_bottom_code_with_iqr throws warning if variable is at person level and pwgt has NAs", {
#
#   list_p1 <- list(zz55ip_na_pwgt = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                                   pwgt = c(NA, rep(1, 11))))
#
#   attr(list_p1, "merged_levels") <- FALSE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_warning(transform_bottom_code_with_iqr(list_p1, variable = "pi11", times = 3),
#                  regexp = "The variable 'pwgt' contains missing values in 'zz55ip_na_pwgt'.",
#                  fixed = TRUE)
#
#   options(zz55ip_na_pwgt_warning_NAs_pwgt = NULL)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr does not throw warning if variable is at person level and 'relation' or 'hwgt' have NAs", {
#
#   list_p1 <- list(zz55ip = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500)),
#                                           pwgt = 1,
#                                           hwgt = c(NA, rep(1, 11)),
#                                           relation = c(NA, rep(1000, 11))))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_warning(transform_bottom_code_with_iqr(list_p1, variable = "pi11"),
#                  regexp = NA)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr does throws a warning if pfile, variable is at hh level and relation or hwgt have NAs", {
#
#   list_p1 <- list(zz55i_na_relation = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                                      pwgt = 1,
#                                                      hwgt = 1,
#                                                      relation = c(NA, rep(1000, 11))))
#
#   list_p2 <- list(zz55i_na_hwgt = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                                  pwgt = 1,
#                                                  hwgt = c(NA, rep(1, 11)),
#                                                  relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   options(zz55i_na_relation_warning_NAs_relation = NULL)
#
#   options(zz55i_na_hwgt_warning_NAs_hwgt = NULL)
#
#   expect_warning(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                  regexp = "The variable 'relation' contains missing values in 'zz55i_na_relation'.")
#
#   expect_warning(transform_bottom_code_with_iqr(list_p2, variable = "dhi", times = 3),
#                  regexp = "The variable 'hwgt' contains missing values in 'zz55i_na_hwgt'.")
#
#   options(zz55i_na_relation_warning_NAs_relation = NULL)
#
#   options(zz55i_na_hwgt_warning_NAs_hwgt = NULL)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr does not throw a warning if pfile, variable is at hh level and pwgt has NAs", {
#
#   list_p1 <- list(zz55ip_na_relation = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                                       pwgt = c(NA, rep(1, 11)),
#                                                       hwgt = 1,
#                                                       relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_warning(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3),
#                  regexp = NA)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws warning if hfile hwgt has NAs", {
#
#   list_h1 <- list(zz55ih_na_hwgt = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                                   hwgt = c(NA, rep(1, 11))))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#
#   expect_warning(transform_bottom_code_with_iqr(list_h1, variable = "dhi"),
#                  regexp = "The variable 'hwgt' contains missing values in 'zz55ih_na_hwgt'.")
#
#   options(zz55ih_na_hwgt_warning_NAs_hwgt = NULL)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr throws warning only once", {
#
#   list_h1 <- list(zz55ih_na_once = tibble::tibble(dhi = c(10, seq(15000, 20000, 500)),
#                                                   hwgt = c(NA, rep(1, 11))))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_warning(transform_bottom_code_with_iqr(list_h1, variable = "dhi"),
#                  regexp = "The variable 'hwgt' contains missing values in 'zz55ih_na_once'.")
#
#   expect_warning(transform_bottom_code_with_iqr(list_h1, variable = "dhi"),
#                  regexp = NA)
#
#   options(zz55ih_na_once_warning_NAs_hwgt = NULL)
#
# })
#
#
# # ** ignoring NAs ---------------------------------------------------------
#
# test_that("transform_bottom_code_with_iqr ignores rows where 'relation' or 'hwgt' are missing when variable is at 'household' level", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                                          hwgt = 1,
#                                          relation = c(rep(1000, 12), NA) ))
#
#   list_p2 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                                          hwgt = c(rep(1, 12), NA ),
#                                          relation = c(rep(1000, 12), 1000) ))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_equal(transform_bottom_code_with_iqr(list_p1, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
#   expect_equal(transform_bottom_code_with_iqr(list_p2, variable = "dhi", times = 3)[["zz55i"]][1, "dhi", drop = TRUE],
#                9828.564, tolerance = .01, check.attributes = FALSE)
#
# })
#
#
# test_that("transform_bottom_code_with_iqr returns the same variable if it only has NAs", {
#   # i.e. Returns variable with only NAs
#
#   list_p1 <- list(zz55ip = tibble::tibble(pi11 = rep(NA, 12),
#                                           pwgt = 1))
#
#   attr(list_p1, "merged_levels") <- FALSE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(transform_bottom_code_with_iqr(list_p1, variable = "pi11", times = 3)[["zz55ip"]][["pi11"]],
#                rep(NA, 12), check.attributes = FALSE)
#
# })
#
#
# # ** other ----------------------------------------------------------------
#
# test_that("transform_bottom_code_with_iqr throws a warning if hfile and p-level variable", {
#
#   list_h1 <- list(zz55ih = tibble::tibble(pi11 = c(10, seq(15000, 20000, 500), 10),
#                                           hwgt = 1))
#
#   attr(list_h1, "merged_levels") <- FALSE
#   attr(list_h1, "level") <- "h"
#   attr(list_h1, "database") <- "i"
#
#   expect_warning(transform_bottom_code_with_iqr(list_h1, variable = "pi11"),
#                  regex = "The variable 'pi11' is at person-level and the file 'zz55ih' is at household-level. The methods used to top code might not be correct.",
#                  fixed = TRUE)
#
# })
#
#
#
# test_that("transform_bottom_code_with_iqr does not drop lissy attributes", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = c(10, seq(15000, 20000, 500), 10),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_bottom_code_with_iqr(list_p1, variable ="dhi")),
#                list(level = "p", merged_levels = TRUE, database = "i"))
#
# })
#
#
#
# # implement_false_zeros_to_na ---------------------------------------------
#
# test_that("implement_false_zeros_to_na works as expected", {
#
#   zz55i <- tibble::tibble(dhi = rep(0, 10),
#                  hwgt = 1,
#                  relation = 1000)
#
#   zz44i <- tibble::tibble(dhi = c(rep(0, 10), 1), # <- not all 0s
#                           hwgt = 1,
#                           relation = 1000)
#
#   expect_equal(implement_false_zeros_to_na(file = zz55i,
#                               file_name = "zz55i",
#                               variable = "dhi"),
#                tibble::tibble(dhi = rep(NA, 10),
#                               hwgt = 1,
#                               relation = 1000))
#
#   expect_equal(implement_false_zeros_to_na(file = zz44i,
#                                            file_name = "zz55i",
#                                            variable = "dhi"),
#                tibble::tibble(dhi = c(rep(0, 10), 1),
#                               hwgt = 1,
#                               relation = 1000))
#
# })
#
#
#
#
# # transform_false_zeros_to_na ---------------------------------------------
#
# test_that("transform_false_zeros_to_na works as expected", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = rep(0, 10),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   list_p2 <- list(zz55i = tibble::tibble(dhi = c(rep(0, 10), 1),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   expect_equal(transform_false_zeros_to_na(lissy_files = list_p1,
#                                            variable = "dhi"),
#                list(zz55i = tibble::tibble(dhi = rep(NA, 10),
#                                            hwgt = 1,
#                                            relation = 1000))
#   )
#
#   expect_equal(transform_false_zeros_to_na(lissy_files = list_p2,
#                                            variable = "dhi"),
#                list(zz55i = tibble::tibble(dhi = c(rep(0, 10), 1),
#                                            hwgt = 1,
#                                            relation = 1000))
#   )
#
# })
#
#
# test_that("transform_false_zeros_to_na does not drop lissy attributes", {
#
#   list_p1 <- list(zz55i = tibble::tibble(dhi = rep(0, 10),
#                                          hwgt = 1,
#                                          relation = 1000))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_false_zeros_to_na(list_p1, variable ="dhi")),
#                list(level = "p", merged_levels = TRUE, database = "i"))
#
# })
#
#
#
# # transform_weight_by_hh_size ---------------------------------------------
#
# test_that("transform_weight_by_hh_size multiplies hh weights", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pid = 1:10,
#                                          hid = sort(rep(1:5, 2)),
#                                          hwgt = 1,
#                                          nhhmem = sort(rep(1:5, 2))))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(transform_weight_by_hh_size(list_p1)[["zz55i"]][["hwgt"]],
#                c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L))
#
# })
#
#
# test_that("transform_weight_by_hh_size throws error if 'nhhmem' or 'hwgt' are missing", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pid = 1:10,
#                                          hid = sort(rep(1:5, 2)),
#                                          nhhmem = sort(rep(1:5, 2))))
#
#   list_p2 <- list(zz55i = tibble::tibble(pid = 1:10,
#                                          hid = sort(rep(1:5, 2)),
#                                          hwgt = 1))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   attr(list_p2, "merged_levels") <- TRUE
#   attr(list_p2, "level") <- "p"
#   attr(list_p2, "database") <- "i"
#
#   expect_error(transform_weight_by_hh_size(list_p1),
#                "'hwgt' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
#   expect_error(transform_weight_by_hh_size(list_p2),
#                "'nhhmem' could not be found in 'zz55i'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_weight_by_hh_size does not drop lissy attributes", {
#
#   list_p1 <- list(zz55i = tibble::tibble(pid = 1:10,
#                                          hid = sort(rep(1:5, 2)),
#                                          hwgt = 1,
#                                          nhhmem = sort(rep(1:5, 2))))
#
#   attr(list_p1, "merged_levels") <- TRUE
#   attr(list_p1, "level") <- "p"
#   attr(list_p1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_weight_by_hh_size(list_p1)),
#                list(level = "p", merged_levels = TRUE, database = "i"))
#
# })
#
#
#
# # get_lws_file_income_reference_year --------------------------------------
#
# test_that("get_lws_file_income_reference_year throws an error with the name of the file when this can not be found", {
#
#   expect_error(get_lws_file_income_reference_year("ca65"),
#                "ca65")
#
#   expect_error(get_lws_file_income_reference_year("ca1965"),
#                "ca65")
#
#
# })
#
#
# test_that("get_lws_file_income_reference_year returns an output of length 1", {
#
#
#   expect_equal(length(get_lws_file_income_reference_year(file_name = "no10")),
#                1)
#
#
# })
#
#
#
#
#
#
#
#
#
#
#
#
# # check_variable_level ----------------------------------------------------
#
# test_that("check_variable_level works well with lis and lws household and person-level variables", {
#
#   expect_equivalent(check_variable_level("dhi"), "household")
#
#   expect_equivalent(check_variable_level("pitotal"), "person")
#
#   expect_equivalent(check_variable_level("ppr"), "household")
#
#   expect_equivalent(check_variable_level("pasil"), "person")
#
# })
#
#
# test_that("check_variable_level throws an error if the variable wasn't expected", {
#
#   expect_error(check_variable_level("hid"),
#                msg = "The variable does not have a level and it appears both in 'p-level' and 'h-level' files.",
#                exact = TRUE)
#
#   expect_error(check_variable_level("unknown"))
#
# })
