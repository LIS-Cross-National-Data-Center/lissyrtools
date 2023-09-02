# # test-transform_filter.R
#
# library(testthat)
# library(lissyrtools)
#
#
#
#
# # implement_filter_age ----------------------------------------------------
#
# test_that("implement_filter_age filters by age as expected", {
#
#   zz55ip <- tibble::tibble(pid = 1:100,
#                            age = 1:100)
#
#   expect_equal(
#     min(implement_filter_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              from = 16,
#                              to = 64)[["age"]]),
#     16
#   )
#
#   expect_equal(
#     max(implement_filter_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              from = 16,
#                              to = 64)[["age"]]),
#     64
#   )
#
#   expect_equal(
#     min(implement_filter_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              from = 25,
#                              to = 55)[["age"]]),
#     25
#   )
#
# })
#
#
# test_that("implement_filter_age filters out cases where age is NA", {
#
#   zz55ip <- tibble::tibble(pid = 1:100,
#                            age = 1:100)
#
#   zz55ip[16, "age"] <- NA
#
#   expect_equal(
#     min(implement_filter_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              from = 16,
#                              to = 64)[["pid"]]),
#     17
#   )
#
# })
#
#
# test_that("implement_filter_age throws an informative error if 'age' is not in file", {
#
#   zz55ip <- tibble::tibble(pid = 1:100)
#
#   expect_error(implement_filter_age(file = zz55ip,
#                                     file_name = "zz55ip",
#                                     from = 16,
#                                     to = 64),
#                "'age' is not a variable in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
#
# # transform_filter_age ----------------------------------------------------
#
# test_that("transform_filter_age filters by age as expected", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100),
#                  zz55ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#
#   expect_equal(
#     min(transform_filter_age(lissy_files = list_1,
#                              from = 16,
#                              to = 64)[["zz44ip"]][["age"]]),
#     16
#   )
#
#   expect_equal(
#     min(transform_filter_age(lissy_files = list_1,
#                              from = 25,
#                              to = 55)[["zz44ip"]][["age"]]),
#     25
#   )
#
#   expect_equal(
#     max(transform_filter_age(lissy_files = list_1,
#                              from = 25,
#                              to = 55)[["zz44ip"]][["age"]]),
#     55
#   )
#
# })
#
#
# test_that("transform_filter_age throws an informative error if 'age' is not in file", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100),
#                  zz55ip = tibble::tibble(pid = 1:100))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_error(transform_filter_age(lissy_files = list_1,
#                                     from = 25,
#                                     to = 55),
#                "'age' is not a variable in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
#
# test_that("transform_filter_age does not drop the lissy attributes", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100),
#                  zz55ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_filter_age(lissy_files = list_1,
#                                                          from = 25,
#                                                          to = 55)),
#                list(level = "p", merged_levels = FALSE, database = "i"))
#
# })
#
#
# # implement_restrict_age --------------------------------------------------
#
# test_that("implement_restrict_age filters by age as expected", {
#
#   zz55ip <- tibble::tibble(pid = 1:100,
#                            age = 1:100,
#                            pi11 = 1:100)
#
#   expect_equal(
#     min(implement_restrict_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              variable = "pi11",
#                              from = 16,
#                              to = 64)[["pi11"]],
#         na.rm = TRUE),
#     16
#   )
#
#   expect_equal(
#     max(implement_restrict_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              variable = "pi11",
#                              from = 16,
#                              to = 64)[["pi11"]],
#         na.rm = TRUE),
#     64
#   )
#
#   expect_equal(
#     min(implement_restrict_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              variable = "pi11",
#                              from = 25,
#                              to = 55)[["pi11"]],
#         na.rm = TRUE),
#     25
#   )
#
# })
#
#
# test_that("implement_restrict_age filters out cases where age is NA", {
#
#   zz55ip <- tibble::tibble(pid = 1:100,
#                            age = 1:100,
#                            pi11 = 1:100)
#
#   zz55ip[16, "age"] <- NA
#
#   expect_equal(
#     min(implement_restrict_age(file = zz55ip,
#                              file_name = "zz55ip",
#                              variable = "pi11",
#                              from = 16,
#                              to = 64)[["pi11"]],
#         na.rm = TRUE),
#     17
#   )
#
# })
#
#
# test_that("implement_restrict_age throws an informative error if 'age' or variable is not in file", {
#
#   zz44ip <- tibble::tibble(pid = 1:100,
#                            pi11 = 1:100)
#
#   zz55ip <- tibble::tibble(pid = 1:100,
#                            age = 1:100)
#
#   expect_error(implement_restrict_age(file = zz44ip,
#                                     file_name = "zz55ip",
#                                     variable = "pi11",
#                                     from = 16,
#                                     to = 64),
#                "'age' is not a variable in 'zz55ip'.",
#                fixed = TRUE)
#
#   expect_error(implement_restrict_age(file = zz55ip,
#                                       file_name = "zz55ip",
#                                       variable = "pi11",
#                                       from = 16,
#                                       to = 64),
#                "'pi11' is not a variable in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
#
# # transform_restrict_age --------------------------------------------------
#
# test_that("transform_restrict_age filters by age as expected", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:70,
#                                          age = 1:70,
#                                          pi11 = 1:70),
#                  zz55ip = tibble::tibble(pid = 1:70,
#                                          age = 1:70,
#                                          pi11 = 1:70))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(transform_restrict_age(lissy_files = list_1,
#                                       variable = "pi11",
#                                       from = 16,
#                                       to = 64)[["zz44ip"]][["pi11"]],
#                c(rep(NA, 15),
#                  16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L,
#                  29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L,
#                  42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L,
#                  55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, rep(NA, 6))
#   )
#
# })
#
#
# test_that("transform_restrict_age throws an informative error if 'age' is not in file", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100,
#                                          pi11 = 1:100),
#                  zz55ip = tibble::tibble(pid = 1:100,
#                                          pi11 = 1:100))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_error(transform_restrict_age(lissy_files = list_1,
#                                       variable = "pi11",
#                                     from = 25,
#                                     to = 55),
#                "'age' is not a variable in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_restrict_age throws an informative error if 'variable' is not in file", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100,
#                                          pi11 = 1:100),
#                  zz55ip = tibble::tibble(pid = 1:100,
#                                          age = 1:100) # missing 'pi11'
#                  )
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_error(transform_restrict_age(lissy_files = list_1,
#                                       variable = "pi11",
#                                       from = 25,
#                                       to = 55),
#                "'pi11' is not a variable in 'zz55ip'.",
#                fixed = TRUE)
#
# })
#
#
# test_that("transform_restrict_age does not drop the lissy attributes", {
#
#   list_1 <- list(zz44ip = tibble::tibble(pid = 1:50,
#                                          age = 1:50,
#                                          pi11 = 1:50),
#                  zz55ip = tibble::tibble(pid = 1:50,
#                                          age = 1:50,
#                                          pi11 = 1:50))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "i"
#
#   expect_equal(get_lissy_attributes(transform_restrict_age(lissy_files = list_1,
#                                                            variable = "pi11",
#                                                          from = 25,
#                                                          to = 55)),
#                list(level = "p", merged_levels = FALSE, database = "i"))
#
# })
#
#
#
#
#
# # implement_restrict_to_household_heads -----------------------------------
#
# test_that("implement_restrict_to_household_heads filters by relation as expected", {
#
#   zz55wp <- tibble::tibble(hid = 1:10,
#                            relation = c(1000, 1000, NA, NA, NA, 1000, NA,
#                                         1000, 1000, NA),
#                            pi11 = 1:10)
#
#   zz44wp <- tibble::tibble(hid = 1:10,
#                            relation = c(1000, 1000, 2100, 2100, 2100, 1000, 2100,
#                                         1000, 1000, 2100),
#                            pi11 = 1:10)
#
#   expect_equal(implement_restrict_to_household_heads(file = zz55wp,
#                                                      file_name = "zz55wp",
#                                                      variable = "pi11")[["pi11"]],
#                c(1, 2, NA, NA, NA, 6, NA, 8, 9, NA))
#
#   expect_equal(implement_restrict_to_household_heads(file = zz44wp,
#                                                      file_name = "zz44wp",
#                                                      variable = "pi11")[["pi11"]],
#                c(1, 2, NA, NA, NA, 6, NA, 8, 9, NA))
# })
#
#
# test_that("implement_restrict_to_household_heads throws an informative error if 'relation' is not in file", {
#
#   zz55wp <- tibble::tibble(hid = 1:10,
#                            pi11 = 1:10)
#
#   expect_error(implement_restrict_to_household_heads(file = zz55wp,
#                                                      file_name = "zz55wp",
#                                                      variable = "pi11"),
#                "'relation' is not a variable in 'zz55wp'.")
#
# })
#
#
# test_that("implement_restrict_to_household_heads throws an informative error if 'variable' is not in file", {
#
#   zz55wp <- tibble::tibble(hid = 1:10,
#                            relation = c(1000, 1000, NA, NA, NA, 1000, NA,
#                                         1000, 1000, NA))
#
#   expect_error(implement_restrict_to_household_heads(file = zz55wp,
#                                                      file_name = "zz55wp",
#                                                      variable = "pi11"),
#                "'pi11' is not a variable in 'zz55wp'.")
#
# })
#
#
#
# # transform_restrict_to_household_heads -----------------------------------
#
# test_that("transform_restrict_to_household_heads filters by relation as expected", {
#
#   list_1 <- list(zz55wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, NA, NA, NA, 1000, NA,
#                                                       1000, 1000, NA),
#                                          pi11 = 1:10),
#                  zz44wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, 2100, 2100, 2100, 1000, 2100,
#                                                       1000, 1000, 2100),
#                                          pi11 = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "w"
#
#   expect_equal(transform_restrict_to_household_heads(lissy_files = list_1,
#                                                      variable = "pi11")[["zz55wp"]][["pi11"]],
#                c(1, 2, NA, NA, NA, 6, NA, 8, 9, NA))
#
#   expect_equal(transform_restrict_to_household_heads(lissy_files = list_1,
#                                                      variable = "pi11")[["zz44wp"]][["pi11"]],
#                c(1, 2, NA, NA, NA, 6, NA, 8, 9, NA))
#
# })
#
#
# test_that("transform_restrict_to_household_heads throws an informative error if 'relation' is not in file", {
#
#   list_1 <- list(zz55wp = tibble::tibble(hid = 1:10,
#                                          pi11 = 1:10), # missing relation
#                  zz44wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, 2100, 2100, 2100, 1000, 2100,
#                                                       1000, 1000, 2100),
#                                          pi11 = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "w"
#
#   expect_error(transform_restrict_to_household_heads(lissy_files = list_1,
#                                                      variable = "pi11"),
#                "'relation' is not a variable in 'zz55wp'.")
#
# })
#
#
# test_that("transform_restrict_to_household_heads throws an informative error if 'variable' is not in file", {
#
#   list_1 <- list(zz55wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, NA, NA, NA, 1000, NA,
#                                                       1000, 1000, NA)),
#                  zz44wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, 2100, 2100, 2100, 1000, 2100,
#                                                       1000, 1000, 2100),
#                                          pi11 = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "w"
#
#   expect_error(transform_restrict_to_household_heads(lissy_files = list_1,
#                                                      variable = "pi11"),
#                "'pi11' is not a variable in 'zz55wp'.")
#
# })
#
#
# test_that("transform_restrict_to_household_heads does not drop the lissy attributes", {
#
#   list_1 <- list(zz55wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, NA, NA, NA, 1000, NA,
#                                                       1000, 1000, NA),
#                                          pi11 = 1:10),
#                  zz44wp = tibble::tibble(hid = 1:10,
#                                          relation = c(1000, 1000, 2100, 2100, 2100, 1000, 2100,
#                                                       1000, 1000, 2100),
#                                          pi11 = 1:10))
#
#   attr(list_1, "merged_levels") <- FALSE
#   attr(list_1, "level") <- "p"
#   attr(list_1, "database") <- "w"
#
#   expect_equal(get_lissy_attributes(transform_restrict_to_household_heads(lissy_files = list_1,
#                                                                           variable = "pi11")),
#                list(level = "p", merged_levels = FALSE, database = "w"))
#
# })
#
#
