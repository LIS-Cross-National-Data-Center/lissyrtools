# # test-lissy_map.R
#
# library(testthat)
# library(lissyrtools)
#
# .run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")
#
#
# # lissy_map ---------------------------------------------------------------
#
# test_that("lissy_map does not drop the lissy attributes", {
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
#   expect_equal(get_lissy_attributes(lissy_map(lissy_files = list_1,
#                                                          .f = ~implement_filter_age(file = ..1,
#                                                                                    file_name = ..2,
#                                                                                    from = 25,
#                                                                                    to = 55)
#                                               )
#                                     ),
#                list(level = "p", merged_levels = FALSE, database = "i"))
#
#
#   expect_equal(get_lissy_attributes(lissy_map(lissy_files = list_1,
#                                               .f = ~implement_filter_age(file = .x,
#                                                                          file_name = .y,
#                                                                          from = 25,
#                                                                          to = 55)
#   )
#   ),
#   list(level = "p", merged_levels = FALSE, database = "i"))
#
# })
