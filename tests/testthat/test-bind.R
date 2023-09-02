# # test-bind
#
#
# library(lissyrtools)
# context("Function for binding multiple LIS or LWS datasets.")
#
#
#
#
# # bind_lissy_files --------------------------------------------------------
#
# test_that("bind_lissy_files concatenates datasets at person level", {
#
#   aa00 <- tibble::tibble(did = 001,
#                         hid = 1:10,
#                         pid = 1:10,
#                         var_ = rep(5, 10))
#
#   aa01 <- tibble::tibble(did = 002,
#                         hid = 1:10,
#                         pid = 1:10,
#                         var_ = rep(5, 10))
#
#   attr(aa00, "level") <- "person"
#   attr(aa01, "level") <- "person"
#
#   list_ <- list(
#     `aa00` = aa00,
#     `aa01` = aa01
#   )
#
#   list_ <- set_lissy_attributes(lissy_object = list_,
#                        lissy_attributes = list(level = "p",
#                                                merged_levels = FALSE,
#                                                database = "i"))
#
#
#   expect_equivalent(bind_lissy_files(list_),
#                     tibble::tibble(file = c(rep("aa00", 10), rep("aa01", 10)),
#                                    did = c(rep(1, 10), rep(2, 10)),
#                                    hid = rep(1:10, 2),
#                                    pid = rep(1:10, 2),
#                                    var_ = rep(5, 20),
#                                    unique_hid = c("1_1", "1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_8", "1_9",
#                                                   "1_10", "2_1", "2_2", "2_3", "2_4", "2_5", "2_6", "2_7", "2_8",
#                                                   "2_9", "2_10"),
#                                    unique_pid = c("1_1", "1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_8", "1_9",
#                                                   "1_10", "2_1", "2_2", "2_3", "2_4", "2_5", "2_6", "2_7", "2_8",
#                                                   "2_9", "2_10")
#                                    ))
#
# })
#
#
#
# test_that("bind_lissy_files concatenates datasets at household level", {
#
#   aa00 <- tibble::tibble(did = 001,
#                          hid = 1:10,
#                          var_ = rep(5, 10))
#
#   aa01 <- tibble::tibble(did = 002,
#                          hid = 1:10,
#                          var_ = rep(5, 10))
#
#   list_ <- list(
#     `aa00` = aa00,
#     `aa01` = aa01
#   )
#
#   list_ <- set_lissy_attributes(lissy_object = list_,
#                        lissy_attributes = list(level = "h",
#                                                merged_levels = FALSE,
#                                                database = "i"))
#
#   expect_equivalent(bind_lissy_files(list_),
#                     tibble::tibble(file = c(rep("aa00", 10), rep("aa01", 10)),
#                                    did = c(rep(1, 10), rep(2, 10)),
#                                    hid = rep(1:10, 2),
#                                    var_ = rep(5, 20),
#                                    unique_hid = c("1_1", "1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_8", "1_9",
#                                                   "1_10", "2_1", "2_2", "2_3", "2_4", "2_5", "2_6", "2_7", "2_8",
#                                                   "2_9", "2_10")
#                     ))
#
# })
#
#
# test_that("bind_lissy_files does not produce ID variables if create_unique_id = FALSE", {
#
#   aa00 <- tibble::tibble(did = 001,
#                          hid = 1:10,
#                          pid = 1:10,
#                          var_ = rep(5, 10))
#
#   aa01 <- tibble::tibble(did = 002,
#                          hid = 1:10,
#                          pid = 1:10,
#                          var_ = rep(5, 10))
#
#   list_ <- list(
#     `aa00` = aa00,
#     `aa01` = aa01
#   )
#
#   list_ <- set_lissy_attributes(lissy_object = list_,
#                        lissy_attributes = list(level = "p",
#                                                merged_levels = FALSE,
#                                                database = "i"))
#
#   expect_equivalent(bind_lissy_files(list_, create_unique_id = FALSE),
#                     tibble::tibble(file = c(rep("aa00", 10), rep("aa01", 10)),
#                                    did = c(rep(1, 10), rep(2, 10)),
#                                    hid = rep(1:10, 2),
#                                    pid = rep(1:10, 2),
#                                    var_ = rep(5, 20)
#                     ))
#
# })
#
#
# test_that("bind_lissy_files keeps the attributes", {
#
#   aa00 <- tibble::tibble(did = 001,
#                          hid = 1:10,
#                          pid = 1:10,
#                          var_ = rep(5, 10))
#
#   aa01 <- tibble::tibble(did = 002,
#                          hid = 1:10,
#                          pid = 1:10,
#                          var_ = rep(5, 10))
#
#   attr(aa00, "level") <- "person"
#   attr(aa01, "level") <- "person"
#
#   list_ <- list(
#     `aa00` = aa00,
#     `aa01` = aa01
#   )
#
#   list_ <- set_lissy_attributes(lissy_object = list_,
#                                 lissy_attributes = list(level = "p",
#                                                         merged_levels = TRUE,
#                                                         database = "i"))
#
#     expect_equal(get_lissy_attributes(bind_lissy_files(list_)),
#                list(level = "p", merged_levels = TRUE, database = "i"))
#
# })
#
#
#
# # add_unique_ids ----------------------------------------------------------
#
# test_that("add_unique_ids adds the new variables for datasets at person and household levels", {
#
#   aa00 <- tibble::tibble(did = 5,
#                          hid = 1:10,
#                          pid = 1001:1010,
#                          var_ = rep(5, 10))
#
#   aa01 <- tibble::tibble(did = 5,
#                          hid = 1:10,
#                          var_ = rep(5, 10))
#
#   expect_equivalent(add_unique_ids(file = aa00,
#                                    file_name = "aa00",
#                                    file_level = "person"),
#                tibble::tibble(
#                  did = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#                  hid = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
#                  pid = c(1001L,1002L,1003L,1004L,1005L,1006L,
#                          1007L,1008L,1009L,1010L),
#                  var_ = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#                  unique_hid = c("5_1","5_2","5_3","5_4","5_5","5_6",
#                                 "5_7","5_8","5_9","5_10"),
#                  unique_pid = c("5_1001","5_1002","5_1003","5_1004",
#                                 "5_1005","5_1006","5_1007","5_1008","5_1009","5_1010")
#                ))
#
#   expect_equivalent(add_unique_ids(file = aa01,
#                                    file_name = "aa01",
#                                    file_level = "household"),
#                tibble::tibble(
#                  did = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#                  hid = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
#                  var_ = c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
#                  unique_hid = c("5_1","5_2","5_3","5_4","5_5","5_6",
#                                 "5_7","5_8","5_9","5_10")
#                ))
#
# })
#
#
#
# test_that("add_unique_ids throws an error if variables 'did' or 'hid' are not present. Error points at faulty dataset.", {
#
#   aa00ip <- tibble::tibble(did = 1:10,
#                          var_ = rep(5, 10))
#
#   aa01ih <- tibble::tibble(hid = 1:10,
#                          var_ = rep(5, 10))
#
#   expect_error(add_unique_ids(aa00ip, "aa00ip",  "person"), "This is not the case of 'aa00ip'")
#
#   expect_error(add_unique_ids(aa01ih, "aa01ih", "household"), "This is not the case of 'aa01ih'")
#
# })
#
#
# test_that("add_unique_ids throws an error if variable 'pid' is not present in a 'person' dataset. Error points at faulty dataset.", {
#
#   aa00ip <- tibble::tibble(did = 1:10,
#                          hid = 1:10,
#                          var_ = rep(5, 10))
#
#   expect_error(add_unique_ids(aa00ip, "aa00ip",  "person"), "This is not the case of 'aa00ip'")
#
# })
#
#
# test_that("add_unique_ids throws an error if 'file_level' argument has invalid value.", {
#
#   aa00ip <- tibble::tibble(did = 1:10,
#                            hid = 1:10,
#                            var_ = rep(5, 10))
#
#   expect_error(add_unique_ids(aa00ip, "aa00ip", file_level = "wong value"),
#                "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.",
#                fixed = TRUE)
#
# })
