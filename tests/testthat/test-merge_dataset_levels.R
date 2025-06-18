#' # test-merge_dataset_levels.R
#' 
#' library(testthat)
#' library(lissyrtools)
#' library(tibble)
#' library(here)
#' 
#' 
#' .run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")
#' 
#' dir_testdata <- testdata_path()
#' 
#' #' TO DO:
#' #' Change test path
#' #' Test wrong values in database attribute of inputed list
#' #' Test throws warning if not all Not all 'hid's in 'pfile_name' appear in '{hfile_name - line 147 of code
#' #' Test throws warning if Not all 'hid's in 'hfile_name' appear in 'pfile_name' - line 183 of code
#' #' Test throws warning if Not all 'hid's in '{pfile_name}' appear in '{hfile_name}' - line 187 of code
#' #' Test throws warning if Not all combinations of 'inum' and 'hid's in '{hfile_name}' appear in '{pfile_name}'. - line 191
#' #' Test throws warning if Not all combinations of 'inum' and 'hid's in '{pfile_name}' appear in '{hfile_name}'. - line 195
#' #'
#' 
#' 
#' # merge_dataset_levels ----------------------------------------------------
#' 
#' if(.run_local_tests){
#' 
#'   test_that("merge_dataset_levels merges LIS files correctly",{
#' 
#'     ih_1 <- list(it14ih =read_LIS(LIS_DIR = dir_testdata, file_name = "it14ih"),
#'                  us16ih = read_LIS(LIS_DIR = dir_testdata, file_name = "us16ih"))
#' 
#'     ih_2 <- list(it2014ih =read_LIS(LIS_DIR = dir_testdata, file_name = "it14ih"),
#'                  us2016ih = read_LIS(LIS_DIR = dir_testdata, file_name = "us16ih"))
#' 
#'     ip_1 <- list(it14ip = read_LIS(LIS_DIR = dir_testdata, file_name = "it14ip"),
#'                  us16ip = read_LIS(LIS_DIR = dir_testdata, file_name = "us16ip"))
#' 
#'     ip_2 <- list(it2014ip = read_LIS(LIS_DIR = dir_testdata, file_name = "it14ip"),
#'                  us2016ip = read_LIS(LIS_DIR = dir_testdata, file_name = "us16ip"))
#' 
#'     ip_3 <- list(us16ip = read_LIS(LIS_DIR = dir_testdata, file_name = "us16ip"),
#'                  it14ip = read_LIS(LIS_DIR = dir_testdata, file_name = "it14ip"))
#' 
#'     attr(ih_1, "level") <- "h"
#'     attr(ih_1, "database") <- "i"
#'     attr(ih_1, "merged_levels") <- FALSE
#' 
#'     attr(ip_1, "level") <- "p"
#'     attr(ip_1, "database") <- "i"
#'     attr(ip_1, "merged_levels") <- FALSE
#' 
#'     attr(ih_2, "level") <- "h"
#'     attr(ih_2, "database") <- "i"
#'     attr(ih_2, "merged_levels") <- FALSE
#' 
#'     attr(ip_2, "level") <- "p"
#'     attr(ip_2, "database") <- "i"
#'     attr(ip_2, "merged_levels") <- FALSE
#' 
#'     attr(ip_3, "level") <- "p"
#'     attr(ip_3, "database") <- "i"
#'     attr(ip_3, "merged_levels") <- FALSE
#' 
#'     # files in 'ccyydl' format
#'     expect_equal(names(merge_dataset_levels(lissy_hfiles = ih_1,
#'                                             lissy_pfiles = ip_1)),
#'                  c("it14i", "us16i"))
#' 
#'     expect_s3_class(merge_dataset_levels(ih_1, ip_1)[["it14i"]],
#'                     "data.frame")
#' 
#'     expect_equal(
#'       sort(names(merge_dataset_levels(ih_1, ip_1)[["it14i"]])),
#'                  sort(union(names_ih_file, names_ip_file)))
#' 
#' 
#'     # files in 'ccyyyydl' format
#'     expect_equal(names(merge_dataset_levels(lissy_hfiles = ih_2,
#'                                             lissy_pfiles = ip_2)),
#'                  c("it2014i", "us2016i"))
#' 
#'     expect_s3_class(merge_dataset_levels(ih_2, ip_2)[["it2014i"]],
#'                     "data.frame")
#' 
#' 
#'     # datasets are the same, but sorted in different order
#'     expect_equal(names(merge_dataset_levels(lissy_hfiles = ih_1,
#'                                             lissy_pfiles = ip_3)),
#'                  c("it14i", "us16i"))
#' 
#' 
#'   })
#' 
#' 
#'   test_that("merge_dataset_levels merges LWS files correctly",{
#' 
#'     # test case when datasets are the same, but sorted in different order
#' 
#'     wh_1 <- list(it14wh =read_LIS(LIS_DIR = dir_testdata, file_name = "it14wh"),
#'                  us16wh = read_LIS(LIS_DIR = dir_testdata, file_name = "us16wh"))
#' 
#'     wh_2 <- list(it2014wh =read_LIS(LIS_DIR = dir_testdata, file_name = "it14wh"),
#'                  us2016wh = read_LIS(LIS_DIR = dir_testdata, file_name = "us16wh"))
#' 
#'     wp_1 <- list(it14wp = read_LIS(LIS_DIR = dir_testdata, file_name = "it14wp"),
#'                  us16wp = read_LIS(LIS_DIR = dir_testdata, file_name = "us16wp"))
#' 
#'     wp_2 <- list(it2014wp = read_LIS(LIS_DIR = dir_testdata, file_name = "it14wp"),
#'                  us2016wp = read_LIS(LIS_DIR = dir_testdata, file_name = "us16wp"))
#' 
#'     attr(wh_1, "level") <- "h"
#'     attr(wh_1, "database") <- "w"
#'     attr(wh_1, "merged_levels") <- FALSE
#' 
#'     attr(wp_1, "level") <- "p"
#'     attr(wp_1, "database") <- "w"
#'     attr(wp_1, "merged_levels") <- FALSE
#' 
#'     attr(wh_2, "level") <- "h"
#'     attr(wh_2, "database") <- "w"
#'     attr(wh_2, "merged_levels") <- FALSE
#' 
#'     attr(wp_2, "level") <- "p"
#'     attr(wp_2, "database") <- "w"
#'     attr(wp_2, "merged_levels") <- FALSE
#' 
#' 
#' 
#'     # files in 'ccyydl' format
#'     expect_equal(names(merge_dataset_levels(lissy_hfiles = wh_1,
#'                                             lissy_pfiles = wp_1)),
#'                  c("it14w", "us16w"))
#' 
#'     expect_s3_class(merge_dataset_levels(wh_1, wp_1)[["it14w"]],
#'                     "data.frame")
#' 
#'     expect_equal(
#'       sort(names(merge_dataset_levels(wh_1, wp_1)[["it14w"]])),
#'       sort(union(names_wh_file, names_wp_file)))
#' 
#' 
#'     # files in 'ccyyyydl' format
#'     expect_equal(names(merge_dataset_levels(lissy_hfiles = wh_2,
#'                                             lissy_pfiles = wp_2)),
#'                  c("it2014w", "us2016w"))
#' 
#'     expect_s3_class(merge_dataset_levels(wh_2, wp_2)[["it2014w"]],
#'                     "data.frame")
#' 
#'   })
#' 
#' }
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws errors if level attributes are not correct",{
#' 
#'   ih_list <- list(aa11ih = tibble(hid = 1:5),
#'                   bb22ih = tibble(hid = 1:5))
#' 
#'   ip_list <- list(aa11ip = tibble(hid = 1:5,
#'                                   pid = 1),
#'                   bb22ip = tibble(hid = 1:5,
#'                                   pid = 1))
#' 
#'   attr(ih_list, "database") <- "w"
#'   attr(ip_list, "database") <- "w"
#' 
#'   attr(ih_list, "merged_levels") <- FALSE
#'   attr(ip_list, "merged_levels") <- FALSE
#' 
#'   #' cases where input lists have no level
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Argument 'lissy_hfiles' needs to have a 'level' attribute.",
#'                fixed = TRUE)
#' 
#'   attr(ih_list, "level") <- "m" # wrong level attribute
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Argument 'lissy_pfiles' needs to have a 'level' attribute.",
#'                fixed = TRUE)
#' 
#'   attr(ip_list, "level") <- "n" # wrong level attribute
#' 
#' 
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Argument 'lissy_hfiles' needs to contain household-level files and have attribute 'level' equal to 'h'.",
#'                fixed = TRUE)
#' 
#'   attr(ih_list, "level") <- "h"
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Argument 'lissy_pfiles' needs to contain person-level files and have attribute 'level' equal to 'p'.",
#'                fixed = TRUE)
#' })
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws errors if database attributes are not correct",{
#' 
#'   ih_list <- list(aa11ih = tibble(hid = 1:5),
#'                   bb22ih = tibble(hid = 1:5))
#' 
#'   ip_list <- list(aa11ip = tibble(hid = 1:5,
#'                                   pid = 1),
#'                   bb22ip = tibble(hid = 1:5,
#'                                   pid = 1))
#' 
#' 
#'   attr(ih_list, "merged_levels") <- FALSE
#'   attr(ip_list, "merged_levels") <- FALSE
#' 
#'   attr(ip_list, "level") <- "p"
#'   attr(ih_list, "level") <- "h"
#' 
#' 
#'   #' cases where input lists have no database
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Argument 'lissy_hfiles' needs to have a 'database' attribute.",
#'                fixed = TRUE)
#' 
#'   attr(ih_list, "database") <- "n" # wrong database attribute
#' 
#'   #' cases where input lists have no database
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Argument 'lissy_pfiles' needs to have a 'database' attribute.",
#'                fixed = TRUE)
#' 
#' 
#'   attr(ip_list, "database") <- "m"  # wrong and different database attribute
#' 
#' 
#'   #' cases where input lists have different database value
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "'lissy_hfiles' and 'lissy_pfiles' should have the same value in 'database' attribute.",
#'                fixed = TRUE)
#' 
#'   attr(ip_list, "database") <- "n" # wrong but same database attribute
#' 
#' 
#'   #' cases where database value is not correct
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Unexpected value in 'database' attribute. Only 'i', 'e' and 'w' are accepted as valid.",
#'                fixed = TRUE)
#' 
#' 
#' })
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws an errors if 'merged_levels' attributes are not correct",{
#' 
#'   ih_list <- list(aa11ih = tibble(hid = 1:5),
#'                   bb22ih = tibble(hid = 1:5))
#' 
#'   ip_list <- list(bb22ih = tibble(hid = 1:5,
#'                                   pid = 1),
#'                   cc33ih = tibble(hid = 1:5,
#'                                   pid = 1))
#' 
#'   attr(ih_list, "database") <- "i"
#'   attr(ip_list, "database") <- "i"
#' 
#'   attr(ip_list, "level") <- "p"
#'   attr(ih_list, "level") <- "h"
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to have a 'merged_levels' argument equal to FALSE.",
#'                fixed = TRUE)
#' 
#'   attr(ih_list, "merged_levels") <- FALSE
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to have a 'merged_levels' argument equal to FALSE.",
#'                fixed = TRUE)
#' 
#'   attr(ip_list, "merged_levels") <- TRUE
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to have a 'merged_levels' argument equal to FALSE.",
#'                fixed = TRUE)
#' 
#' })
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws an error if both lists don't contain the same datasets",{
#' 
#'   ih_list_1 <- list(aa11ih = tibble(hid = 1:5),
#'                     bb22ih = tibble(hid = 1:5))
#' 
#'   ip_list_1 <- list(bb22ih = tibble(hid = 1:5,
#'                                   pid = 1),
#'                      cc33ih = tibble(hid = 1:5,
#'                                   pid = 1))
#' 
#'   ih_list_2 <- list(aa11ih = tibble(hid = 1:5),
#'                     bb22ih = tibble(hid = 1:5))
#' 
#'   ip_list_2 <- list(aa11ih = tibble(hid = 1:5),
#'                     bb22ih = tibble(hid = 1:5),
#'                     cc33ih = tibble(hid = 1:5))
#' 
#'   attr(ih_list_1, "database") <- "i"
#'   attr(ip_list_1, "database") <- "i"
#' 
#'   attr(ih_list_1, "level") <- "h"
#'   attr(ip_list_1, "level") <- "p"
#' 
#'   attr(ih_list_1, "merged_levels") <- FALSE
#'   attr(ip_list_1, "merged_levels") <- FALSE
#' 
#'   attr(ih_list_2, "database") <- "i"
#'   attr(ip_list_2, "database") <- "i"
#' 
#'   attr(ih_list_2, "level") <- "h"
#'   attr(ip_list_2, "level") <- "p"
#' 
#'   attr(ih_list_2, "merged_levels") <- FALSE
#'   attr(ip_list_2, "merged_levels") <- FALSE
#' 
#' 
#' expect_error(merge_dataset_levels(lissy_hfiles = ih_list_1,
#'                                   lissy_pfiles = ip_list_1),
#'              "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to contain files from the same datasets. The following datasets were found in 'lissy_pfiles' but not in 'lissy_hfiles' argument names: aa2011",
#'              fixed = TRUE)
#' 
#' 
#' expect_error(merge_dataset_levels(lissy_hfiles = ih_list_2,
#'                                   lissy_pfiles = ip_list_2),
#'              "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to contain files from the same datasets. The following datasets were found in 'lissy_hfiles' but not in 'lissy_pfiles' argument names: cc2033",
#'              fixed = TRUE)
#' 
#' })
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws an error if LWS files don't have 'inum'",{
#' 
#' wh_list_1 <- list(aa11wh = tibble(hid = 1:5,
#'                                 inum = 1),
#'                 bb22wh = tibble(hid = 1:5,
#'                                 inum = 1))
#' 
#' wp_list_1 <- list(aa11wh = tibble(hid = 1:5,
#'                                 pid = 1),
#'                 bb22wh = tibble(hid = 1:5,
#'                                 pid = 1))
#' 
#' wh_list_2 <- list(aa11wh = tibble(hid = 1:5),
#'                   bb22wh = tibble(hid = 1:5))
#' 
#' wp_list_2 <- list(aa11wh = tibble(hid = 1:5,
#'                                   pid = 1,
#'                                   inum = 1),
#'                   bb22wh = tibble(hid = 1:5,
#'                                   pid = 1,
#'                                   inum = 1))
#' 
#' attr(wh_list_1, "level") <- "h"
#' attr(wh_list_1, "database") <- "w"
#' attr(wh_list_1, "merged_levels") <- FALSE
#' 
#' attr(wp_list_1, "level") <- "p"
#' attr(wp_list_1, "database") <- "w"
#' attr(wp_list_1, "merged_levels") <- FALSE
#' 
#' attr(wh_list_2, "level") <- "h"
#' attr(wh_list_2, "database") <- "w"
#' attr(wh_list_2, "merged_levels") <- FALSE
#' 
#' attr(wp_list_2, "level") <- "p"
#' attr(wp_list_2, "database") <- "w"
#' attr(wp_list_2, "merged_levels") <- FALSE
#' 
#' 
#' expect_error(merge_dataset_levels(lissy_hfiles = wh_list_1,
#'                                   lissy_pfiles = wp_list_1),
#'              "'pfile' should contain 'inum' column.",
#'              fixed = TRUE)
#' 
#' expect_error(merge_dataset_levels(lissy_hfiles = wh_list_2,
#'                                   lissy_pfiles = wp_list_2),
#'              "'hfile' should contain 'inum' column.",
#'              fixed = TRUE)
#' 
#' })
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws an error if imported lists don't have names",{
#' 
#'   ih_list <- list(tibble(hid = 1:5),
#'                     tibble(hid = 1:5))
#' 
#'   ip_list <- list(tibble(hid = 1:5,
#'                            pid = 1),
#'                     tibble(hid = 1:5,
#'                            pid = 1))
#' 
#'   attr(ih_list, "database") <- "i"
#'   attr(ip_list, "database") <- "i"
#' 
#'   attr(ih_list, "level") <- "h"
#'   attr(ip_list, "level") <- "p"
#' 
#'   attr(ih_list, "merged_levels") <- FALSE
#'   attr(ip_list, "merged_levels") <- FALSE
#' 
#' 
#' expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                   lissy_pfiles = ip_list),
#'              "'lissy_hfiles' needs to be a named list containing h-level files.")
#' 
#' names(ih_list) <- c("aa11ih", "bb22ih")
#' 
#' expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                   lissy_pfiles = ip_list),
#'              "'lissy_pfiles' needs to be a named list containing p-level files.")
#' 
#' 
#' })
#' 
#' 
#' 
#' test_that("merge_dataset_levels merges throws an error if format names are not the same",{
#'   # within and between both lists
#' 
#'   ih_list <- list(aa11ih = tibble(hid = 1:5),
#'                   bb2022ih = tibble(hid = 1:5))
#' 
#'   ip_list <- list(aa11ip = tibble(hid = 1:5,
#'                          pid = 1),
#'                   bb2022ip = tibble(hid = 1:5,
#'                          pid = 1))
#' 
#'   attr(ih_list, "database") <- "i"
#'   attr(ip_list, "database") <- "i"
#' 
#'   attr(ih_list, "level") <- "h"
#'   attr(ip_list, "level") <- "p"
#' 
#'   attr(ih_list, "merged_levels") <- FALSE
#'   attr(ip_list, "merged_levels") <- FALSE
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "The format of 'lissy_hfiles' file names must be the same for all elements.")
#' 
#'   names(ih_list) <- c("aa11ih", "bb22ih") # same names in ih_list, but different in ip_list
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "The format of 'lissy_pfiles' file names must be the same for all elements.")
#' 
#'   names(ip_list) <- c("aa2011ip", "bb2022ip")  # same names in ip_list, but different format than in ih_list
#' 
#'   expect_error(merge_dataset_levels(lissy_hfiles = ih_list,
#'                                     lissy_pfiles = ip_list),
#'                "The format of file names in 'lissy_hfiles' must be the same as that in 'lissy_pfiles'.")
#' 
#' 
#' 
#' })
#' 
#' 
#' test_that("merge_dataset_levels outputs lists with the correct attributes",{
#' 
#'   h_list <- list(aa11ih = tibble(hid = 1:5),
#'                  bb22ih = tibble(hid = 1:5))
#' 
#'   p_list <- list(bb22ip = tibble(hid = 1:5,
#'                                  pid = 1),
#'                  aa11ip = tibble(hid = 1:5,
#'                                  pid = 1))
#' 
#'   attr(h_list, "level") <- "h"
#'   attr(h_list, "database") <- "i"
#'   attr(h_list, "merged_levels") <- FALSE
#' 
#'   attr(p_list, "level") <- "p"
#'   attr(p_list, "database") <- "i"
#'   attr(p_list, "merged_levels") <- FALSE
#' 
#'   expect_equal(attr(merge_dataset_levels(lissy_hfiles = h_list,
#'                                     lissy_pfiles = p_list),
#'                     "merged_levels"),
#'                TRUE)
#' 
#'   expect_equal(attr(merge_dataset_levels(lissy_hfiles = h_list,
#'                                          lissy_pfiles = p_list),
#'                     "level"),
#'                "p")
#' 
#'   expect_equal(attr(merge_dataset_levels(lissy_hfiles = h_list,
#'                                          lissy_pfiles = p_list),
#'                     "database"),
#'                "i")
#' 
#' })
#' 
#' 
#' 
#' # merge_lis_dataset_levels ------------------------------------------------
#' 
#' if(.run_local_tests){
#' 
#' test_that("merge_lis_dataset_levels merges LIS files correctly", {
#' 
#'   zz55ih <- read_LIS(LIS_DIR = dir_testdata, file_name = "zz55ih")
#'   zz55ip <- read_LIS(LIS_DIR = dir_testdata, file_name = "zz55ip")
#' 
#'   expect_equal(nrow(suppressWarnings(merge_lis_dataset_levels(zz55ih,
#'                                              zz55ip,
#'                                              hfile_name = "zz55ih",
#'                                              pfile_name = "zz55ip"))),
#'                5)
#' 
#'   expect_equal(unique(suppressWarnings(merge_lis_dataset_levels(zz55ih,
#'                                              zz55ip,
#'                                              hfile_name = "zz55ih",
#'                                              pfile_name = "zz55ip"))[["hid"]]),
#'                1:3)
#' 
#'   expect_warning(merge_lis_dataset_levels(zz55ih,
#'                                           zz55ip,
#'                                           hfile_name = "zz55ih",
#'                                           pfile_name = "zz55ip"),
#'                  "Not all 'hid's in 'zz55ih' appear in 'zz55ip'.",
#'                  fixed = TRUE)
#' 
#' })
#' 
#' }
#' 
#' 
#' test_that("merge_lis_dataset_levels throws a warning if not all hids in pfile appear in hfile", {
#' 
#'   expect_warning(merge_lis_dataset_levels(tibble::tibble(hid = 1:4),
#'                                         tibble::tibble(hid = 1:5,
#'                                                        pid = 1),
#'                                         hfile_name = "aa11ih",
#'                                         pfile_name = "aa11ip"),
#'                "Not all 'hid's in 'aa11ip' appear in 'aa11ih'.",
#'                fixed = TRUE)
#' 
#' })
#' 
#' 
#' test_that("merge_lis_dataset_levels throws a warning if not all hids in hfile appear in pfile", {
#' 
#'   expect_warning(merge_lis_dataset_levels(tibble::tibble(hid = 1:5),
#'                                           tibble::tibble(hid = 1:4,
#'                                                          pid = 1),
#'                                           hfile_name = "aa11ih",
#'                                           pfile_name = "aa11ip"),
#'                  "Not all 'hid's in 'aa11ih' appear in 'aa11ip'.",
#'                  fixed = TRUE)
#' 
#' })
#' 
#' 
#' if(.run_local_tests){
#' 
#' test_that("merge_lws_dataset_levels fails if 'hid' is not a column in one of the files", {
#' 
#'   zz55ih <- read_LIS(LIS_DIR = dir_testdata, file_name = "zz55ih")
#'   zz55ip <- read_LIS(LIS_DIR = dir_testdata, file_name = "zz55ip")
#' 
#'   zz55ih_nohid <- dplyr::select(zz55ih, -hid)
#'   zz55ip_nohid <- dplyr::select(zz55ip, -hid)
#' 
#'   expect_error(merge_lis_dataset_levels(zz55ih_nohid,
#'                                         zz55ip,
#'                                         hfile_name = "zz55ih",
#'                                         pfile_name = "zz55ip"),
#'                "'hfile' should contain 'hid' column.")
#' 
#'   expect_error(merge_lis_dataset_levels(zz55ih,
#'                                         zz55ip_nohid,
#'                                         hfile_name = "zz55ih",
#'                                         pfile_name = "zz55ip"),
#'                "'pfile' should contain 'hid' column.")
#' 
#' })
#' 
#' }
#' 
#' # merge_lws_dataset_levels ------------------------------------------------
#' 
#' 
#' test_that("merge_lws_dataset_levels merges LWS files correctly", {
#' 
#'   zzw55wh <- tibble::tibble(
#'     inum = c(rep(1, 5), rep(2, 5)),
#'     hid = c(rep(1:5, 2)),
#'     ppr = c(rep(c(8000, 2000, 16000, 7500, 15000), 2)),
#'     cname = "zz55w"
#' 
#'   )
#' 
#'   zzw55wp <- tibble::tibble(
#'     inum = c(rep(1, 10), rep(2, 10)),
#'     hid = rep(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 5), 2),
#'     pid = rep(c(1, 1, 2, 1, 2, 1, 2, 3, 1, 2), 2),
#'     pi33 = c(rep(c(seq(100, 1000, 100) ), 2)),
#'     cname = "zz55w"
#' 
#'   )
#' 
#'   expect_equal(nrow(merge_lws_dataset_levels(zzw55wh, zzw55wp, "zzw55wh", "zzw55wp")),
#'                20)
#' 
#'   expect_equal(unique(merge_lws_dataset_levels(zzw55wh, zzw55wp, "zzw55wh", "zzw55wp")[["hid"]]),
#'                1:5)
#' 
#'   ## contains only 1 column for 'cname'
#'   expect_equal(names(merge_lws_dataset_levels(zzw55wh, zzw55wp, "zzw55wh", "zzw55wp")),
#'                c("inum", "hid", "ppr", "cname", "pid", "pi33"))
#' 
#' })
#' 
#' 
#' test_that("merge_lws_dataset_levels fails if 'hid' or 'inum' are not a columns in one of the files", {
#' 
#'   zzw55wh <- tibble::tibble(
#'     inum = c(rep(1, 5), rep(2, 5)),
#'     hid = c(rep(1:5, 2)),
#'     ppr = c(rep(c(8000, 2000, 16000, 7500, 15000), 2)),
#'     cname = "zz55w"
#' 
#'   )
#' 
#'   zzw55wp <- tibble::tibble(
#'     inum = c(rep(1, 10), rep(2, 10)),
#'     hid = rep(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 5), 2),
#'     pid = rep(c(1, 1, 2, 1, 2, 1, 2, 3, 1, 2), 2),
#'     pi33 = c(rep(c(seq(100, 1000, 100) ), 2)),
#'     cname = "zz55w"
#' 
#'   )
#' 
#'   zzw55wh_noinum <- tibble::tibble(
#'     hid = c(rep(1:5, 2)),
#'     ppr = c(rep(c(8000, 2000, 16000, 7500, 15000), 2)),
#'     cname = "zz55w"
#' 
#'   )
#' 
#'   zzw55wp_nohid <- tibble::tibble(
#'     inum = c(rep(1, 10), rep(2, 10)),
#'     pid = rep(c(1, 1, 2, 1, 2, 1, 2, 3, 1, 2), 2),
#'     pi33 = c(rep(c(seq(100, 1000, 100) ), 2)),
#'     cname = "zz55w"
#' 
#'   )
#' 
#'   expect_error(merge_lws_dataset_levels(zzw55wh_noinum, zzw55wp),
#'                "'hfile' should contain 'inum' column.",
#'                fixed = TRUE)
#' 
#'   expect_error(merge_lws_dataset_levels(zzw55wh, zzw55wp_nohid),
#'                "'pfile' should contain 'hid' column.",
#'                fixed = TRUE)
#' 
#' })
#' 
#' 
#' 
#' 
#' test_that("merge_lws_dataset_levels throws a warning if not all hids and inum in pfile appear in hfile", {
#' 
#' 
#'   expect_warning(merge_lws_dataset_levels(tibble::tibble(hid = 1:4,
#'                                                          inum = 1),
#'                                           tibble::tibble(hid = 1:5,
#'                                                          pid = 1,
#'                                                          inum = 1),
#'                                           hfile_name = "aa11ih",
#'                                           pfile_name = "aa11ip"),
#'                  "Not all 'hid's in 'aa11ip' appear in 'aa11ih'.",
#'                  fixed = TRUE)
#' 
#'   expect_warning(merge_lws_dataset_levels(tibble::tibble(hid = 1:5,
#'                                                          inum = 1),
#'                                           tibble::tibble(hid = 1:5,
#'                                                          pid = 1,
#'                                                          inum = c(1,1,1,2,2)),
#'                                           hfile_name = "aa11ih",
#'                                           pfile_name = "aa11ip"),
#'                  "Not all combinations of 'inum' and 'hid's in 'aa11ip' appear in 'aa11ih'.",
#'                  fixed = TRUE)
#' 
#' })
#' 
#' 
#' test_that("merge_lws_dataset_levels throws a warning if not all hids and inum in hfile appear in pfile", {
#' 
#'   expect_warning(merge_lws_dataset_levels(tibble::tibble(hid = 1:5,
#'                                                          inum = 1),
#'                                           tibble::tibble(hid = 1:4,
#'                                                          pid = 1,
#'                                                          inum = 1),
#'                                           hfile_name = "aa11ih",
#'                                           pfile_name = "aa11ip"),
#'                  "Not all 'hid's in 'aa11ih' appear in 'aa11ip'.",
#'                  fixed = TRUE)
#' 
#'   expect_warning(merge_lws_dataset_levels(tibble::tibble(hid = 1:5,
#'                                                          inum = c(1,1,1,2,2)),
#'                                           tibble::tibble(hid = 1:5,
#'                                                          pid = 1,
#'                                                          inum = 1),
#'                                           hfile_name = "aa11ih",
#'                                           pfile_name = "aa11ip"),
#'                  "Not all combinations of 'inum' and 'hid's in 'aa11ih' appear in 'aa11ip'.",
#'                  fixed = TRUE)
#' 
#' })
#' 
#' 
#' 
#' 
#' # check_file_lists_contain_same_datasets ----------------------------------
#' 
#' test_that("check_file_lists_contain_same_datasets returns TRUE when datasets are the same in both lists", {
#' 
#'   list_h1 <- list('aa11ih' = tibble(), 'bb22ih' = tibble(),  'cc33ih' = tibble())
#' 
#'   list_h2 <- list('aa2011ih' = tibble(), 'bb2022ih' = tibble(),  'cc2033ih' = tibble())
#' 
#'   list_p1 <- list('aa11ip' = tibble(), 'bb22ip' = tibble(),  'cc33ip' = tibble())
#' 
#'   list_p2 <- list('aa2011ip' = tibble(), 'bb2022ip' = tibble(),  'cc2033ip' = tibble())
#' 
#' 
#'   expect_equal(check_file_lists_contain_same_datasets(list_h1, list_p1),
#'                TRUE)
#' 
#'   expect_equal(check_file_lists_contain_same_datasets(list_h2, list_p2),
#'                TRUE)
#' 
#' })
#' 
#' 
#' test_that("check_file_lists_contain_same_datasets throws an error if datasets are not the same in both lists", {
#' 
#'   list_h2 <- list('aa2011ih' = tibble(), 'bb2022ih' = tibble(),  'cc2033ih' = tibble())
#' 
#'   list_h3 <- list('aa2011ih' = tibble())
#' 
#'   list_p2 <- list('aa2011ip' = tibble(), 'bb2022ip' = tibble(),  'cc2033ip' = tibble())
#' 
#'   list_p3 <- list('aa2011ip' = tibble())
#' 
#'   expect_error(check_file_lists_contain_same_datasets(list_h2, list_p3),
#'                "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to contain files from the same datasets. The following datasets were found in 'lissy_pfiles' but not in 'lissy_hfiles' argument names: bb2022, cc2033")
#' 
#'   expect_error(check_file_lists_contain_same_datasets(list_h3, list_p2),
#'                "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to contain files from the same datasets. The following datasets were found in 'lissy_hfiles' but not in 'lissy_pfiles' argument names: bb2022, cc2033")
#' 
#' })
#' 
#' 
#' 
#' # sort_lissy_list ---------------------------------------------------------
#' 
#' # add two more tests:
#' # function works well if input lists are in 'ccyydl' format
#' # function throws error if format in 'list_to_sort' and 'according_to' are not the same.
#' 
#' test_that("sort_lissy_list works as expected", {
#' 
#'   list_to_sort_1 <- list(aa2011ih = tibble::tibble(), # needs to be in format 'ccyyyydl' or 'ccyydl'
#'                          bb2022ih = tibble::tibble())
#' 
#'   list_to_sort_2 <- list(aa2011ip = tibble::tibble(), # needs to be in format 'ccyyyydl' or 'ccyydl'
#'                          bb2022ip = tibble::tibble())
#' 
#'   list_to_sort_3 <- list(aa11ih = tibble::tibble(), # needs to be in format 'ccyyyydl' or 'ccyydl'
#'                          bb22ih = tibble::tibble())
#' 
#'   attr(list_to_sort_1, "database") <- "i"
#' 
#'   attr(list_to_sort_2, "database") <- "i"
#' 
#'   according_to_1 <- c("bb22ip", "aa11ip") # needs to be in 'ccyydl' or 'ccyyyydl' format
#' 
#'   according_to_2 <- c("bb22ih", "aa11ih") # needs to be in 'ccyydl' or 'ccyyyydl' format
#' 
#'   according_to_3 <- c("bb2022ih", "aa2011ih") # needs to be in 'ccyydl' or 'ccyyyydl' format
#' 
#' 
#'   expect_equal(names(sort_lissy_list(list_to_sort = list_to_sort_1,
#'                                according_to = according_to_1)),
#'                c("bb2022ih", "aa2011ih"))
#' 
#'   expect_equal(names(sort_lissy_list(list_to_sort = list_to_sort_2,
#'                                      according_to = according_to_2)),
#'                c("bb2022ip", "aa2011ip"))
#' 
#'   expect_equal(names(sort_lissy_list(list_to_sort = list_to_sort_3,
#'                                      according_to = according_to_3)),
#'                c("bb22ih", "aa11ih"))
#' 
#' })
#' 
#' 
#' test_that("sort_lissy_list throws error if the inputs are not in right format", {
#' 
#'   list_to_sort_1 <- list(aa11ih = tibble::tibble(),
#'                          bb2022ih = tibble::tibble())
#' 
#'   list_to_sort_2 <- list(aa2011ih = tibble::tibble(), # needs to be in format 'ccyyyydl'
#'                          bb2022ih = tibble::tibble())
#' 
#'   list_to_sort_3 <- list(aa11 = tibble::tibble(),
#'                          bb22 = tibble::tibble())
#' 
#'   attr(list_to_sort_1, "database") <- "i"
#' 
#'   attr(list_to_sort_3, "database") <- "i"
#' 
#'   according_to_1 <- c("bb22ip", "aa11ip") # needs to be in 'ccyydl' or 'ccyyyydl' format
#' 
#'   according_to_2 <- c("bb22p", "aa11p")
#' 
#'   expect_error(sort_lissy_list(list_to_sort = list_to_sort_1,
#'                                according_to = according_to_1),
#'                "All elements in 'list_to_sort' need to be in the same format",
#'                fixed = TRUE)
#' 
#'   expect_error(sort_lissy_list(list_to_sort_2,
#'                                according_to_2),
#'                "The change in format of the file name is not possible.",
#'                fixed = TRUE)
#' 
#'   expect_error(sort_lissy_list(list_to_sort_3,
#'                                according_to_2),
#'                "All elements in 'list_to_sort' need to be in 'ccyyyydl' or 'ccyydl' formats.",
#'                fixed = TRUE)
#' 
#' })
