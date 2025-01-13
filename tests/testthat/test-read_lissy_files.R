library(testthat)
library(lissyrtools)
library(tibble)
library(here)

# dir_testdata <- "C:/Users/jespa/OneDrive/Desktop/Projects/LIS/lissyrtools/tests/testdata/"


dir_testdata <- testdata_path()


test_that("read_lissy_files_locally merges LIS files correctly",{

   files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih", "us16ih"), path_to_files = dir_testdata))

   expect_equal(is.list(files_h), TRUE)
   expect_equal(is.data.frame(files_h[["it2014ih"]]), TRUE)
   expect_equal(all(names(files_h) == c("it2014ih", "us2016ih")), TRUE)

})


test_that("read_lissy_files_locally outputs lists with the correct attributes",{

  files_ih <- suppressWarnings(read_lissy_files_locally(files = c("it14ih", "us16ih"), path_to_files = dir_testdata))
  files_ip <- suppressWarnings(read_lissy_files_locally(files = c("it14ip", "us16ip"), path_to_files = dir_testdata))

  files_wh <- suppressWarnings(read_lissy_files_locally(files = c("it14wh", "us16wh"), path_to_files = dir_testdata))
  files_wp <- suppressWarnings(read_lissy_files_locally(files = c("it14wp", "us16wp"), path_to_files = dir_testdata))

  expect_equal(attr(files_ih,
                    "merged_levels"),
               FALSE)

  expect_equal(attr(files_ip,
                    "merged_levels"),
               FALSE)

  expect_equal(attr(files_ih,
                    "database"),
               "i")

  expect_equal(attr(files_ip,
                    "database"),
               "i")

  expect_equal(attr(files_wh,
                    "database"),
               "w")

  expect_equal(attr(files_wp,
                    "database"),
               "w")

  expect_equal(attr(files_ih,
                    "level"),
               "h")

  expect_equal(attr(files_ip,
                    "level"),
               "p")

})


test_that("read_lissy_files_locally throws error with invalid file names", {

  expect_error(read_lissy_files_locally(c("invalid.dta"), path_to_files = "testdata/"), "Argument 'files' should specify file names in 'ccyydl' or 'ccyyyydl' format.")

})


test_that("read_lissy_files_locally selects specified columns correctly", {

  files_ih <- suppressWarnings(read_lissy_files_locally(files = c("it14ih"), path_to_files = dir_testdata, col_select = c("hid", "dhi")))
  files_ip <- suppressWarnings(read_lissy_files_locally(files = c("it14ip"), path_to_files = dir_testdata, col_select = c("hid", "pi11")))

  files_wh <- suppressWarnings(read_lissy_files_locally(files = c("it14wh"), path_to_files = dir_testdata, col_select = c("hid", "dhi")))
  files_wp <- suppressWarnings(read_lissy_files_locally(files = c("it14wp"), path_to_files = dir_testdata, col_select = c("hid", "pi11")))

  expect_equal(names(files_ih[[1]]), c("hid", "dhi", "nhhmem", "hwgt", "hpopwgt", "hwgta", "dname", "iso2", "currency"))
  expect_equal(names(files_ip[[1]]), c("hid", "pi11", "relation", "pwgt", "ppopwgt", "pwgta", "pid", "dname", "iso2", "currency"))

  expect_equal(names(files_wh[[1]]), c("hid", "inum", "dhi", "nhhmem", "hwgt", "hpopwgt", "hwgta", "dname", "iso2", "currency"))
  expect_equal(names(files_wp[[1]]), c("hid", "inum" , "pid", "pi11", "relation", "pwgt", "ppopwgt", "pwgta", "dname", "iso2", "currency"))

})


test_that("read_lissy_files_locally returns names in 'ccyyyydl' format when full_year_names is TRUE", {

  files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih"), path_to_files = dir_testdata, full_year_names = TRUE))
  expect_equal(names(files_h), "it2014ih")

})

test_that("read_lissy_files_locally returns names in 'ccyydl' format when full_year_names is FALSE", {

  files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih"), path_to_files = dir_testdata, full_year_names = FALSE))
  expect_equal(names(files_h), "it14ih")

})



# read_rename_files -------------------------------------------------------


# Test renaming with valid new names
test_that("read_rename_files works with valid new names", {

  files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih", "us16ih"), path_to_files = dir_testdata))

  renamed_files <- read_rename_files(files_h, c("Italy_2014_household", "US_2016_household"))

  expect_equal(names(renamed_files), c("Italy_2014_household", "US_2016_household"))
})

# Test with mismatched lengths
test_that("read_rename_files throws error with mismatched lengths", {

  files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih", "us16ih"), path_to_files = dir_testdata))

  expect_error(read_rename_files(files_h, c("France_1984_household")),
               "The length of 'new_names' should be the same as the lengh of the list of files.")
})

# Test with NA values
test_that("read_rename_files throws error with NA values", {

  files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih", "us16ih"), path_to_files = dir_testdata))

  expect_error(read_rename_files(files_h, c("Italy_2014_household", NA)),
               "'new_names' should not contain NAs.")
})



# retrieve_database_from_directory ----------------------------------------

test_that("retrieve_database_from_directory works as expected", {

  assign(x = "LIS_DIR", value = "/lisdata/r/", envir = globalenv())

  expect_equal(retrieve_database_from_directory(), "i")

  assign(x = "LIS_DIR", value = "/lwsdata/r/", envir = globalenv())

  expect_equal(retrieve_database_from_directory(), "w")

  assign(x = "LIS_DIR", value = "/erflisdata/r/", envir = globalenv())

  expect_equal(retrieve_database_from_directory(), "e")

  assign(x = "LIS_DIR", value = "/another/", envir = globalenv())

  expect_error(retrieve_database_from_directory(),
               "Database couldn't be retrieved from '/another/'")

  assign(x = "LIS_DIR", value = "anotherlws/", envir = globalenv())

  expect_error(retrieve_database_from_directory(),
               "Database couldn't be retrieved from 'anotherlws/'")

})

