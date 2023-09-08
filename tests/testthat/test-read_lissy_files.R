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

  files_h <- suppressWarnings(read_lissy_files_locally(files = c("it14ih", "us16ih"), path_to_files = dir_testdata))
  files_p <- suppressWarnings(read_lissy_files_locally(files = c("it14ip", "us16ip"), path_to_files = dir_testdata))

  expect_equal(attr(files_h,
                    "merged_levels"),
               FALSE)

  expect_equal(attr(files_p,
                    "merged_levels"),
               FALSE)

  expect_equal(attr(files_h,
                    "database"),
               "i")

  expect_equal(attr(files_p,
                    "database"),
               "i")

  expect_equal(attr(files_h,
                    "level"),
               "h")

  expect_equal(attr(files_p,
                    "level"),
               "p")

})


test_that("read_lissy_files_locally throws error with invalid file names", {
  expect_error(read_lissy_files_locally(c("invalid.dta"), path_to_files = "testdata/"), "Argument 'files' should specify file names in 'ccyydl' or 'ccyyyydl' format.")
})
