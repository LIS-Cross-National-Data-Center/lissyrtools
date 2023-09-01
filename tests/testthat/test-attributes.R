library(lissyrtools)
library(here)


.run_local_tests <- (Sys.info()[["effective_user"]] == "josep" && Sys.info()[["nodename"]] == "DEVV-CT01")

dir_testdata <- testdata_path()



# get_lissy_attributes ----------------------------------------------------

test_that("get_lissy_attributes works as expected", {

  x <- 1:3
  y <- 1:3
  z <- 1:3
  l <- 1:3

  attr(x, "merged_levels") <- FALSE
  attr(x, "level") <- "p"
  attr(x, "database") <- "i"

  attr(y, "merged_levels") <- FALSE
  attr(y, "database") <- "i"

  attr(l, "another_attribute") <- "whatever"

  expect_equal(get_lissy_attributes(x),
               list(level = "p", merged_levels = FALSE, database = "i"))

  expect_equal(get_lissy_attributes(y),
               list(level = NULL, merged_levels = FALSE, database = "i"))

  expect_equal(get_lissy_attributes(z),
               NULL,
               fixed = TRUE)

  expect_equal(get_lissy_attributes(l),
               NULL,
               fixed = TRUE)

})


# set_lissy_attributes ----------------------------------------------------

test_that("set_lissy_attributes works as expected", {

  x <- 1:3
  y <- 1:3

  lissy_attributes_x <- list(merged_levels = FALSE,
                        level = "p",
                        database = "i")

  lissy_attributes_y <- list(merged_levels = FALSE,
                             database = "i")

  expect_equal(set_lissy_attributes(x, lissy_attributes_x),
               structure(1:3, level = "p", merged_levels = FALSE, database = "i"))

  expect_error(set_lissy_attributes(x, lissy_attributes_y),
               "'lissy_attributes' must be a list with the following elements: 'level', 'merged_levels', 'database'.",
               fixed = TRUE)

})




# has_lissy_attributes ----------------------------------------------------

test_that("has_lissy_attributes returns TRUE if list has all or some lissy attributes", {

  list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(1,10),
                                            hwgt = 1,
                                            relation = 1000),
                 dataset_2 = tibble::tibble(pi11 = c(rep(1, 9), 0),
                                            hwgt = 1,
                                            relation = 1000))

  list_2 <- list_1
  list_3 <- list_1


  attr(list_1, "merged_levels") <- FALSE
  attr(list_1, "level") <- "p"
  attr(list_1, "database") <- "i"


  attr(list_2, "merged_levels") <- FALSE
  attr(list_2, "database") <- "i"

  attr(list_3, "merged_levels") <- FALSE
  attr(list_3, "level") <- "p"
  attr(list_3, "ANOTHER_ATTRIBUTE") <- "whatever"


  # has all lissy attributes
  expect_equal(has_lissy_attributes(list_1), TRUE)

  # some some (not all) lissy attributes
  expect_equal(has_lissy_attributes(list_2), TRUE)

  # some lissy attributes, some non-lissy attributes
  expect_equal(has_lissy_attributes(list_3), TRUE)

})


test_that("has_lissy_attributes returns FALSE if list has no lissy attributes", {


  list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(1,10),
                                            hwgt = 1,
                                            relation = 1000),
                 dataset_2 = tibble::tibble(pi11 = c(rep(1, 9), 0),
                                            hwgt = 1,
                                            relation = 1000))

  list_2 <- list_1


  attr(list_2, "NON_LISSY_ATTRIBUTE") <- "aa"
  attr(list_2, "ANOTHER_ATTRIBUTE") <- "bb"

  # other attributes
  expect_equal(has_lissy_attributes(list_1), FALSE)

  # no attributes
  expect_equal(has_lissy_attributes(list_2), FALSE)


})



# paste_lissy_attributes --------------------------------------------------


test_that("paste_lissy_attributes works as expected when lissy_attributes is not NULL", {

  list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(1,10),
                                            hwgt = 1,
                                            relation = 1000),
                 dataset_2 = tibble::tibble(pi11 = c(rep(1, 9), 0),
                                            hwgt = 1,
                                            relation = 1000))

  list_2 <- list(dataset_1 = tibble::tibble(pi11 = rep(1,10),
                                            hwgt = 1,
                                            relation = 1000),
                 dataset_2 = tibble::tibble(pi11 = c(rep(1, 9), 0),
                                            hwgt = 1,
                                            relation = 1000))

  attr(list_1, "merged_levels") <- FALSE
  attr(list_1, "level") <- "p"
  attr(list_1, "database") <- "i"

  list_2 <- paste_lissy_attributes(list_2, get_lissy_attributes(list_1))

  expect_equal(has_lissy_attributes(list_2), TRUE)

  expect_equal(get_lissy_attributes(list_2), get_lissy_attributes(list_1))

  expect_equal(get_lissy_attributes(list_2),
               list(level = "p", merged_levels = FALSE, database = "i"))

})


test_that("paste_lissy_attributes works as expected when lissy_attributes is NULL", {

  list_1 <- list(dataset_1 = tibble::tibble(pi11 = rep(1,10),
                                            hwgt = 1,
                                            relation = 1000),
                 dataset_2 = tibble::tibble(pi11 = c(rep(1, 9), 0),
                                            hwgt = 1,
                                            relation = 1000))

  list_2 <- list(dataset_1 = tibble::tibble(pi11 = rep(1,10),
                                            hwgt = 1,
                                            relation = 1000),
                 dataset_2 = tibble::tibble(pi11 = c(rep(1, 9), 0),
                                            hwgt = 1,
                                            relation = 1000))

  list_2 <- paste_lissy_attributes(list_2,
                                   get_lissy_attributes(list_1)) # this is equal to NULL

  expect_equal(has_lissy_attributes(list_2), FALSE)

  expect_equal(get_lissy_attributes(list_2), get_lissy_attributes(list_1))

  expect_equal(get_lissy_attributes(list_2),
               NULL)

})
