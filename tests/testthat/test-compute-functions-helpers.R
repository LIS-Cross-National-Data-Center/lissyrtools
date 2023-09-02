library(lissyrtools)
context("Helper functions for computing estimates")
set.seed(4)



# checks_compute_functions ------------------------------------------------

test_that("checks_compute_functions passes if no errors are found", {

  file_<- tibble::tibble(`one` = c(1:10), `two` = c(11:20), `pwgt` = 1)

  expect_error(checks_compute_functions(file = file_,
                                        file_name = "aa55i",
                                        variable = "one",
                                        weight = 'pwgt'),
               NA)

  expect_error(checks_compute_functions(file = file_,
                                        file_name = "aa55i",
                                        variable = "one"),
               NA)

})


test_that("checks_compute_functions throws error if file is not data.frame", {

  vector_object <- c(`one` = 1, `two` = 2)

  list_object <- list(`one` = c(1:10), `two` = c(11:20))

  expect_error(checks_compute_functions(file = vector_object,
                                        file_name = "aa55i",
                                        variable = "one"),
               "Argument 'file' in 'aa55i' needs to be a data.frame/tibble type of object.",
               fixed = TRUE)

  expect_error(checks_compute_functions(file = list_object,
                                        file_name = "aa55i",
                                        variable ="one"),
               "Argument 'file' in 'aa55i' needs to be a data.frame/tibble type of object.",
               fixed = TRUE)

})


test_that("checks_compute_functions throws error if variable not in file", {

  file_<- tibble::tibble(`one` = c(1:10), `two` = c(11:20), `hwgt` = 1)

  expect_error(checks_compute_functions(file = file_,
                                        file_name = "aa55i",
                                        variable = "three"),
               "'three' was not found in 'aa55i'",
               fixed = TRUE)


})


test_that("checks_compute_functions throws error if weight variable not in file", {

  file_1 <- tibble::tibble(`one` = c(1:10), `two` = c(11:20))

  file_2 <- tibble::tibble(`one` = c(1:10), `two` = c(11:20), hwgt = 1)


  expect_error(checks_compute_functions(file = file_1,
                                        file_name = "aa55i",
                                        variable = "one",
                                        weight = 'hwgt'),
               "'hwgt' was not found in 'aa55i'.",
               fixed = TRUE)

  expect_error(checks_compute_functions(file = file_1,
                                        file_name = "aa55i",
                                        variable = "one",
                                        weight = 'pwgt'),
               "'pwgt' was not found in 'aa55i'.",
               fixed = TRUE)

  expect_error(checks_compute_functions(file = file_1,
                                        file_name = "aa55i",
                                        variable = "one",
                                        weight = 'my_weight'),
               "'my_weight' was not found in 'aa55i'.",
               fixed = TRUE)

  expect_error(checks_compute_functions(file = file_2,
                                        file_name = "aa55i",
                                        variable = "one",
                                        weight = 'pwgt'),
               "'pwgt' was not found in 'aa55i'.",
               fixed = TRUE)

})



test_that("checks_compute_functions throws error if variable is not numeric", {

  file_ <- tibble::tibble(`one` = c(letters),  `hwgt` = 1)

  expect_error(checks_compute_functions(file = file_,
                                        file_name = "aa55i",
                                        variable = "one"),
               "The argument 'variable' needs to be numeric.")

})



test_that("checks_compute_functions throws error if weight variable contains only NAs or zeroes", {

  file_ <- tibble::tibble(`one` = c(1:5), `my_weight` = c(NA))

  expect_error(checks_compute_functions(file_, "one", "my_weight"))

})
