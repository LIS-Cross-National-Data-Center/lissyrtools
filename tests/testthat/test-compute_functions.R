library(lissyrtools)
context("Helper functions for computing estimates")
set.seed(4)


test_that("print_gini returns expected Gini coefficients", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3), weight = c(1, 1, 1)),
                      file2 = data.frame(var1 = c(4, 5, 6), weight = c(1, 1, 1)))

  result <- print_gini(lissy_files, "var1", na.rm = FALSE)
  expect_equal(class(result), "numeric")
  expect_named(result, c("file1", "file2"))
})

test_that("print_gini handles NA values correctly", {
  lissy_files <- list(file1 = data.frame(var1 = c(1, 2, 3, NA), weight = c(1, 1, 1, 1)),
                      file2 = data.frame(var1 = c(4, 5, 6, NA), weight = c(1, 1, 1, 1)))

  result <- print_gini(lissy_files, "var1", na.rm = FALSE)
  expect_true(any(is.na(result)))

  result <- print_gini(lissy_files, "var1", na.rm = TRUE)
  expect_true(all(!is.na(result)))
})

test_that("print_gini handles all NA or zero values in variable", {
  lissy_files <- list(file1 = data.frame(var1 = c(NA_integer_, NA_integer_, NA_integer_), weight = c(1, 1, 1)),
                      file2 = data.frame(var1 = c(0, 0, 0), weight = c(1, 1, 1)))

  result <- print_gini(lissy_files, "var1", na.rm = FALSE)
  expect_true(all(is.na(result)))
})
