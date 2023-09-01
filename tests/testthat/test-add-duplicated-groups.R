library(lissyrtools)
context("Function for duplicating repeated percentiles in compute_lorenz_curve")



test_that("add_duplicated_groups returns expected outputs", {

  x <-  c(1:15)
  list_groups <- list(c(1, 2, 3),
                      c(6, 7, 8),
                      c(10, 11))


  expect_equal(add_duplicated_groups(x = x,
                                     list_index_duplicated_groups = list_groups),

               c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 6L, 6L, 6L, 7L, 8L, 9L, 10L, 10L,
                 11L, 12L, 13L, 14L, 15L))


})
