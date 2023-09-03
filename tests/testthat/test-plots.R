# library(testthat)
# library(ggplot2)
# library(purrr)  # Make sure to load purrr
#
# # Test for plot_lorenz_curve
# test_that("plot_lorenz_curve returns a ggplot object", {
#   dataset1 <- data.frame(dhi = c(1, 2, 3, 4, 5))
#   dataset2 <- data.frame(dhi = c(2, 3, 4, 5, 6))
#   result <- plot_lorenz_curve(list(dataset1, dataset2), "dhi")
#   expect_is(result, "ggplot")
# })
#
# # Test for plot_indicator
# test_that("plot_indicator returns a ggplot object", {
#   dataset1 <- data.frame(dhi = c(1, 2, 3, 4, 5))
#   dataset2 <- data.frame(dhi = c(2, 3, 4, 5, 6))
#   result <- plot_indicator(list(dataset1, dataset2), "dhi", "mean")
#   expect_is(result, "ggplot")
# })
#
# test_that("plot_indicator throws an error for unsupported indicators", {
#   dataset1 <- data.frame(dhi = c(1, 2, 3, 4, 5))
#   dataset2 <- data.frame(dhi = c(2, 3, 4, 5, 6))
#   expect_error(plot_indicator(list(dataset1, dataset2), "dhi", "unsupported_indicator"))
# })
