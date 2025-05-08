# library(lissyrtools)
# library(testthat)
# library(ggplot2)
# # library(purrr)  # Make sure to load purrr
# #
# # # Test for plot_lorenz_curve
# # test_that("plot_lorenz_curve returns a ggplot object", {
# #   dataset1 <- data.frame(dhi = c(1, 2, 3, 4, 5))
# #   dataset2 <- data.frame(dhi = c(2, 3, 4, 5, 6))
# #   result <- plot_lorenz_curve(list(dataset1, dataset2), "dhi")
# #   expect_is(result, "ggplot")
# # })
# #
# # # Test for plot_indicator
# # test_that("plot_indicator returns a ggplot object", {
# #   dataset1 <- data.frame(dhi = c(1, 2, 3, 4, 5))
# #   dataset2 <- data.frame(dhi = c(2, 3, 4, 5, 6))
# #   result <- plot_indicator(list(dataset1, dataset2), "dhi", "mean")
# #   expect_is(result, "ggplot")
# # })
# #
# # test_that("plot_indicator throws an error for unsupported indicators", {
# #   dataset1 <- data.frame(dhi = c(1, 2, 3, 4, 5))
# #   dataset2 <- data.frame(dhi = c(2, 3, 4, 5, 6))
# #   expect_error(plot_indicator(list(dataset1, dataset2), "dhi", "unsupported_indicator"))
# # })
# 
# 
# # plot_indicator ----------------------------------------------------------
# 
# # Sample data for testing
# sample_lissy_files <- list(
#   us2010ih = data.frame(dhi = rnorm(100), hwgt = 1),
#   us2011ih = data.frame(dhi = rnorm(100), hwgt = 1),
#   ca2010ih = data.frame(dhi = rnorm(100), hwgt = 1),
#   ca2011ih = data.frame(dhi = rnorm(100), hwgt = 1)
# )
# 
# test_that("plot_indicator returns a ggplot2 plot for valid inputs", {
#   plot <- plot_indicator(lissy_files = sample_lissy_files, variable = "dhi", indicator = "mean")
#   expect_true(is.ggplot(plot))
# })
# 
# test_that("plot_indicator throws an error for invalid lissy_files names", {
#   invalid_files <- list(
#     invalid_namei = data.frame(dhi = rnorm(100))
#   )
#   expect_error(plot_indicator(invalid_files, "dhi", "mean"))
# })
# 
# test_that("plot_indicator throws an error for invalid type values", {
#   expect_error(plot_indicator(sample_lissy_files, "dhi", "mean", type = "invalid_type"))
# })
# 
# test_that("plot_indicator throws an error for invalid indicator values", {
#   expect_error(plot_indicator(sample_lissy_files, "dhi", "invalid_indicator"))
# })
# 
# test_that("plot_indicator handles NA values based on the na.rm parameter", {
#   files_with_na <- list(
#     us2010ih = data.frame(dhi = c(NA, rnorm(99)))
#   )
#   expect_error(
#     plot_indicator(lissy_files = files_with_na, variable = "dhi", indicator = "mean"),
#     "None of the indicators computed had valid values. Did you forget to pass na.rm = TRUE?"
#   )
#   expect_silent(plot_indicator(files_with_na, "dhi", "mean", na.rm = TRUE))
# })
# 
# test_that("plot_indicator handles an empty list of lissy_files", {
#   expect_error(plot_indicator(list(), "dhi", "mean"))
# })
# 
# test_that("plot_indicator handles missing parameters correctly", {
#   expect_silent(plot_indicator(sample_lissy_files, "dhi", "mean"))
# })
# 
# 
# # decide_plot_type --------------------------------------------------------
# # Test for a data frame with multiple countries and multiple years
# results_df1 <- data.frame(
#   file = c("us2010h", "us2011h", "ca2010h", "ca2011h"),
#   country = c("US", "US", "CA", "CA"),
#   year = c(2010, 2011, 2010, 2011),
#   value = c(1, 2, 3, 4)
# )
# expected_output1 <- "line"
# test_that("decide_plot_type() returns the correct plot type for a data frame with multiple countries and multiple years", {
#   expect_equal(decide_plot_type(results_df1), expected_output1)
# })
# 
# # Test for a data frame with multiple years and only one country
# results_df2 <- data.frame(
#   file = c("us2010h", "us2011h"),
#   country = c("US", "US"),
#   year = c(2010, 2011),
#   value = c(1, 2)
# )
# expected_output2 <- "line"
# test_that("decide_plot_type() returns the correct plot type for a data frame with multiple years and only one country", {
#   expect_equal(decide_plot_type(results_df2), expected_output2)
# })
# 
# # Test for a data frame with only one year and multiple countries
# results_df3 <- data.frame(
#   file = c("us2010h", "ca2010h", "mx2010h"),
#   country = c("US", "CA", "MX"),
#   year = c(2010, 2010, 2010),
#   value = c(1, 2, 3)
# )
# expected_output3 <- "bar"
# test_that("decide_plot_type() returns the correct plot type for a data frame with only one year and multiple countries", {
#   expect_equal(decide_plot_type(results_df3), expected_output3)
# })
# 
# # Test for a data frame with only one year and only one country
# results_df4 <- data.frame(
#   file = c("us2010h"),
#   country = c("US"),
#   year = c(2010),
#   value = c(1)
# )
# expected_output4 <- "bar"
# test_that("decide_plot_type() returns the correct plot type for a data frame with only one year and only one country", {
#   expect_equal(decide_plot_type(results_df4), expected_output4)
# })
# 
# 
# 
# # plot_line ---------------------------------------------------------------
# results_df <- data.frame(
#   country = c("us", "us", "us", "us", "ca", "ca", "ca", "ca"),
#   file = c("us2010h", "us2011h", "us2012h", "us2013h", "ca2010h", "ca2011h", "ca2012h", "ca2013h"),
#   year = c(2010L, 2011L, 2012L, 2013L, 2010L, 2011L, 2012L, 2013L),
#   value = c(1, 2, 3, 4, 3, 4, 5, 6)
# )
# 
# # Test that the function returns a ggplot object
# test_that("plot_line returns a ggplot object", {
#   expect_s3_class(plot_line(results_df), "ggplot")
# })
# 
# # Test that the function plots the correct x-axis values
# test_that("plot_line plots the correct x-axis values", {
#   p <- plot_line(results_df)
#   expect_equal(sort(unique(results_df$year)), sort(unique(p$data[["year"]])))
# })
# 
# # Test that the function plots the correct y-axis values
# test_that("plot_line plots the correct y-axis values", {
#   p <- plot_line(results_df)
#   expect_equal(sort(unique(results_df$value)), sort(unique(p$data[["value"]])))
# })
# 
# 
# # plot_bar ----------------------------------------------------------------
# 
# # Create a test data frame
# results_df <- data.frame(
#   file = c("us2010h", "us2011h", "ca2010h", "ca2011h"),
#   country = c("us", "us", "ca", "ca"),
#   year = c(2010, 2011, 2010, 2011),
#   value = c(1, 2, 3, 4)
# )
# 
# # Test that the function returns a ggplot object
# test_that("plot_bar returns a ggplot object", {
#   expect_s3_class(plot_bar(results_df), "ggplot")
# })
# 
# test_that("plot_line plots the correct x-axis values", {
#   p <- plot_line(results_df)
#   expect_equal(sort(unique(results_df$year)), sort(unique(p$data[["year"]])))
# })
# 
# # Test that the function plots the correct y-axis values
# test_that("plot_line plots the correct y-axis values", {
#   p <- plot_line(results_df)
#   expect_equal(sort(unique(results_df$value)), sort(unique(p$data[["value"]])))
# })
