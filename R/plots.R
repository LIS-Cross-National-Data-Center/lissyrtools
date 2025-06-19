# LIS colors palette


# lis_colors <- c("#3891af", "#c8cccf", "#006d91", "#5ba7c1", "#6C848C", "#d5490c")
# 
# lis_theme <- list(
#   ggplot2::theme(
#     text = ggplot2::element_text(size = 16),
#     plot.background = ggplot2::element_rect(
#       fill = "#6C848C",
#       colour = "black"
#     ),
#     panel.grid.major = ggplot2::element_line(
#       size = 0.5,
#       linetype = "solid",
#       colour = "#c8cccf"
#     ),
#     panel.grid.minor = ggplot2::element_blank(),
#     panel.background = ggplot2::element_rect(
#       fill = "white",
#       colour = "black",
#       size = 2,
#       linetype = "solid"
#     ),
#     axis.text = ggplot2::element_text(colour = "black")
#   ),
#   ggplot2::scale_color_manual(values = lis_colors),
#   ggplot2::scale_fill_manual(values = lis_colors)
# )


#' Plot an Indicator.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Computes an indicator and plots it for one or multiple LIS or LWS datasets.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string indicating the aggregate for which the indicator needs to be computed.
#' @param indicator A character string indicating the type of indicator statistic to be computed.
#'   Currently the function supports only 'mean', 'median', 'ratio' and 'gini'.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights. If NULL (default), the function tries to guess the
#'   needed weight to compute the indicator. This guess is made on the information
#'   from files_level and variable_level.
#' @param type A character vector indicating the type of plot to be used. Valid
#'  inputs are 'line' and 'bar'. If NULL (default), the function tries to guess
#' the appropriate plot type based on the structure of the input data frame.
#' @param plot_theme A character vector.    
#' @param ratio A vector of two numeric values between 0 and 1.Only used in the computation of 'ratio' indicator.
#'   Defines the percentiles in the numerator and denominator respectively.
#'   E.g. (0.9, 0.1) computes the 90/10 ratio.
#' @param epsilon A numeric vector of length one. Only used in the computation of 'atkinson' indicator'.
#'   The inequality adversion parameter. Needs to be epsilon > 0.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
#' @param files_level  A string indicating the level of the file. Valid inputs are:
#'   'household', 'h', 'person' or 'p'. If NULL (default), the file level will
#'   be retrived from the 'lissy_files' attributes.
#' @param variable_level Level of the variable. Should be either 'household', 'h', 'person' or 'p'.
#'   If NULL (default), the function will try to guess the level of the variable.
#'   This is done by comparing the value in 'variable' with pre-set lists of variables.
#'
#' @return A ggplot2 plot.
plot_indicator <- function(lissy_files, variable, indicator, weight = NULL, type = NULL, plot_theme = NULL, ratio = NULL, epsilon = NULL, na.rm = FALSE, files_level = NULL, variable_level = NULL) {
  results <- print_indicator(lissy_files, variable, indicator,
    weight = weight,
    ratio = ratio, epsilon = epsilon, na.rm = na.rm, files_level = files_level,
    variable_level = variable_level
  )

  assertthat::assert_that(any(!is.na(results)),
    msg = "None of the indicators computed had valid values. Did you forget to pass na.rm = TRUE?"
  )

  results_df <- tibble::enframe(results, name = "file")

  assertthat::assert_that(all(stringr::str_detect(results_df$file, "\\w{2}\\d{2,4}[iw](hp)?")),
    msg = "Names of the 'lissy_files' must contain 'ccyyd', 'ccyyyyd', 'ccyydl' or 'ccyyyydl'"
  )

  # compute country variable
  results_df$country <- stringr::str_match(results_df$file, pattern = "(\\w{2})\\d{2,4}[iw]")[, 2]

  # compute year variable
  results_df$year <- stringr::str_match(results_df$file, pattern = "\\w{2}(\\d{2,4})[iw]")[, 2]

  if (missing(type)) {
    type <- decide_plot_type(results_df)
  } else {
    assertthat::assert_that(type %in% c("line", "bar"))
  }

  if (type == "line") {
    return(plot_line(results_df) +
      lis_theme)
  } else if (type == "bar") {
    return(plot_bar(results_df) +
      lis_theme)
  }
}



#' Decide plot type based on input data frame
#'
#' This function determines the appropriate plot type based on the structure of the input data frame.
#'   Use only within 'plot_indicator()'
#'
#' @param results_df A data frame containing the results of the indicator. Needs to contain
#'  'file', 'country', 'year' and 'value' columns. Use only within 'plot_indicator()
#'
#' @return A character string indicating the appropriate plot type. The possible values are:
#'   - "line": A line plot with one or multiple lines.
#'   - "bar": A bar plot with one or multiple bars.
#'
#' @details The function determines the plot type based on the number of countries and years in the input data frame.
#'  If the data frame has multiple countries and multiple years, the plot type is a line plot with multiple lines.
#' If the data frame has multiple years and only one country, the plot type is a line plot with one line.
#' If the data frame has only one year and multiple countries, the plot type is a bar plot with multiple bars (sorted).
#' If the data frame has only one year and only one country, the plot type is a bar plot with one bar.
#'
#' @examples
#' # Create a test data frame
#' results_df <- data.frame(
#'   file = c("us2010h", "us2011h", "ca2010h", "ca2011h"),
#'   year = c(2010, 2011, 2010, 2011),
#'   value = c(1, 2, 3, 4)
#' )
#'
#' # Determine the appropriate plot type
#' decide_plot_type(results_df)
#' # Output: "line"
#'
#' @keywords internal
decide_plot_type <- function(results_df) {
  # Extract Countries from files 
  results_df$country <- stringr::str_sub(results_df$file,1,2)
  # Does it have multiple countries?
  multiple_countries_bool <- length(unique(results_df$country)) > 1

  # Does it have multiple years for any country?
  multiple_year_bool <- dplyr::group_by(results_df, country) %>%
    dplyr::summarise(n_years = length(unique(year))) %>%
    dplyr::filter(n_years > 1) %>%
    nrow() > 0

  # If it has multiple years and multiple countries:
  # * Plot should be a line plot with multiple lines
  # If it has multiple years and only one country:
  # * Plot should be a line plot with one line
  # If it has only one year and multiple countries:
  # * Plot should be a bar plot with multiple bars (sorted)
  # If it has only one year and only one country:
  # * Plot should be a bar plot with one bar

  # If it has multiple years and multiple countries:
  if (multiple_year_bool && multiple_countries_bool) {
    return("line")
  } else if (multiple_year_bool && !multiple_countries_bool) {
    return("line")
  } else if (!multiple_year_bool && multiple_countries_bool) {
    return("bar")
  } else if (!multiple_year_bool && !multiple_countries_bool) {
    return("bar")
  }
}


#' Plot an indicator using a line plot.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param results_df A data frame with the results of the indicator.
#'
#' @return A ggplot2 plot.
#'
#' @keywords internal
plot_line <- function(results_df) {
  ggplot2::ggplot(
    data = results_df,
    mapping = ggplot2::aes(x = year, y = value, group = country, colour = country)
  ) +
    ggplot2::geom_line()
}


#' Plot an indicator using a bar plot.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param results_df A data frame with the results of the indicator.
#'
#' @return A ggplot2 plot.
#'
#' @keywords internal
plot_bar <- function(results_df) {
  ggplot2::ggplot(
    data = results_df,
    mapping = ggplot2::aes(x = reorder(country, value, mean), y = value, fill = country)
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_discrete(labels = stringr::str_to_title)
}
