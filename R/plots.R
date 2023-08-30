
# LIS colors palette


lis_colors <- c("#3891af", "#c8cccf", "#006d91", "#5ba7c1", "#6C848C", "#d5490c")

lis_theme <- list(ggplot2::theme(text=ggplot2::element_text(size=16),
                                     plot.background = ggplot2::element_rect(fill = "#6C848C",
                                                                             colour = "black"),
                                     panel.grid.major = ggplot2::element_line(size = 0.5,
                                                                              linetype = 'solid',
                                                                              colour = "#c8cccf"),
                                     panel.grid.minor = ggplot2::element_blank(),
                                     panel.background = ggplot2::element_rect(fill = "white",
                                                                              colour = "black",
                                                                              size = 2,
                                                                              linetype = "solid"),
                                     axis.text =  ggplot2::element_text(colour = "black")),
                      ggplot2::scale_color_manual(values = lis_colors),
                      ggplot2::scale_fill_manual(values = lis_colors))




#' Plot Lorenz Curve.
#'
#'\lifecycle{experimental}
#' Computes the equivalised percentiles of an indicator and plots the Lorenz
#' Curve for one or multiple LIS or LWS datasets into a single graph.
#'
#' @param list_datasets A list of LIS or LWS datasets.
#' @param variable A character vector of length one.
#' @examples
#' \dontrun{
#' datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' plot_lorenz_curve(datasets, "dhi")
#' }
plot_lorenz_curve <- function(list_datasets, variable, plot_theme = NULL, na.rm = FALSE){

  percentiles_ <- map_dfr(list_datasets,
                          ~ compute_percentiles(.x, variable = variable, na.rm = na.rm) %>%
                            mutate(cum_value = cumsum(value)/sum(value)*100),
                          .id = "file")

  plot <- ggplot(data = percentiles_,
         aes(x = percentile, y = cum_value, col = file, group = file, type = file)) +
    geom_line(size = 1.2) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, size = 1.2) +
  scale_x_continuous(name = "Percentile", limits=c(0,100)) +
    scale_y_continuous(name = glue::glue("Cumulative share of {variable}"), limits=c(-10,100))

  if(!is.null(plot_theme)){

    if(plot_theme == "lis"){

      plot <- plot + lis_theme

    }else{
      warning("Currently only 'lis theme is supported'.")

    }

  }

    return(plot)
}




#' Plot an Indicator.
#'
#'\lifecycle{experimental}
#' Computes an indicator and plots it for one or multiple LIS or LWS datasets.
#'
#' @param list_datasets A list of LIS or LWS datasets.
#' @param variable A character string indicating the aggregate for which the indicator needs to be plotted.
#' @param indicator A character string indicating the type of indicator to be computed.
#'   Currently the function supports only 'mean', 'median', 'ratio' and 'gini'.
#' @param theme A ggplot2 theme for the graph.
#' @param ratio Only used in the computation of 'ratio' indicator. A vector of
#' two numeric values between 0 and 1. Defines the percentiles in the numerator
#' and denominator respectively. E.g. (0.9, 0.1) computes the 90/10 ratio.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
plot_indicator <- function(list_datasets, variable, indicator, plot_theme = NULL, ratio = NULL, na.rm = FALSE, ...){

  assertthat::assert_that(indicator %in% c("mean", "median", "ratio", "gini"),
                          msg = "Value for 'indicator' argument must be one of the following: 'mean', 'median', 'ratio' and 'gini' as values for ")

  if(indicator == "mean"){

    indicator <- purrr::map_dbl(list_datasets, .f = function(x){compute_mean(x, variable, na.rm)})

  }else if(indicator == "median"){

    indicator <- purrr::map_dbl(list_datasets, .f = function(x){compute_median(x, variable, na.rm)})

  }else if(indicator == "ratio"){

    if(is.null(ratio)){ratio <- c(0.9, 0.1)}
    warning("No argument was passed to 'ratio' so c(0.9, 0.1) was specified as default.")
    indicator <- purrr::map_dbl(list_datasets, .f = function(x){compute_ratio(x, variable, ratio = ratio, na.rm = na.rm)})

  }else if(indicator == "gini"){

    indicator <- purrr::map_dbl(list_datasets, .f = function(x){compute_gini(x, variable, na.rm)})

  }

  plot <- tibble::enframe(indicator, name = "file") %>%
    ggplot2::ggplot(ggplot2::aes(x = file, y = value)) +
    ggplot2::geom_col(colour = lis_colors[[1]], fill = lis_colors[[1]])

  if(!is.null(plot_theme)){

    if(plot_theme == "lis"){

      plot <- plot + lis_theme

    }else{
     warning("Currently only 'lis theme is supported'.")

    }
  }
  plot

}

