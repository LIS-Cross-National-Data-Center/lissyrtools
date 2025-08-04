

#' Compute Weighted Percentiles cross a List of Data Frames (with optional grouping)
#' @description
#' Applies the `compute_weighted_percentiles()` function to each data frame in a named list, using a specified variable and optional weight. 
#'  Supports optional grouping via a categorical `by` variable, and returns either percentile values or share values, depending on the `share` argument.
#'
#' @param data_list A named list of data frames, (e.g., across countries or years).
#' @param var_name A string specifying the variable name (e.g., "dhi", "pilabour") to compute percentiles or shares for.
#' @param wgt_name An optional string specifying the weight variable to be used. If `NULL`, equal weights are assumed.
#' @param probs A numeric vector of probabilities with values between 0 and 1, defining percentiles (if `share = FALSE`) or the brackets between which shares are computed (if `share = TRUE`).
#' @param type A character string indicating which percentile definition to use. Only used when `share = FALSE`.
#' - `"type_4"`: default, linear interpolation-based of the empirical cdf - continuous sample quantile.
#' - `"type_2"`: used in Stata commands like collapse and _pctile, inverse of empirical distribution function with averaging at discontinuities - discontinuous sample quantile.
#' @param share Logical. If `TRUE`, returns `var_name`  shares between percentile brackets instead of the percentile values. Default is \code{FALSE}. 
#'   Note: This **always uses** `type = "type_4"` (interpolation), regardless of the `type` parameter. It cannot be combined with `type = "type_2"`.
#'   This cannot be set to `TRUE` if `average = TRUE`.
#' @param average Logical. If `TRUE`, returns `var_name` averages across percentile brackets instead of the percentile values. Default is \code{FALSE}. 
#'   Note: This **always uses** `type = "type_4"` (interpolation), regardless of the `type` parameter. It cannot be combined with `type = "type_2"`. 
#'   This cannot be set to `TRUE` if `share = TRUE`.
#' @param na.rm Logical. If `TRUE`, missing values in `var_name` or `wgt_name` are removed.
#' @param by Optional string giving the name of a categorical variable to split the data within each data frame before computing statistics. 
#' 
#' @return A named list.
#' 
#' - If `by == NULL`, `share == FALSE` and `length(probs) == 1`: each list element is named by country and contains a named numeric vector, where the names are years and values are the computed statistics.
#' 
#' - If `by == NULL`, and either `share == TRUE` or `length(probs) > 1`: each list element is named by `ccyy` (country-year) identifiers and contains a named numeric vector, where the names represent the share intervals or the percentiles defined in `probs`. 
#' 
#' - If `by != NULL`: the list has `ccyy` identifiers as keys. Each element is a sublist, named after the categories of the `by` variable. Each sublist contains a named numeric vector of computed statistics.
#' 
#' @details
#' Percentiles are computed using weighted version of **quantile definition 4** from Hyndman and Fan (1996), by default, or **quantile definition 2** if specified. 
#' When `share = TRUE`, the function estimates Lorenz ordinates by taking quantiles from the running sum of the ordered outcome variable (divided by the total), 
#' according to the same **quantile definition 4 only**.
#' 
#' @export
#'
#' @examples
#' \dontrun{  
#' library(lissyrtools)
#' library(purrr)
#' library(dplyr)
#' 
#' # Import data
#' my_data_list <- lissyuse(data = "es", from  = 2016)
#' 
#' # Retrieve the percentile estimates
#' percentiles_result <- my_data_list %>% 
#'   run_weighted_percentiles(
#'     var_name = "dhi",
#'     wgt_name = "hpopwgt",
#'     probs = seq(0.1, 0.9, 0.1),
#'     type = "type_2",
#'     na.rm = TRUE
#' )
#' print(percentiles_result)
#' 
#' # Compute the distribution shares
#' shares_result <- run_weighted_percentiles(
#'     data_list = my_data_list,
#'     var_name = "dhi",
#'     wgt_name = "hpopwgt",
#'     probs = seq(0, 1, 0.1),
#'     share = TRUE,
#'     na.rm = TRUE
#' )
#' print(shares_result)
#' 
#' # Compute averages for each distribution group 
#' averages_result <- run_weighted_percentiles(
#'     data_list = my_data_list,
#'     var_name = "dhi",
#'     wgt_name = "hpopwgt",
#'     probs = seq(0, 1, 0.1),
#'     average = TRUE,
#'     na.rm = TRUE
#' )
#' print(averages_result)
#' 
#' # Using the by option 
#' by_result_median <- run_weighted_percentiles(
#'     data_list = purrr::map(my_data_list[1:2], ~.x %>% filter(emp == 1)),
#'     var_name = "pi11",
#'     wgt_name = "ppopwgt",
#'     probs = 0.5,
#'     type = "type_4",
#'     na.rm = TRUE, 
#'     by = "region_c"
#'    
#' )
#' print(by_result_median)
#' }
run_weighted_percentiles <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  probs = seq(0, 1, 0.25),
  type = c("type_4", "type_2"),
  share = FALSE,
  average = FALSE,
  na.rm = TRUE,
  by = NULL
) {
  
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
    data_list,
    wgt_name
  ) # return a list cleaned

  # Check that var_name exists
  assertthat::assert_that(
    var_name %in% names(data_list[[1]]),
    msg = glue::glue(
      "Variable '{var_name}' could not be found as a column name in the datasets."
    )
  )

  # Check that all variables in `by` exist, if provided
  if (!is.null(by)) {
    assertthat::assert_that(
      by %in% names(data_list[[1]]),
      msg = glue::glue(
        "Grouping variable '{by}' could not be found as a column name in the datasets."
      )
    )
  }

  # Check that wgt_name exists, if provided
  if (!is.null(wgt_name)) {
    assertthat::assert_that(
      wgt_name %in% names(data_list[[1]]),
      msg = glue::glue(
        "Weight variable '{wgt_name}' could not be found as a column name in the datasets."
      )
    )
  }
  lissyrtools::check_input_in_weight_argument(wgt_name)

  type <- match.arg(type)
  if ((share || average) && type != "type_4") {
    warning("When `share = TRUE`, `type` is ignored and set to 'type_4'.")
  }

  if (!is.null(by)) {
    recommended_categoricals <- c(
      lissyrtools::lis_categorical_variables,
      lissyrtools::lws_wealth_categorical_variables,
      "inum"
    )
    if (!by %in% recommended_categoricals) {
      warning(sprintf(
        "The `by` variable is not recognized as a categorical variable in `lissyrtools::lis_categorical_variables`, `lissyrtools::lws_wealth_categorical_variables`, or as the variable 'inum'."
      ))
    }

    df_to_keep <- purrr::map_lgl(data_list, ~ !all(is.na(.x[[by]])))
    # change the data frames in `data_list` !
    if (any(!df_to_keep)) {
      warning(sprintf(
        "The `by` variable '%s' contains only NAs in the following frames, which will be dropped: %s",
        by,
        paste(names(df_to_keep)[!df_to_keep], collapse = ", ")
      ))
      data_list <- data_list[df_to_keep]
    }
  }

  result <- purrr::imap(data_list, function(df, name) {
    if (!is.null(by)) {
      df[[by]] <- as.character(haven::as_factor(df[[by]]))
      df <- df[!is.na(df[[by]]), ]
      split_df <- split(df, df[[by]])

      result <- purrr::imap(split_df, function(sub_df, group_name) {
        var <- sub_df[[var_name]]
        wgt <- if (!is.null(wgt_name)) sub_df[[wgt_name]] else NULL
        lissyrtools::compute_weighted_percentiles(
          var = var,
          wgt = wgt,
          probs = probs,
          share = share,
          average = average,
          type = if (share || average) "type_4" else type,
          na.rm = na.rm
        )
      })
      # Flatten if only one prob value (e.g., just median) and not share nor average
      if (length(probs) == 1 && share == FALSE && average == FALSE) {
        result <- purrr::map(result, ~ unname(.x)) %>% unlist()
      }

      result
    } else {
      var <- df[[var_name]]
      wgt <- if (!is.null(wgt_name)) df[[wgt_name]] else NULL
      lissyrtools::compute_weighted_percentiles(
        var = var,
        wgt = wgt,
        probs = probs,
        share = share,
        average = average,
        type = if (share || average) "type_4" else type,
        na.rm = na.rm
      )
    }
  })

  if (length(probs) == 1 && share == FALSE &&  average == FALSE && is.null(by)) {
    result <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(result)
  }

  return(result)
}
#' Compute Weighted Percentiles or Share of Distribution
#' 
#' @description
#' This function computes weighted percentiles of a numeric vector, or the share of total value within specified percentile intervals.
#'
#'
#' @param var A numeric vector of values (e.g., .x$dhi, .x$pi11).
#' @param wgt A numeric vector of weights (e.g., .x$hpopwgt, .x$pwgt). Must be the same length as \code{x}.
#' @param probs A numeric vector of probabilities between 0 and 1 indicating which percentiles to compute. Default is \code{seq(0, 1, 0.25)}.
#' @param na.rm Logical; if \code{TRUE}, missing values in \code{x} and \code{w} are removed before computation. Default is \code{TRUE}.
#' @param share Logical; if \code{TRUE}, computes the share of total value (e.g., .x$dhi) within each interval defined by \code{probs}. If \code{FALSE}, returns the percentile values. Default is \code{FALSE}. Note: This option always uses \code{type = "type_4"}, and can not be used toghether with \code{type = "type_2"} or \code{average = "TRUE"}.
#' @param average Logical; if \code{TRUE}, computes the weighted mean of `var` (e.g., .x$dhi) within each interval defined by \code{probs}. If \code{FALSE}, returns the percentile values. Default is \code{FALSE}. Note: This option always uses \code{type = "type_4"}, and can not be used toghether with \code{type = "type_2"}  or \code{share = "TRUE"}.
#' @param type A character string indicating which percentile definition to use. Either \code{"type_4"} (default, linear interpolation-based of the empirical cdf - continuous sample quantile) or 
#' \code{"type_2"} (used in Stata commands like collapse and _pctile, inverse of empirical distribution function with averaging at discontinuities - discontinuous sample quantile).
#' 
#' @return A named numeric vector. If \code{share = FALSE}, returns weighted percentiles with names corresponding to the percentiles (e.g., "25%"). If \code{share = TRUE}, returns the share of the total value in each percentile range (e.g., "0-25%").  
#'
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' data <- lissyrtools::lissyuse(data = "es22", vars = c("dhi", "age"))
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt)
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01))
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01), share = TRUE)
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01), average = TRUE)
#' }
compute_weighted_percentiles <- function(
  var,
  wgt = NULL,
  probs = seq(0, 1, 0.25),
  na.rm = TRUE,
  share = FALSE,
  average = FALSE,
  type = c("type_4", "type_2")
) {
  type <- match.arg(type)

  # Enforce mutual exclusivity: share and average cannot both be TRUE
  if (share && average) {
    stop("Only one of 'share' or 'average' can be TRUE at the same time.")
  }

  if (is.null(wgt)) {
    wgt <- rep(1, length(var))
  }
  stopifnot(length(var) == length(wgt))

  if (any(probs > 1) || any(probs < 0)) {
    stop(
      glue::glue(
        "Values in the argument 'probs' must be between 0 and 1 (inclusive)."
      )
    )
  }

  # Remove NAs if needed
  if (na.rm) {
    keep <- !is.na(var) & !is.na(wgt)
    var <- var[keep]
    wgt <- wgt[keep]
  }

  # Sort var and wgt
  ord <- order(var, wgt)
  var <- var[ord]
  wgt <- wgt[ord]

  # Sort probs
  probs <- probs[order(probs)]
  probs <- if (share == TRUE | average == TRUE) {
    unique(c(0, probs, 1))
  } else {
    probs
  }

  w_total <- sum(wgt)
  cw <- cumsum(wgt)
  target <- probs * w_total
  # Neeeded for the shares
  cxw <- cumsum(var * wgt)

  # --- type (Stata-like (collapse / _pctile approximation.)) implementation ---
  if (type == "type_2" && !share && !average) {
    result <- numeric(length(probs))
    for (i in seq_along(probs)) {
      t <- target[i]
      if (t == 0) {
        result[i] <- var[1]
      } else if (t == w_total) {
        result[i] <- var[length(var)]
      } else {
        idx <- which(cw > t)[1]
        if (any((t - cw) < .Machine$double.eps^0.5 & t >= cw)) {
          #  < .Machine$double.eps^0.5 for eveb more precision
          # in practise if cw[idx-1] == t
          # Interpolate between adjacent values if needed (matches boundary)
          result[i] <- (var[idx - 1] + var[idx]) / 2
        } else {
          result[i] <- var[idx]
        }
      }
    }
    names(result) <- paste0(probs * 100, "%")
    return(result)
  }
  # --- END: Definition 2 ---

  if (type == "type_4" && !share && !average) {
    # Definition 4 (used in percentils in STATA from Philip van Kerm)

    result <- numeric(length(probs))

    for (i in seq_along(probs)) {
      t <- target[i]

      # If the target is 0 or total weight, assign the first or last value
      if (t == 0) {
        # t == 0
        # Min.
        result[i] <- var[1]
      } else if (t == w_total) {
        # Max
        result[i] <- var[length(var)]
      } else {
        idx <- which(cw[-1] > t & cw[-length(cw)] <= t)[1] + 1
        result[i] <- var[idx - 1] +
          (var[idx] - var[idx - 1]) * ((t - cw[idx - 1]) / wgt[idx])
      }
    }

    # Set the names for the result
    names(result) <- paste0(probs * 100, "%")
    return(result)
  } 
  if (share == TRUE) {
    # SHARE calculation (using only type 4)
    result_for_shares <- numeric(length(probs) - 1)
    for (i in 1:(length(probs) - 1)) {
      if (i == 1) {
        t <- probs[2] * sum(wgt)
        idx <- which(cw[-1] > t & cw[-length(cw)] <= t)[1] + 1

        Yi_minus_1_W <- (cxw[idx - 1] / cxw[length(var)])
        Yi_W <- (cxw[idx] / cxw[length(var)])
        gamma <- (t - cw[idx - 1]) / (cw[idx] - cw[idx - 1])

        result_for_shares[1] <- 100 *
          ((1 - gamma) * Yi_minus_1_W + gamma * Yi_W)
      } else if (i == (length(probs) - 1)) {
        t <- probs[i] * sum(wgt)
        idx <- which(cw[-1] > t & cw[-length(cw)] <= t)[1] + 1

        Yi_minus_1_W <- (cxw[idx - 1] / cxw[length(var)])
        Yi_W <- (cxw[idx] / cxw[length(var)])
        gamma <- (t - cw[idx - 1]) / (cw[idx] - cw[idx - 1])

        result_for_shares[length(result_for_shares)] <- (100 -
          ((1 - gamma) * Yi_minus_1_W + gamma * Yi_W) * 100)
      } else {
        # for example probs = 0.5
        t_up <- probs[i + 1] * sum(wgt)
        idx_up <- which(cw[-1] > t_up & cw[-length(cw)] <= t_up)[1] + 1

        Yi_minus_1_W_up <- (cxw[idx_up - 1] / cxw[length(var)])
        Yi_W_up <- (cxw[idx_up] / cxw[length(var)])
        gamma_up <- (t_up - cw[idx_up - 1]) / (cw[idx_up] - cw[idx_up - 1])

        # for example probs = 0.25
        t_low <- probs[i] * sum(wgt)
        idx_low <- which(cw[-1] > t_low & cw[-length(cw)] <= t_low)[1] + 1

        Yi_minus_1_W_low <- (cxw[idx_low - 1] / cxw[length(var)])
        Yi_W_low <- (cxw[idx_low] / cxw[length(var)])
        gamma_low <- (t_low - cw[idx_low - 1]) / (cw[idx_low] - cw[idx_low - 1])

        result_for_shares[i] <- (((1 - gamma_up) *
          Yi_minus_1_W_up +
          gamma_up * Yi_W_up) -
          ((1 - gamma_low) * Yi_minus_1_W_low + gamma_low * Yi_W_low)) *
          100
      }
    }

    probs_percent <- round(probs * 100)
    interval_labels <- paste0(
      probs_percent[-length(probs_percent)],
      "-",
      probs_percent[-1],
      "%"
    )
    names(result_for_shares) <- interval_labels
    return(result_for_shares)
  } 
  if (average == TRUE) {
    result_for_averages <- numeric(length(probs) - 1)
    for (i in 1:(length(probs) - 1)) {
      p_low <- probs[i] * w_total
      p_high <- probs[i + 1] * w_total

      # Find lower idx and interpolate
      idx_low <- which(cw >= p_low)[1]
      if (idx_low == 1) {
        low_val <- var[1]
        low_cum_val <- 0
      } else {
        gamma_low <- (p_low - cw[idx_low - 1]) / (cw[idx_low] - cw[idx_low - 1])
        low_cum_val <- cxw[idx_low - 1] +
          gamma_low * (cxw[idx_low] - cxw[idx_low - 1])
      }

      # Find upper idx and interpolate
      idx_high <- which(cw >= p_high)[1]
      if (idx_high == 1) {
        high_val <- var[1]
        high_cum_val <- 0
      } else {
        gamma_high <- (p_high - cw[idx_high - 1]) /
          (cw[idx_high] - cw[idx_high - 1])
        high_cum_val <- cxw[idx_high - 1] +
          gamma_high * (cxw[idx_high] - cxw[idx_high - 1])
      }

      # Total weighted sum in percentile interval
      total_weighted_sum <- high_cum_val - low_cum_val
      total_weight <- p_high - p_low

      # Average income in this percentile interval
      result_for_averages[i] <- total_weighted_sum / total_weight
    }

    names(result_for_averages) <- paste0(
      probs[-length(probs)] * 100,
      "-",
      probs[-1] * 100,
      "%"
    )
    return(result_for_averages)
  }
}