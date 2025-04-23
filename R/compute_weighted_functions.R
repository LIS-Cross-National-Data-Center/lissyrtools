

#' Compute Weighted Percentiles Across a List of Data Frames
#' @description
#' Applies the `compute_weighted_percentiles()` function to each data frame in a named list, using a specified variable and optional weight. 
#' The function returns either percentile values or share values, depending on the `share` argument.
#'
#' @param data_list A named list of data frames, typically from multiple countries, years, or datasets.
#' @param var_name A string specifying the variable name for which percentiles or shares should be computed.
#' @param wgt_name An optional string specifying the weight variable to be used. If `NULL`, no weights are applied.
#' @param probs A numeric vector of probabilities with values between 0 and 1. Used to determine percentile cutoffs.
#' @param share Logical. If `TRUE`, returns income shares between percentile brackets instead of the percentile values.
#' @param na.rm Logical. If `TRUE`, missing values in `var_name` or `wgt_name` are removed.
#'
#' @return A named list containing the result (numeric vectors) for each element of `data_list`, with names inherited from the list.
#' 
#' @details
#' Percentiles are computed using the weighted version of **quantile definition 4** from Hyndman and Fan (1996). 
#' When `share = TRUE`, the function estimates Lorenz ordinates by taking quantiles from the running sum of the ordered outcome variable (divided by the total), 
#' according to the same quantile definition.
#' 
#' @export
#'
#' @examples
#' \dontrun{  
#' 
#' # Import data
#' my_data_list <- lissyuse(data = "es")
#' 
#' # Retrieve the percentile estimates
#' percentiles_result <- my_data_list %>% 
#'   transform_negative_values_to_zero("dhi") %>%
#'   run_weighted_percentiles(
#'     var_name = "dhi",
#'     wgt_name = "hpopwgt",
#'     probs = seq(0, 1, 0.1),
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
#' }
run_weighted_percentiles <- function(data_list, var_name, wgt_name = NULL, probs = seq(0, 1, 0.25), share = FALSE, na.rm = TRUE) {
 
   data_list <- lissyrtools::remove_dname_with_missings_in_weights(data_list, wgt_name) # return a list cleaned 
   lissyrtools::check_input_in_weight_argument(wgt_name) 
  
   output_run_weighted_percentiles <- purrr::imap(data_list, ~{
    var <- .x[[var_name]]
    wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else NULL
    compute_weighted_percentiles(var = var, wgt = wgt, probs = probs, share = share, na.rm = na.rm)
  })
   
  return(output_run_weighted_percentiles) 
}





#' Compute Weighted Percentiles or Share of Distribution
#' 
#' @description
#' This function computes weighted percentiles of a numeric vector, or the share of total value within specified percentile intervals.
#'
#'
#' @param x A numeric vector of values (e.g., .x$dhi, .x$pi11).
#' @param w A numeric vector of weights (e.g., .x$hpopwgt, .x$pwgt). Must be the same length as \code{x}.
#' @param probs A numeric vector of probabilities between 0 and 1 indicating which percentiles to compute. Default is \code{seq(0, 1, 0.25)}.
#' @param na.rm Logical; if \code{TRUE}, missing values in \code{x} and \code{w} are removed before computation. Default is \code{TRUE}.
#' @param share Logical; if \code{TRUE}, computes the share of total value (e.g., .x$dhi) within each interval defined by \code{probs}. If \code{FALSE}, returns the percentile values. Default is \code{FALSE}.
#'
#'
#' @return A named numeric vector. If \code{share = FALSE}, returns weighted percentiles with names corresponding to the percentiles (e.g., "25%"). If \code{share = TRUE}, returns the share of the total value in each percentile range (e.g., "0-25%"). 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt)
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01))
#' compute_weighted_percentiles(data$es22$dhi, data$es22$ppopwgt, probs = c(0.03, 0.72, 0.48, .01), share = TRUE)
#' }
compute_weighted_percentiles <- function(var, wgt = NULL, probs = seq(0, 1, 0.25), na.rm = TRUE, share = FALSE) {
  
  
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
  probs <- if (share == TRUE) unique(c(0,probs,1)) else probs
  
  w_total <- sum(wgt)
  cw <- cumsum(wgt)
  target <- probs * w_total
  
  # Neeeded for the shares
  cxw <- cumsum(var * wgt)
  
  if (!share) {
    # Result vector
    result <- numeric(length(probs))
    
    for (i in seq_along(probs)) {
      t <- target[i]
      
      
      # If the target is 0 or total weight, assign the first or last value
      if (t == 0) { # t == 0
        # Min.
        result[i] <- var[1]
      } else if (t == w_total) {
        # Max
        result[i] <- var[length(var)]
      } else {
        idx <- which(cw[-1] > t & cw[-length(cw)] <= t)[1] + 1
        result[i] <- var[idx - 1] + (var[idx] - var[idx - 1]) * ((t - cw[idx - 1]) / wgt[idx])
      }
    }
    
    # Set the names for the result
    names(result) <- paste0(probs * 100, "%")
    return(result)
    
    
  } else {
    result_for_shares <- numeric(length(probs) - 1)
    for (i in 1:(length(probs) - 1)) {
      
      if (i == 1) {
        t <- probs[2] * sum(wgt)
        idx <- which(cw[-1] > t & cw[-length(cw)] <= t)[1] + 1
        
        
        Yi_minus_1_W <- (cxw[idx - 1] / cxw[length(var)])
        Yi_W <- (cxw[idx] / cxw[length(var)])
        gamma <- (t - cw[idx - 1]) / (cw[idx] - cw[idx - 1])  
        
        result_for_shares[1] <- 100 * ((1 - gamma) * Yi_minus_1_W + gamma * Yi_W)
        
      } else if (i == (length(probs) - 1)) {
        t <- probs[i] * sum(wgt)
        idx <- which(cw[-1] > t & cw[-length(cw)] <= t)[1] + 1
        
        Yi_minus_1_W <- (cxw[idx - 1] / cxw[length(var)])
        Yi_W <- (cxw[idx] / cxw[length(var)])
        gamma <- (t - cw[idx - 1]) / (cw[idx] - cw[idx - 1])
        
        result_for_shares[length(result_for_shares)] <- (100 - ((1 - gamma) * Yi_minus_1_W + gamma * Yi_W) * 100)
        
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
        
        
        result_for_shares[i] <- (((1 - gamma_up) * Yi_minus_1_W_up + gamma_up * Yi_W_up) - ((1 - gamma_low) * Yi_minus_1_W_low + gamma_low * Yi_W_low)) * 100
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
}



