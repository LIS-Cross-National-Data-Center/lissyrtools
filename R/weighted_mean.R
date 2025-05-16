#' Compute Weighted Mean Across a List of Data Frames (with optional grouping)
#'
#' @param data_list A named list of data frames, (e.g., across countries or years).
#' @param var_name A string specifying the variable name (e.g., "dhi", "pilabour") to compute the mean on.
#' @param wgt_name An optional string specifying the weight variable to be used. If `NULL`, equal weights are assumed.
#' @param na.rm Logical. If `TRUE`, missing values in `var_name` or `wgt_name` are removed.
#' @param by Optional string giving the name of a categorical variable to split the data within each data frame before computing the mean.
#'
#' @return A named list of data frames.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi", "age", "pi11", "region_c", "area_c", "educ", "emp"), from = 2016)
#' 
#' data %>% 
#' purrr::map(~ .x %>% filter(relation == 1000)) %>%
#' run_weighted_mean("dhi", "hpopwgt")
#' 
#' data %>% 
#' purrr::map(~ .x %>% filter(age > 25 & age <65)) %>%
#' run_weighted_mean("pi11", "ppopwgt", by = "educ")
#' 
#' }
run_weighted_mean <- function(
  data_list,
  var_name,
  wgt_name = NULL,
  na.rm = TRUE,
  by = NULL
) {
  data_list <- lissyrtools::remove_dname_with_missings_in_weights(
    data_list,
    wgt_name
  )
  

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
  
  

  if (!is.null(by)) {
    allowed_categoricals <- c(
      lissyrtools::lis_categorical_variables,
      lissyrtools::lws_wealth_categorical_variables,
      "inum"
    )
    if (!by %in% allowed_categoricals) {
      stop(sprintf(
        "The `by` variable must be a categorical variable in `lissyrtools::lis_categorical_variables`, `lissyrtools::lws_wealth_categorical_variables`, or the variable 'inum'."
      ))
    }

    
    df_to_keep <- purrr::map_lgl(data_list, ~ !all(is.na(.x[[by]])))
    to_drop <- names(df_to_keep[!df_to_keep])
    if (any(!df_to_keep)) {
      warning(sprintf(
        "The `by` variable '%s' contains only NA values in the following data frames, which will be dropped: %s",
        by,
        paste(to_drop, collapse = ", ")
      ))
      data_list <- data_list[df_to_keep]
    }
  }

  output_run_weighted_mean <- purrr::imap(
    data_list,
    ~ {
      d <- .x

      if (!is.null(by)) {
        d[[by]] <- as.character(haven::as_factor(d[[by]]))
        split_d <- split(d, d[[by]])

        purrr::imap(
          split_d,
          ~ {
            var <- .x[[var_name]]
            wgt <- if (!is.null(wgt_name)) .x[[wgt_name]] else NULL
            lissyrtools::compute_weighted_mean(
              var = var,
              wgt = wgt,
              na.rm = na.rm
            )
          }
        ) %>%
          unlist()
      } else {
        var <- d[[var_name]]
        wgt <- if (!is.null(wgt_name)) d[[wgt_name]] else NULL
        lissyrtools::compute_weighted_mean(
          var = var,
          wgt = wgt,
          na.rm = na.rm
        )
      }
    }
  )

  if (!is.null(by)) {
    return(output_run_weighted_mean)
  } else {
    output_run_weighted_mean <- lissyrtools::convert_list_from_ccyy_to_cc_names_yyyy(
      output_run_weighted_mean
    )
    return(output_run_weighted_mean)
  }
}

#' Compute (Weighted) Means for a Variable 
#' 
#'
#'
#' @param var A column refering to one of the variables in a LIS or LWS data frame.
#' @param wgt A numeric vector of weights (e.g., .x$hpopwgt, .x$pwgt). Must be the same length as \code{x}.
#' @param na.rm Logical; if \code{TRUE}, missing values in \code{x} and \code{w} are removed before computation. Default is \code{TRUE}.

#' @return A numeric vector.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' data <- lissyrtools::lissyuse(data = "de20", vars = c("dhi", "age", "educ"))
#' compute_weighted_mean(data$de20$age, na.rm = TRUE)
#' compute_weighted_mean(data$de20$dhi, data$de20$hwgt)
#' }

compute_weighted_mean <- function(var, wgt = NULL, na.rm = TRUE) {
  if (!length(wgt)) return(mean(var, na.rm = na.rm))
  if (na.rm) {
    keep <- !is.na(var) & !is.na(wgt)
    var <- var[keep]
    wgt <- wgt[keep]
  }
  sum(wgt * var) / sum(wgt)
}



