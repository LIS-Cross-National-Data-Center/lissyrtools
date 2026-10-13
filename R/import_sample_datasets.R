

#' Import Sample Datasets to lissyuse
#'
#' @description
#' Internal helper to import sample datasets into the `lissyuse` environment.
#'
#' @param data Optional. A named list of datasets to import. If `NULL`, all the default set is used.
#' @param database A character value. One of "lis" (default), "lws", or "lcs".
#' @param from A numeric value representing the year (inclusive) after which the LIS/LWS datasets should be loaded.
#' @param to A numeric value representing the year (inclusive) up to which the LIS/LWS datasets should be loaded.
#'
#' @keywords internal
#' @return A list.
import_sample_datasets_to_lissyuse <- function(data = NULL, database = "lis") {
  
  assertthat::assert_that(database %in% c("lis", "lws", "lcs"),
                          msg = glue::glue("'database' must be one of 'lis', 'lws', or 'lcs'. Got '{database}' instead."))
  
  # All available datasets grouped by country
  all_datasets <- list(
    lis = list(
      it = list(
        it14 = it14_h_lis %>%
          dplyr::left_join(it14_p_lis, by = lissyrtools::lis_both_hp_variables),
        it16 = it16_h_lis %>%
          dplyr::left_join(it16_p_lis, by = lissyrtools::lis_both_hp_variables),
        it20 = it20_h_lis %>%
          dplyr::left_join(it20_p_lis, by = lissyrtools::lis_both_hp_variables)
      ),
      mx = list(
        mx14 = mx14_h_lis %>%
          dplyr::left_join(mx14_p_lis, by = lissyrtools::lis_both_hp_variables),
        mx16 = mx16_h_lis %>%
          dplyr::left_join(mx16_p_lis, by = lissyrtools::lis_both_hp_variables),
        mx18 = mx18_h_lis %>%
          dplyr::left_join(mx18_p_lis, by = lissyrtools::lis_both_hp_variables)
      ),
      us = list(
        us14 = us14_h_lis %>%
          dplyr::left_join(us14_p_lis, by = lissyrtools::lis_both_hp_variables),
        us16 = us16_h_lis %>%
          dplyr::left_join(us16_p_lis, by = lissyrtools::lis_both_hp_variables),
        us18 = us18_h_lis %>%
          dplyr::left_join(us18_p_lis, by = lissyrtools::lis_both_hp_variables)
      )
    ),
    lws = list(
      it = list(
        it14 = it14_h_lws %>%
          dplyr::left_join(it14_p_lws, by = lissyrtools::lws_both_hp_variables),
        it16 = it16_h_lws %>%
          dplyr::left_join(it16_p_lws, by = lissyrtools::lws_both_hp_variables)
      ),
      us = list(
        us16 = us16_h_lws %>%
          dplyr::left_join(us16_p_lws, by = lissyrtools::lws_both_hp_variables),
        us19 = us19_h_lws %>%
          dplyr::left_join(us19_p_lws, by = lissyrtools::lws_both_hp_variables)
      )
    ),
    lcs = list(
      es = list(
        es16 = es16_h_lcs %>%
          dplyr::left_join(es16_p_lcs, by = lissyrtools::lcs_both_hp_variables),
        es24 = es24_h_lcs %>%
          dplyr::left_join(es24_p_lcs, by = lissyrtools::lcs_both_hp_variables)
      ),
      mx = list(
        mx16 = mx16_h_lcs %>%
          dplyr::left_join(mx16_p_lcs, by = lissyrtools::lcs_both_hp_variables),
        mx20 = mx20_h_lcs %>%
          dplyr::left_join(mx20_p_lcs, by = lissyrtools::lcs_both_hp_variables)
      )
    )
  )
  
  # Determine dataset type
  datasets <- all_datasets[[database]]
  valid_values <- unique(c(
    names(datasets), # it, mx, us
    unlist(lapply(datasets, names)) # it14, it16, it20, mx14 etc
  ))
  
  # Check validity
  if (is.null(data)) {
    selected_keys <- valid_values
  } else if (any(!(data %in% valid_values))) {
    stop(sprintf(
      "Invalid value for 'data'. When database = '%s', valid values are: %s.",
      database,
      paste(shQuote(valid_values), collapse = ", ")
    ))
  } else {
    selected_keys <- data
  }
  
  # Helper to resolve high-level country keys into full year-version keys
  resolve_keys <- function(keys) {
    expanded <- unlist(lapply(keys, function(k) {
      if (k %in% names(datasets)) {
        return(names(datasets[[k]])) # e.g., "it" → c("it14", "it16")
      } else {
        return(k)
      }
    }))
    unique(expanded)
  }
  
  resolved_keys <- resolve_keys(selected_keys)
  
  # Now extract the datasets and name them properly
  data_to_load <- list()
  for (k in resolved_keys) {
    for (country in names(datasets)) {
      if (k %in% names(datasets[[country]])) {
        data_to_load[[k]] <- datasets[[country]][[k]]
      }
    }
  }
  
  # Filter by year range (dataset names are ccyy, e.g. "it14" -> 2014)
  if (!is.null(from) || !is.null(to)) {
    yy   <- as.numeric(substr(names(data_to_load), 3, 4))
    year <- ifelse(yy > 50, 1900 + yy, 2000 + yy)   # same convention as ccyy
    keep <- rep(TRUE, length(year))
    if (!is.null(from)) keep <- keep & year >= from
    if (!is.null(to))   keep <- keep & year <= to
    data_to_load <- data_to_load[keep]
  }
  
  if (length(data_to_load) == 0) {
    stop("No sample datasets matched the provided criteria. Please check 'data', 'from' and 'to'.",
         call. = FALSE)
  }
  
  return(data_to_load)
}