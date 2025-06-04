


# Import sample datasets to lissyuse() locally

import_sample_datasets_to_lissyuse <- function(data = NULL, lws = FALSE) {
  
  # Define valid values (group and individual dataset codes)
  if (lws) {
    valid_values <- c("it", "us", "it14", "it16", "us16", "us19")
  } else {
    valid_values <- c("it", "us", "mx",
                      "it14", "it16", "it20",
                      "mx14", "mx16", "mx18",
                      "us14", "us16", "us18")
  }
  
  # Validate input
  if (!is.null(data) && any(!(data %in% valid_values))) {
    stop(sprintf(
      "Invalid value for 'data'. When lws = %s, only the sample datasets stored in this package can be loaded in the following format: %s.",
      as.character(lws),
      paste(shQuote(valid_values), collapse = ", ")
    ))
  }
  
  # Define group-to-dataset mappings
  data_groups <- list(
    it = c("it14", "it16", "it20"),
    us = c("us14", "us16", "us18", "us19"),
    mx = c("mx14", "mx16", "mx18")
  )
  
  # Define LIS datasets
  lis_data <- list(
    it14 = it14_h_lis %>% left_join(it14_p_lis, by = lis_both_hp_variables),
    it16 = it16_h_lis %>% left_join(it16_p_lis, by = lis_both_hp_variables),
    it20 = it20_h_lis %>% left_join(it20_p_lis, by = lis_both_hp_variables),
    mx14 = mx14_h_lis %>% left_join(mx14_p_lis, by = lis_both_hp_variables),
    mx16 = mx16_h_lis %>% left_join(mx16_p_lis, by = lis_both_hp_variables),
    mx18 = mx18_h_lis %>% left_join(mx18_p_lis, by = lis_both_hp_variables),
    us14 = us14_h_lis %>% left_join(us14_p_lis, by = lis_both_hp_variables),
    us16 = us16_h_lis %>% left_join(us16_p_lis, by = lis_both_hp_variables),
    us18 = us18_h_lis %>% left_join(us18_p_lis, by = lis_both_hp_variables)
  )
  
  # Define LWS datasets
  lws_data <- list(
    it14 = it14_h_lws %>% left_join(it14_p_lws, by = lws_both_hp_variables),
    it16 = it16_h_lws %>% left_join(it16_p_lws, by = lws_both_hp_variables),
    us16 = us16_h_lws %>% left_join(us16_p_lws, by = lws_both_hp_variables),
    us19 = us19_h_lws %>% left_join(us19_p_lws, by = lws_both_hp_variables)
  )
  
  # Pick correct dataset pool
  data_pool <- if (lws) lws_data else lis_data
  
  # If no data is specified, load everything
  if (is.null(data)) {
    return(data_pool)
  }
  
  # Expand group keys to specific dataset keys
  expanded_keys <- unlist(lapply(data, function(d) {
    if (d %in% names(data_groups)) {
      data_groups[[d]]
    } else {
      d
    }
  }))
  
  # Validate expanded keys
  if (any(!expanded_keys %in% names(data_pool))) {
    stop("One or more requested datasets are not available.")
  }
  
  # Return the selected datasets
  data_to_load <- data_pool[expanded_keys]
  return(data_to_load)
}

