


# Import sample datasets to lissyuse() locally

import_sample_datasets_to_lissyuse <- function(data = NULL, lws = FALSE) {
  
  # All available datasets grouped by country
  all_datasets <- list(
    lis = list(
      it = list(it14 = it14_h_lis %>% left_join(it14_p_lis, by = lis_both_hp_variables),
                it16 = it16_h_lis %>% left_join(it16_p_lis, by = lis_both_hp_variables),
                it20 = it20_h_lis %>% left_join(it20_p_lis, by = lis_both_hp_variables)),
      mx = list(mx14 = mx14_h_lis %>% left_join(mx14_p_lis, by = lis_both_hp_variables),
                mx16 = mx16_h_lis %>% left_join(mx16_p_lis, by = lis_both_hp_variables),
                mx18 = mx18_h_lis %>% left_join(mx18_p_lis, by = lis_both_hp_variables)),
      us = list(us14 = us14_h_lis %>% left_join(us14_p_lis, by = lis_both_hp_variables),
                us16 = us16_h_lis %>% left_join(us16_p_lis, by = lis_both_hp_variables),
                us18 = us18_h_lis %>% left_join(us18_p_lis, by = lis_both_hp_variables))
    ),
    lws = list(
      it = list(it14 = it14_h_lws %>% left_join(it14_p_lws, by = lws_both_hp_variables),
                it16 = it16_h_lws %>% left_join(it16_p_lws, by = lws_both_hp_variables)),
      us = list(us16 = us16_h_lws %>% left_join(us16_p_lws, by = lws_both_hp_variables),
                us19 = us19_h_lws %>% left_join(us19_p_lws, by = lws_both_hp_variables))
    )
  )
  
  # Determine dataset type
  dataset_type <- if (lws) "lws" else "lis"
  datasets <- all_datasets[[dataset_type]]
  
  valid_values <- unique(c(
    names(datasets), # it, mx, us
    unlist(lapply(datasets, names)) # it14, it16, it20, mx14 etc
  ))
  
  # Check validity
  if (is.null(data)) {
    selected_keys <- valid_values
  } else if (any(!(data %in% valid_values))) {
    stop(sprintf(
      "Invalid value for 'data'. When lws = %s, valid values are: %s.",
      as.character(lws),
      paste(shQuote(valid_values), collapse = ", ")
    ))
  } else {
    selected_keys <- data
  }
  
  # Helper to resolve high-level country keys into full year-version keys
  resolve_keys <- function(keys) {
    expanded <- unlist(lapply(keys, function(k) {
      if (k %in% names(datasets)) {
        return(names(datasets[[k]]))  # e.g., "it" → c("it14", "it16")
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
  
  return(data_to_load)
}