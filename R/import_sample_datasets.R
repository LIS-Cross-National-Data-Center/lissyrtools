


# Import sample datasets to lissyuse() locally

import_sample_datasets_to_lissyuse <- function(data = NULL, lws = FALSE) {

  # argument {data} can only take the sample datasets
  
if (lws) valid_values <- c("us", "it", "us16", "it14") else valid_values <- c("us", "it", "mx", "us16", "it14", "mx18")
  
if (any(!(data %in% valid_values))) {
    stop(sprintf(
      "Invalid value for 'data'. When lws = %s, only the sample datasets stored in this package can be loaded in the following format: %s.", # Refer to the vignette or self-teaching materials for details."
      as.character(lws),  
      paste(shQuote(valid_values), collapse = ", ")  
    ))
}  
  
  # create list with the sample datasets selected, depending on LWS TRUE or FALSE
  # later on columns can be removed based on argument {vars}, and the same goes for the rows based on argument {subset} 
 
if (lws) {
  
  embedded_sample_data <- list(
    it = italy_14_lws_h %>% left_join(italy_14_lws_p , by = lws_both_hp_variables),
    us = united_states_16_lws_h %>% left_join(united_states_16_lws_p , by = lws_both_hp_variables)
  )
  
  if (is.null(data)) {
    
    data_to_load <- embedded_sample_data
    names(data_to_load) <-  paste0(names(data_to_load), sapply(data_to_load, function(x) { stringr::str_sub(unique(x$year),3,4) }))
    
  } else {
    
    data_to_load <- embedded_sample_data[stringr::str_sub(data,1,2)]
    names(data_to_load) <-  paste0(names(data_to_load), sapply(data_to_load, function(x) { stringr::stringr::str_sub(unique(x$year),3,4) }))
  }
  
} 


else {  
  
  embedded_sample_data <- list(
    it = italy_14_lis_h %>% left_join(italy_14_lis_p , by = lis_both_hp_variables),
    us = united_states_16_lis_h %>% left_join(united_states_16_lis_p , by = lis_both_hp_variables),
    mx = mexico_18_lis_h %>% left_join(mexico_18_lis_p, by = lis_both_hp_variables)
  )
  
  
  if (is.null(data)) {
    
    data_to_load <- embedded_sample_data
    names(data_to_load) <-  paste0(names(data_to_load), sapply(data_to_load, function(x) { stringr::str_sub(unique(x$year),3,4) }))
    
  } else {
  
  
  data_to_load <- embedded_sample_data[stringr::str_sub(data,1,2)]
  names(data_to_load) <-  paste0(names(data_to_load), sapply(data_to_load, function(x) { stringr::str_sub(unique(x$year),3,4) }))
  
  }

}


return(data_to_load)
}


