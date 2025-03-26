


#' Print all the country code in LIS
#'
#' @returns A data frame.
#'
#' @examples
#' show_countries_lis()
show_countries_lis <- function() {

  output <- lissyrtools::datasets %>% 
    dplyr::filter(database == "LIS") %>% 
    dplyr::group_by(cname) %>% 
    dplyr::filter(year == max(year)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(cname, iso2) %>% 
    tibble::deframe()
  
  
  attributes(output)[1] <- NULL

  return(output)
}


#' Print all the country code in LWS
#'
#' @returns A data frame.
#'
#' @examples
#' show_countries_lws()
show_countries_lws <- function() {
  
  output <- lissyrtools::datasets %>% 
    dplyr::filter(database == "LWS") %>% 
    dplyr::group_by(cname) %>% 
    dplyr::filter(year == max(year)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(cname, iso2) %>% 
    tibble::deframe()
  
  attributes(output)[1] <- NULL
  
  return(output)
}




#' Print all the existing years in LIS for a given country.
#'
#' @param iso2 A character vector with valid iso2 codes of countries present in LIS.
#'
#' @returns A list, made of numeric vectors. Each elements corresponds to a country in LIS.
#'
#' @examples
#' get_years_lis("it")
#' get_years_lis(iso2 = c("de", "jp"))
get_years_lis <- function(iso2) {

  
  # ensure the validity of the iso2 codes
  
  valid_iso2 <- lissyrtools::show_countries_lis()
  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::show_countries_lis()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'iso2' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::show_countries_lis()."
      )
    )
  }
  
  # body of the function
  process_country <- function(i) {
    
    years_to_output <- lissyrtools::datasets %>% 
      dplyr::filter(database == "LIS" & iso2 == i) %>%
      dplyr::select(year) %>% 
      dplyr::arrange(year) %>% 
      dplyr::pull()
    
    attributes(years_to_output)[1] <- NULL
    
    return(years_to_output)
  }
  
  
  to_be_used_iso2 <- iso2[iso2 %in% valid_iso2]
  result_list <- purrr::map(to_be_used_iso2,process_country)
  names(result_list) <- to_be_used_iso2
  return(result_list)
}

  




#' Print all the existing years in LWS for a given country.
#'
#' @param iso2 A character vector with valid iso2 codes of countries present in LWS.
#'
#' @returns A list, made of numeric vectors. Each elements corresponds to a country in LWS.
#'
#' @examples
#' get_years_lws("it")
#' get_years_lws(iso2 = c("de", "jp"))
get_years_lws <- function(iso2) {
  
  # ensure the validity of the iso2 codes
  
  valid_iso2 <- lissyrtools::show_countries_lws()
  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]
  
  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::show_countries_lws()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'iso2' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::show_countries_lws()."
      )
    )
  }
  
  # body of the function
  process_country <- function(i) {
    
    years_to_output <- lissyrtools::datasets %>% 
      dplyr::filter(database == "LWS" & iso2 == i) %>% 
      dplyr::select(year) %>% 
      dplyr::arrange(year) %>% 
      dplyr::pull()
    
    attributes(years_to_output)[1] <- NULL
    
    return(years_to_output)
  }
  
  to_be_used_iso2 <- iso2[iso2 %in% valid_iso2]
  result_list <- purrr::map(to_be_used_iso2,process_country)
  names(result_list) <- to_be_used_iso2
  return(result_list)
}



