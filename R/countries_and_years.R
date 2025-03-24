


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
#' @param iso2 A string with 2 characters, specifically an iso2 code present in show_countries_lis().
#'
#' @returns A numeric vector.
#'
#' @examples
#' show_country_years_lis("it")
#' show_country_years_lis("us")
show_country_years_lis <- function(iso2 = NULL) {

  
  if (!is.null(iso2)) {
    
    assertthat::assert_that(
      length(iso2) ==  1,
      msg = glue::glue("This function only accepts 1 iso2 code at the time.")
    )
    
    assertthat::assert_that(
      iso2 %in% unique(show_countries_lis()),
      msg = glue::glue("The character '{iso2}' could not be found in `show_countries_lis()`.")
    )
    
    
    output <- lissyrtools::datasets %>% 
      dplyr::filter(database == "LIS" & iso2 == !!iso2) %>%  
      dplyr::select(year) %>% 
      unique() %>% 
      dplyr::pull() %>% 
      sort()
    
  } else {
    output <- NULL  
  }
  
  return(output)  
} 
  




#' Print all the existing years in LWS for a given country.
#'
#' @param iso2 A string with 2 characters, specifically an iso2 code present in show_countries_lws()
#'
#' @returns A numeric vector.
#'
#' @examples
#' show_country_years_lis("it")
#' show_country_years_lis("us")
show_country_years_lws <- function(iso2 = NULL) {
  
  
  if (!is.null(iso2)) {
    
    assertthat::assert_that(
      length(iso2) ==  1,
      msg = glue::glue("This function only accepts 1 iso2 code at the time.")
    )
    
    assertthat::assert_that(
      iso2 %in% unique(show_countries_lws()),
      msg = glue::glue("The character '{iso2}' could not be found in `show_countries_lws()`.")
    )
    
    
    output <- lissyrtools::datasets %>% 
      dplyr::filter(database == "LWS" & iso2 == !!iso2) %>%  
      dplyr::select(year) %>% 
      unique() %>% 
      dplyr::pull() %>% 
      sort()
    
  } else {
    output <- NULL  
  }
  
  return(output)  
} 


