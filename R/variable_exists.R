

#' Inspect whether a given variable contains more than just non-missing or non-zero values for a selected group of countries in the LIS or LWS databases.
#'
#' @param variable A unit-length character vector containing specified LIS/LWS variables. 
#' @param iso2 A character vector with valid iso2 codes of countries present in LIS/LWS. 
#' @param lws  A logical value, that guides the tool to search in the LIS or LWS database. The argument is FALSE by default, taking LIS as the database to be investigated if nothing is specified.
#' @param share A logical value indicating whether to output the share of datasets, across the entire time series for each country, where a variable has more than just non-missing or non-zero values, instead of displaying its presence year by year.           
#'
#' @return A list made of character vectors. If share = TRUE, then a list with a numeric vector. 
#' @export
#'
#' @examples
#' variable_exists(variable = "area_c", iso2 = "br")
#' variable_exists(variable = "basb", iso2 = c("fr", "de", "us", "uk"), lws = TRUE)
#' variable_exists(variable = "basb", iso2 = c("fr", "de", "us", "uk"), lws = TRUE, share = TRUE)
variable_exists <- function(variable, iso2, lws = FALSE, share = FALSE) {
  
  # Ensure that argument 'variable' only accepts one character

  if (length(variable) > 1) {
    stop(
      glue::glue(
        "Argument `variable` accepts no more than one character."
      )
    )
  }

  # ensure the validity of the variable

  if (lws) {
    invalid_var <- variable[!variable %in% lissyrtools::lws_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue(
        "Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lws_variables'."
      ))
    }
  } else {
    invalid_var <- variable[!variable %in% lissyrtools::lis_variables]
    if (length(invalid_var) > 0) {
      stop(glue::glue(
        "Invalid variable: {paste(invalid_var)} not found in 'lissyrtools::lis_variables'."
      ))
    }
  }

  # ensure the validity of the iso2 codes

  valid_iso2 <- if (lws) {
    lissyrtools::get_countries_lws()
  } else {
    lissyrtools::get_countries_lis()
  }

  invalid_iso2 <- iso2[!iso2 %in% valid_iso2]

  if (length(invalid_iso2) == length(iso2)) {
    # If no valid iso2 codes, stop with an error
    stop(
      glue::glue(
        "None of the provided iso2 codes in argument 'iso2' are valid: {toString(iso2)}. ",
        "Valid codes are stored in lissyrtools::get_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  } else if (length(invalid_iso2) > 0) {
    # If some codes are invalid, issue a warning
    warning(
      glue::glue(
        "The argument 'iso2' contains invalid iso2 codes: {toString(invalid_iso2)}. ",
        "These iso2 codes are not in the valid list for the selected database. See: lissyrtools::get_countries_{ifelse(lws, 'lws', 'lis')}()."
      )
    )
  }

  # body of the function
  if (share == FALSE) {
    process_country <- function(i) {
      db <- if (lws) "LWS" else "LIS"
      get_years_function <- if (lws) {
        lissyrtools::get_years_lws
      } else {
        lissyrtools::get_years_lis
      }

      years <- get_years_function(i)[[1]]

      existing_var_years <- lissyrtools::missing_or_zero_vars_all %>%
        dplyr::filter(
          database == db &
            iso2 == i &
            variable == {{ variable }} &
            status == FALSE
        ) %>%
        dplyr::pull(year)

      year_status <- ifelse(years %in% existing_var_years, "Yes", "No")
      names(year_status) <- years
      return(year_status)
    }

    to_be_used_iso2 <- iso2[iso2 %in% valid_iso2]
    result <- purrr::map(to_be_used_iso2, process_country)
    names(result) <- to_be_used_iso2

    return(result)
  } else if (share == TRUE) {
    db <- if (lws) "LWS" else "LIS"
    to_be_used_iso2 <- iso2[iso2 %in% valid_iso2]

    share_to_output <- lissyrtools::missing_or_zero_vars_all %>%
      dplyr::filter(
        database == db,
        iso2 %in% to_be_used_iso2,
        variable == {{ variable }}
      ) %>%
      dplyr::group_by(iso2, {{ variable }}) %>%
      dplyr::summarise(
        share = 100 - (sum(status) / dplyr::n() * 100),
        .groups = "drop"
      ) %>%
      dplyr::select(iso2, share) %>%
      dplyr::mutate(share = round(share, digits = 1)) %>%
      tibble::deframe()

    result <- list()
    result[[1]] <- share_to_output
    names(result) <- paste0(
      "Share of years across the series in ",
      dplyr::if_else(lws, "LWS", "LIS"),
      " where: ",
      variable,
      " has values other than zeros and missings."
    )
    return(result)
  }
}