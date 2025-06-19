#' Adjust Monetary Variables for Inflation and PPP
#'
#' Applies price adjustments to a monetary variable in a list of LIS/LWS datasets using
#' LIS-provided deflators. Adjustments can be made for domestic inflation (CPI),
#' purchasing power parity (PPP), or both (`lisppp`).
#'
#' @param data_list A named list of data frames, from LIS or LWS microdata.
#' @param var_name A string. The name of the monetary variable to be adjusted.
#' @param database A string, either `"lis"` or `"lws"`, indicating the source database.
#' Required for determining how to handle the deflators and income reference years.
#' @param transformation A string specifying the type of adjustment:
#' \itemize{
#'   \item `"lisppp"` (default): Adjusts by CPI and by PPP.
#'   \item `"cpi"`: Adjusts only for domestic inflation (CPI).
#'   \item `"ppp"`: Adjusts only for purchasing power parity (PPP), for cross-country comparability.
#' }
#' 
#' @details
#' For LWS datasets and income variables, the function accounts for discrepancies between
#' survey years and income reference years. It merges the appropriate deflator tables before
#' applying the requested adjustment. 
#' 
#' \strong{Important:} When using `"ppp"` or `"lisppp"` transformations, the monetary values
#' are converted out of their original currency. These adjustments are intended to support cross-country comparability, but the result is
#' no longer expressed in national currency units.
#' 
#' @return A list of data frames, with the specified variable adjusted based on the chosen transformation. 
#' 
#' @export 
#' 
#' @examples
#' \dontrun{ 
#' library(lissyrtools)
#' library(dplyr)
#' 
#' # --- Example 1: CPI Adjustment (Domestic Inflation, Italy) ---
#' 
#' it_data <- lissyuse("it", vars = "dhi", from = 2010)
#' run_weighted_mean(it_data, var_name = "dhi")  # Nominal income
#'
#' it_data_cpi <- apply_ppp_adjustment(it_data, "dhi", database = "lis", transformation = "cpi")
#' run_weighted_mean(it_data_cpi, var_name = "dhi")  # Real income (CPI-adjusted, base year = 2017)
#'
#'
#' # --- Example 2: PPP Adjustment Across Countries (France, Poland, US, UK, Mexico in 2016) ---
#' multi_2016 <- lissyuse(c("fr16", "pl16", "us16", "uk16", "mx16"), vars = "dhi")
#' run_weighted_mean(multi_2016, var_name = "dhi")  # Nominal
#'
#' multi_2016_ppp <- apply_ppp_adjustment(multi_2016, "dhi", database = "lis", transformation = "ppp")
#' run_weighted_mean(multi_2016_ppp, var_name = "dhi")  # PPP-adjusted
#'
#'
#' # --- Example 3: LIS PPP (Across Time and Countries: Canada & Mexico, 2015–2020) ---
#' can_mex <- lissyuse(c("ca15", "ca18", "ca20", "mx16", "mx18", "mx20"), vars = "dhi")
#' run_weighted_mean(can_mex, var_name = "dhi")  # Nominal
#'
#' can_mex_lisppp <- apply_ppp_adjustment(can_mex, "dhi", database = "lis", transformation = "lisppp")
#' run_weighted_mean(can_mex_lisppp, var_name = "dhi")  # Fully deflated (real + PPP)
#'
#'
#' # --- Example 4: Reference Year Differences in Same Survey Year (Germany 2017, LIS vs LWS) ---
#' lis_de17 <- lissyuse("de17", vars = "dhi")
#' lws_de17 <- lissyuse("de17", vars = c("dhi", "dnw"), lws = TRUE)
#'
#' apply_ppp_adjustment(lis_de17, "dhi", database = "lis", transformation = "lisppp")[[1]] %>% dplyr::summarise(mean_cpi_lis = mean(cpi)) 
#' apply_ppp_adjustment(lws_de17, "dhi", database = "lws", transformation = "lisppp")[[1]] %>% dplyr::summarise(mean_cpi_lis = mean(cpi))
#'
#' # Even with the same survey year (DE17), different income reference years are accounted for automatically.
#' } 
apply_ppp_adjustment <- function(
  data_list,
  var_name,
  database,
  transformation = "lisppp"
) {
  
  
  if (missing(database) || is.null(database) || database == "") {
    warning(
      "The `database` argument was not specified. Please define it as either \"lis\" or \"lws\" (no default is assumed)."
    )
  }

  # --- Validate arguments ---

  # Check if variable exists in datasets
  assertthat::assert_that(
    var_name %in% names(data_list[[1]]),
    msg = glue::glue("Variable '{var_name}' could not be found in the dataset.")
  )

  # Check `database` argument
  assertthat::assert_that(
    database %in% c("lis", "lws"),
    msg = glue::glue("Argument '{database}' must be either \"lis\" or \"lws\".")
  )

  # Check `trasnformation` argument
  assertthat::assert_that(
    transformation %in% c("lisppp", "cpi", "ppp"),
    msg = glue::glue(
      "Argument '{transformation}' must be one of \"lisppp\" (default), \"cpi\", or \"ppp\"."
    )
  )

  # --- Merge deflators data ---

  result_merged <- if (var_name %in% lissyrtools::lis_income_variables & database == "lws") {
    message(glue::glue(
      "Income variable: '{var_name}'. For some LWS datasets, the reference year differs from the survey year. ",
      "Please check on METIS > Results > Dataset Information > Data Source > Income. ",
      "Adjustment has been automatically made."
    ))

    purrr::map(
      data_list,
      ~ .x %>%
        dplyr::left_join(lissyrtools::data_inc_ref_year, by = "dname") %>%
        dplyr::left_join(
          lissyrtools::deflators %>%
            dplyr::mutate(income_reference_year = year) %>%
            dplyr::select(-year, -cname, -iso3),
          by = c("iso2", "income_reference_year")
        )
    )
  } else {
    # lis or lws but with some wealth variable or whatever as long as it is not income.
    purrr::map(
      data_list,
      ~ .x %>%
        dplyr::left_join(
          lissyrtools::deflators %>% dplyr::select(-cname, -iso3),
          by = c("iso2", "year")
        )
    )
  }

  # ---  mismatches betweewn missing deflators data for 1) Taiwan and 2) the most recent year/s in some countries ---

  dname_dts <- stringr::str_sub(names(data_list), 1, 4)
  dname_dflt <- lissyrtools::deflators %>%
    dplyr::mutate(
      dname = stringr::str_c(iso2, stringr::str_sub(year, 3, 4))
    ) %>%
    dplyr::select(dname) %>%
    unique() %>%
    dplyr::pull()

  missing_dnames <- setdiff(dname_dts, dname_dflt)

  if (length(missing_dnames) > 0) {
    warning(glue::glue(
      "Deflator data (CPI/PPP) is missing for the following datasets: {paste(missing_dnames, collapse = ', ')}.\n",
      "For these datasets, transformed values may be incorrect or NA."
    ))
  }

  # --- One-time message on chosen transformation ---

  if (transformation == "lisppp") {
    message(glue::glue(
      "Variable '{var_name}' was adjusted using LIS-specific deflators: ",
      "both for domestic inflation (CPI, base year 2017 = 100), ",
      "and for cross-country comparability using purchasing power parity (PPP) conversion factors, ",
      "with 2017 USD as the base (USA = 1)."
    ))
  } else if (transformation == "cpi") {
    message(glue::glue(
      "Variable '{var_name}' was adjusted for domestic inflation using the CPI (base year 2017 = 100)."
    ))
  } else if (transformation == "ppp") {
    message(glue::glue(
      "Variable '{var_name}' was adjusted for cross-country comparability using 2017 USD PPP conversion factors (USA = 1)."
    ))
  }

  # --- Perform adjustment ---

  result_adj <- purrr::map(result_merged, function(df) {
    if (transformation == "lisppp") {
      df[[var_name]] <- df[[var_name]] / df[["lisppp"]]
    } else if (transformation == "cpi") {
      df[[var_name]] <- df[[var_name]] / df[["cpi"]] * 100
    } else if (transformation == "ppp") {
      df[[var_name]] <- df[[var_name]] / df[["ppp"]]
    }

    # could be "lisppp" "cpi" "ppp"
    return(df)
  })

  return(result_adj)
}