
 
#' Load data easily and efficiently with lissyuse
#' 
#' @description
#' `lissyuse()` enables the user to specify which variables to import, along with a set of default variables (IDs, weights, currency, year, relation, etc.). If both household-level and person-level variables are specified, lissyuse() will automatically merge the two types of files. For faster and more efficient processing, we strongly recommend selecting of a restricted set of variables in `vars` argument. Additionally, the function includes a subset argument that allows users to limit the data to a specific subgroup.
#'
#' @param data A character vector containing ISO2 country codes, and/or the country-year specific datasets in its `ccyy` format.
#' @param vars A character vector specifying the LIS/LWS variables to be loaded. 
#' @param subset A logical expression defining the criteria for subsetting the data. Observations for which the expression evaluates to TRUE are included in the subset.
#' @param from A numeric value representing the year (inclusive) after which the LIS/LWS datasets should be loaded.
#' @param to 	A numeric value representing the year (inclusive) up to which the LIS/LWS datasets should be loaded.
#' @param lws A logical value indicating whether to load LWS data. If TRUE, LWS data is loaded; otherwise (default: FALSE), LIS data is loaded instead. Note that this does not eliminate the need to set the ‘Project’ field accordingly in the LISSY remote system.
#'
#' @return A list whose elements will be a data frame named after their respective dataset. See the naming formats in the examples below. Each data frame will contain as many columns as the selected variables, plus the default technical ones.
#' @export
#'
#' @examples
#' \dontrun{
#' library(lissyrtools)
#' 
#'lis_datasets <- lissyuse(data = c("it", "de16", "us19"), vars  = c("dhi", "region_c", "age", "hourstot", "status1"), subset = "!is.na(status1) & relation %in% c(1000,2000)")
#' # Checking the names of the data frames. 
#'names(lis_datasets)
#'
#' # Selecting certain elemennts of the list 
#'lis_datasets[["it14"]]   # By their name
#'lis_datasets[1:3]        # By their respective order within lis_datasets
#'
#' # Selecting all the italian datasets, while restrict them to a certain year range. 
#'lis_datasets <- lissyuse( data = c("it"), vars  = c("dhi", "region_c"), from = 2004, to = 2016)
#'
#'# In the previous line only household-level variables were selected. 
#'# this will lead to slightly different names for the data frames. 
#'names(lis_datasets)
#'
#'# The same occurs when only person-level variables were selected 
#'lis_datasets <- lissyuse(data = c("it"), vars  = c("age", "sex"), from = 2004, to = 2016)
#'
#'names(lis_datasets)
#'
#'
#' # ------------ LWS ------------------ 
#'
#'lws_datasets <- lissyuse(data = c("us", "uk17", "uk19"), vars = "dnw", from = 2015, to = 2021, lws = TRUE)
#'
#'names(lws_datasets)
#'}
lissyuse <- function(
  data = NULL,
  vars = NULL,
  subset = NULL,
  from = NULL,
  to = NULL,
  lws = FALSE
) {
  # 0) Define paths and location  ---------------------------------------------------------

  if (!exists("define_path")) {
    data_to_load <- import_sample_datasets_to_lissyuse(data, lws) # local machine ----> only access to sample datasets
  } else {
    path_to_files <- define_path(lws)[[1]]
    location <- define_path(lws)[[2]]

    # 1) Argument {data}  -------------------------------------

    check_empty_data(data, lws)

    check_length_iso2(data)

    check_iso2(data, lws)

    invalid_ccyy_pairs(data, lws)

    # Define data to be loaded -----------------------------------

    data_to_load <- load_datasets(data, lws, from, to)
  }

  # 2)  Variable-driven selection of files bases on argument {vars}  -------------------------------------

  check_invalid_vars(vars, lws)

  # Main function to select columns based on argument {vars}
  intermediate_data_and_message <- variable_selection_for_lissyuse(
    data_to_load,
    path_to_files,
    vars,
    lws
  )

  # allocation of its outputs
  intermediate_data_to_filter <- intermediate_data_and_message$data
  message <- intermediate_data_and_message$message

  # 3)  Filtering of rows based on the condition imposed in argument {subset}  -------------------------------------

  datasets_final <- if (!is.null(subset)) {
    subset_expr <- rlang::parse_expr(subset)
    subset_datasets(intermediate_data_to_filter, lws, subset_expr)
  } else {
    intermediate_data_to_filter
  }

  # 4)  LISSY adjustment  -------------------------------------

  if (exists("define_path")) {
    if (location == "L") {
      datasets_final <- lapply(datasets_final, as.data.frame)
    }
  }

  # 5) Attributes -------------------------

  # To delete in the future perhaps
  # Needed to accomodate the earliest developments in lissyrtools package

  if (exists("relation", datasets_final[[1]])) {
    attr(datasets_final, "level") <- "p"
    attr(datasets_final, "merged_levels") <- TRUE
  } else {
    attr(datasets_final, "level") <- "h"
    attr(datasets_final, "merged_levels") <- FALSE
  }

  if (exists("inum", datasets_final[[1]])) {
    attr(datasets_final, "database") <- "lws"
  } else {
    attr(datasets_final, "database") <- "lis"
  }

  # 6) Print message on the availability of the list with the datasets and its names ----------
  cat(message, "\n")

  # 7) Return (instead of assigning it to the Global environment as in the past)
  return(datasets_final)
}



#' Load Dataset Names Based on Criteria
#'
#' @description
#' Internal function to filter and return dataset names based on input criteria such as
#' specified iso2 codes or ccyy pairs, database type (LWS or LIS), and optional year range.
#'
#' @param data Optional character vector specifying ccyy pairs or iso2 codes.
#' @param lws Logical indicating whether to use the LWS or LIS database.
#' @param from Optional numeric lower bound for dataset years.
#' @param to Optional numeric upper bound for dataset years.
#'
#' @return A character vector of dataset names matching the criteria.
#'
#' @keywords internal
load_datasets <- function(data = NULL, lws = FALSE, from = NULL, to = NULL) {
  # Step 1: Split the `data` into series for country and dataset pairs
  entire_series_for_a_country <- data[stringr::str_length(data) == 2]
  ccyy_datasets <- data[stringr::str_length(data) == 4]

  # Step 2: Extract dnames for entire series based on LWS or LIS
  entire_series_extract_dname <- lissyrtools::datasets %>%
    dplyr::filter(
      if (lws) {
        database == "LWS" & iso2 %in% entire_series_for_a_country
      } else {
        database == "LIS" & iso2 %in% entire_series_for_a_country
      }
    ) %>%
    dplyr::select(dname) %>%
    unique() %>%
    dplyr::pull()

  # Combine datasets for the entire series and cc/yy datasets
  all_dnames <- c(entire_series_extract_dname, ccyy_datasets)

  # Step 3: Filter datasets based on the combined list and the year range
  data_to_load <- lissyrtools::datasets %>%
    dplyr::filter(
      if (lws) {
        database == "LWS"
      } else {
        database == "LIS"
      }
    ) %>%
    dplyr::filter(
      if (!is.null(data)) {
        dname %in% all_dnames
      } else {
        TRUE
      }
    ) %>%
    dplyr::filter(
      if (!is.null(from) & !is.null(to)) {
        year >= from & year <= to
      } else if (!is.null(from)) {
        year >= from
      } else if (!is.null(to)) {
        year <= to
      } else {
        year > 0
      }
    ) %>%
    dplyr::select(dname) %>%
    unique() %>%
    dplyr::pull()

  # Clean attributes of the final result
  attributes(data_to_load) <- NULL

  # Step 4: Check if the result is empty and stop if no data is found
  # (probably already caught on previous checks)
  if (length(data_to_load) == 0) {
    stop(
      glue::glue(
        "No datasets matched the provided criteria. Please check the arguments provided, especially 'data'."
      )
    )
  }

  return(data_to_load)
}







#' Load the datasets for lissyuse, and automatically merge h and p level files based on the variables. 
#'
#' @description
#' Internal function to load datasets from specified files.
#' Depending on the variable criteria, it loads household-level, person-level,
#' or both datasets. If both are loaded, it merges them; otherwise, it returns
#' the single dataset. The merge stage may be skipped if only one dataset type is required. 
#'
#' @param data_to_load Character vector of dataset names to load.
#' @param path_to_files File path where datasets are stored.
#' @param vars Optional character vector of variable names to select. If `NULL`, all variables are considered.
#' @param lws Logical.
#'
#' @return A list containing the data frames selected.
#'
#' @keywords internal
variable_selection_for_lissyuse <- function(
  data_to_load,
  path_to_files,
  vars = NULL,
  lws = FALSE
) {
  
  if (exists("define_path")) {
    if (lws) {
      # 1.1 LWS #

      # 1.1.1 Select all variables, if none is specified # ---------------------------
      if (is.null(vars)) {
        files_h <- purrr::map(
          data_to_load,
          ~ haven::read_dta(file = paste0(path_to_files, .x, "wh", ".dta"))
        )

        files_p <- purrr::map(
          data_to_load,
          ~ haven::read_dta(file = paste0(path_to_files, .x, "wp", ".dta"))
        )

        list_with_data <- purrr::map2(
          files_h,
          files_p,
          ~ dplyr::inner_join(.x, .y, c("hid", "inum")) %>%
            dplyr::select(-dplyr::ends_with(".y")) %>%
            dplyr::rename_with(~ sub("\\.x$", "", .), dplyr::ends_with(".x"))
        )
        names(list_with_data) <- data_to_load

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n", # Proper period and paragraph space
          "All variables were imported! We recommend specifying a character vector with the desired variables in the argument `vars`."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lws_household_variables, lissyrtools::lws_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lws_person_variables, lissyrtools::lws_household_variables)
          ) ==
            0
      ) {
        # 1.1.2 Only household-level variables were selected: load only h-level file # -----------------------
        vars <- vars[vars %in% lissyrtools::lws_household_variables] # remove any invalid variables

        list_with_data <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "wh", ".dta"),
            col_select = unique(c(vars, lissyrtools::key_vars_household_lws))
          )
        )
        names(list_with_data) <- paste0(data_to_load, "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `household-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lws_household_variables, lissyrtools::lws_person_variables)) ==
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lws_person_variables, lissyrtools::lws_household_variables)
          ) >
            0
      ) {
        # 1.1.3 Only person-level variables were selected: load only p-level file # -----------------------------------
        vars <- vars[vars %in% lissyrtools::lws_person_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "wp", ".dta"),
            col_select = unique(c(vars, lissyrtools::key_vars_person_lws))
          )
        )
        names(list_with_data) <- paste0(data_to_load, "p")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lws_household_variables, lissyrtools::lws_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lws_person_variables, lissyrtools::lws_household_variables)
          ) >
            0
      ) {
        # 1.1.4 Both household and person level varaibles were selected: load both filed and merge them # --------------------
        hvars <- vars[vars %in% lissyrtools::lws_household_variables]
        pvars <- vars[vars %in% lissyrtools::lws_person_variables]

        files_h <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "wh", ".dta"),
            col_select = unique(c(hvars, lissyrtools::key_vars_household_lws))
          )
        )

        files_p <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "wp", ".dta"),
            col_select = unique(c(pvars, lissyrtools::key_vars_person_lws))
          )
        )

        list_with_data <- purrr::map2(
          files_h,
          files_p,
          ~ dplyr::inner_join(.x, .y, by = c("hid", "inum")) %>%
            dplyr::select(-edplyr::nds_with(".y")) %>%
            dplyr::rename_with(~ sub("\\.x$", "", .), dplyr::ends_with(".x"))
        )

        names(list_with_data) <- data_to_load

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (sum(vars %in% lissyrtools::lws_variables) == 0) {
        # 1.1.5 No valid lws variables supplied # ----------------

        stop("No valid LWS variable names specified.") # already ensured by check_invalid_vars(vars, lws)
      } else if (sum(vars %in% lissyrtools::lws_variables) > 0) {
        # 1.1.6 # Mix between invalid variables and lws_both_hp_variables # --------------------------------

        vars <- vars[vars %in% lissyrtools::lws_variables]
        assertthat::assert_that(all(vars %in% lissyrtools::lws_both_hp_variables))

        list_with_data <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "wh", ".dta"),
            col_select = unique(c(vars, lissyrtools::key_vars_household_lws))
          )
        )
        names(list_with_data) <- paste0(data_to_load, "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n",
          "NOTE: The selected variables are not exclusive to either household or individual-level datasets. The imported datasets have been defaulted to `household-level`."
        )
      }

      return(list(data = list_with_data, message = message_to_print_in_the_end))
    } else {
      # LIS #

      # 1.2.1 Select all variables, if none is specified # ------------------------------------
      if (is.null(vars)) {
        files_h <- purrr::map(
          data_to_load,
          ~ haven::read_dta(file = paste0(path_to_files, .x, "ih", ".dta"))
        )

        files_p <- purrr::map(
          data_to_load,
          ~ haven::read_dta(file = paste0(path_to_files, .x, "ip", ".dta"))
        )

        list_with_data <- purrr::map2(
          files_h,
          files_p,
          ~ dplyr::inner_join(.x, .y, by = c("hid")) %>%
            dplyr::select(-dplyr::ends_with(".y")) %>%
            dplyr::rename_with(~ sub("\\.x$", "", .), dplyr::ends_with(".x"))
        )
        names(list_with_data) <- data_to_load

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n", # Proper period and paragraph space
          "All variables were imported! We recommend specifying a character vector with the desired variables in the argument `vars`."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lis_household_variables, lissyrtools::lis_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lis_person_variables, lissyrtools::lis_household_variables)
          ) ==
            0
      ) {
        # 1.2.2 Only household-level variables were selected: load only h-level file # --------------------
        vars <- vars[vars %in% lissyrtools::lis_household_variables] # remove any invalid variables

        list_with_data <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "ih", ".dta"),
            col_select = unique(c(vars, lissyrtools::key_vars_household_lis))
          )
        )
        names(list_with_data) <- paste0(data_to_load, "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `household-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lis_household_variables, lissyrtools::lis_person_variables)) ==
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lis_person_variables, lissyrtools::lis_household_variables)
          ) >
            0
      ) {
        # 1.2.3 Only person-level variables were selected: load only p-level file #    ------------------------
        vars <- vars[vars %in% lissyrtools::lis_person_variables] # remove any invalid variables

        list_with_data <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "ip", ".dta"),
            col_select = unique(c(vars, lissyrtools::key_vars_person_lis))
          )
        )
        names(list_with_data) <- paste0(data_to_load, "p")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lis_household_variables, lissyrtools::lis_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lis_person_variables, lissyrtools::lis_household_variables)
          ) >
            0
      ) {
        # 1.2.4 Both household and person level varaibles were selected: load both filed and merge them # --------------------
        hvars <- vars[vars %in% lissyrtools::lis_household_variables] # remove any invalid variables
        pvars <- vars[vars %in% lissyrtools::lis_person_variables] # remove any invalid variables

        files_h <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "ih", ".dta"),
            col_select = unique(c(hvars, lissyrtools::key_vars_household_lis))
          )
        )

        files_p <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "ip", ".dta"),
            col_select = unique(c(pvars, lissyrtools::key_vars_person_lis))
          )
        )

        list_with_data <- purrr::map2(
          files_h,
          files_p,
          ~ dplyr::inner_join(.x, .y, by = c("hid")) %>%
            dplyr::select(-dplyr::ends_with(".y")) %>%
            dplyr::rename_with(~ sub("\\.x$", "", .), dplyr::ends_with(".x"))
        )

        names(list_with_data) <- data_to_load

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (sum(vars %in% lissyrtools::lis_variables) == 0) {
        # 1.2.5 No valid LIS variables supplied # ----------------------------------

        stop("No valid LIS variable names specified.") # already ensured by check_invalid_vars(vars, lws)
      } else if (sum(vars %in% lissyrtools::lis_variables) > 0) {
        # 1.2.6 Mix between invalid variables and lis_both_hp_variables # ---------------------------

        vars <- vars[vars %in% lissyrtools::lis_variables]
        assertthat::assert_that(all(vars %in% lissyrtools::lis_both_hp_variables))

        list_with_data <- purrr::map(
          data_to_load,
          ~ haven::read_dta(
            file = paste0(path_to_files, .x, "ih", ".dta"),
            col_select = unique(c(vars, lissyrtools::key_vars_household_lis))
          )
        )
        names(list_with_data) <- paste0(data_to_load, "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n",
          "NOTE: The selected variables are not exclusive to either household or individual-level datasets. The imported datasets have been defaulted to `household-level`."
        )
      }

      return(list(data = list_with_data, message = message_to_print_in_the_end))
    }
  } else if (!exists("define_path")) {
    #  Variable selection for the sample datasets to be used in a local environemnt -------------------------------

    if (lws) {
      # 2.1.1 Select all variables, if none is specified #	 ----------------------
      if (is.null(vars)) {
        list_with_data <- data_to_load
        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n", # Proper period and paragraph space
          "All variables were imported! We recommend specifying a character vector with the desired variables in the argument `vars`."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lws_household_variables, lissyrtools::lws_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lws_person_variables, lissyrtools::lws_household_variables)
          ) ==
            0
      ) {
        # 2.1.2 Only household-level variables were selected: load only h-level file # ----------------------
        vars <- vars[vars %in% lissyrtools::lws_household_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>%
            dplyr::filter(pid == 1) %>%
            dplyr::select(unique(c(vars, lissyrtools::key_vars_household_lws)))
        )
        names(list_with_data) <- paste0(names(list_with_data), "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `household-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lws_household_variables, lissyrtools::lws_person_variables)) ==
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lws_person_variables, lissyrtools::lws_household_variables)
          ) >
            0
      ) {
        # 2.1.3 Only person-level variables were selected: load only p-level file #	----------------------
        vars <- vars[vars %in% lissyrtools::lws_person_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>% dplyr::select(unique(c(vars, lissyrtools::key_vars_person_lws)))
        )
        names(list_with_data) <- paste0(names(list_with_data), "p")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lws_household_variables, lissyrtools::lws_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lws_person_variables, lissyrtools::lws_household_variables)
          ) >
            0
      ) {
        # 2.1.4 Both household and person level varaibles were selected: load both filed and merge them # ----------------------
        hvars <- vars[vars %in% lissyrtools::lws_household_variables]
        pvars <- vars[vars %in% lissyrtools::lws_person_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>%
            dplyr::select(unique(c(
              hvars,
              pvars,
              lissyrtools::key_vars_household_lws,
              lissyrtools::key_vars_person_lws
            )))
        )

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (sum(vars %in% lissyrtools::lws_variables) == 0) {
        # 2.1.5 No valid lws variables supplied # ----------------------
        stop("No valid LWS variable names specified.") # already ensured by check_invalid_vars(vars, lws)
      } else if (sum(vars %in% lissyrtools::lws_variables) > 0) {
        # 2.1.6 # Mix between invalid variables and lws_both_hp_variables ----------------------
        vars <- vars[vars %in% lissyrtools::lws_variables]
        assertthat::assert_that(all(vars %in% lissyrtools::lws_both_hp_variables))

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>%
            dplyr::filter(pid == 1) %>%
            dplyr::select(unique(c(vars, lissyrtools::key_vars_household_lws)))
        )
        names(list_with_data) <- paste0(names(list_with_data), "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n",
          "NOTE: The selected variables are not exclusive to either household or individual-level datasets. The imported datasets have been defaulted to `household-level`."
        )
      }

      return(list(data = list_with_data, message = message_to_print_in_the_end))
    } else {
      # 2.2.1 Select all variables, if none is specified # ----------------------
      if (is.null(vars)) {
        list_with_data <- data_to_load
        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n", # Proper period and paragraph space
          "All variables were imported! We recommend specifying a character vector with the desired variables in the argument `vars`."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lis_household_variables, lissyrtools::lis_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lis_person_variables, lissyrtools::lis_household_variables)
          ) ==
            0
      ) {
        # 2.2.2 Only household-level variables were selected: load only h-level file # ----------------------
        vars <- vars[vars %in% lissyrtools::lis_household_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>%
            dplyr::filter(pid == 1) %>%
            dplyr::select(unique(c(vars, lissyrtools::key_vars_household_lis)))
        )
        names(list_with_data) <- paste0(names(list_with_data), "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `household-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lis_household_variables, lissyrtools::lis_person_variables)) ==
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lis_person_variables, lissyrtools::lis_household_variables)
          ) >
            0
      ) {
        # 2.2.3 Only person-level variables were selected: load only p-level file # ----------------------
        vars <- vars[vars %in% lissyrtools::lis_person_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>% dplyr::select(unique(c(vars, lissyrtools::key_vars_person_lis)))
        )
        names(list_with_data) <- paste0(names(list_with_data), "p")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (
        sum(vars %in% setdiff(lissyrtools::lis_household_variables, lissyrtools::lis_person_variables)) >
          0 &
          sum(
            vars %in% setdiff(lissyrtools::lis_person_variables, lissyrtools::lis_household_variables)
          ) >
            0
      ) {
        # 2.2.4 Both household and person level varaibles were selected: load both filed and merge them # ----------------------
        hvars <- vars[vars %in% lissyrtools::lis_household_variables]
        pvars <- vars[vars %in% lissyrtools::lis_person_variables]

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>%
            dplyr::select(unique(c(
              hvars,
              pvars,
              lissyrtools::key_vars_household_lis,
              lissyrtools::key_vars_person_lis
            )))
        )

        message_to_print_in_the_end <- paste0(
          "The list contains the following `person-level sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          "."
        )
      } else if (sum(vars %in% lissyrtools::lis_variables) == 0) {
        # 2.2.5 No valid LIS variables supplied # ----------------------
        stop("No valid LIS variable names specified.") # already ensured by check_invalid_vars(vars, lws)
      } else if (sum(vars %in% lissyrtools::lis_variables) > 0) {
        # 2.2.6 Mix between invalid variables and lis_both_hp_variables # ----------------------
        vars <- vars[vars %in% lissyrtools::lis_variables]
        assertthat::assert_that(all(vars %in% lissyrtools::lis_both_hp_variables))

        list_with_data <- purrr::map(
          data_to_load,
          ~ .x %>%
            dplyr::filter(pid == 1) %>%
            dplyr::select(unique(c(vars, lissyrtools::key_vars_household_lis)))
        )
        names(list_with_data) <- paste0(names(list_with_data), "h")

        message_to_print_in_the_end <- paste0(
          "The list contains the following `sample` data frames: ",
          paste(names(list_with_data), collapse = ", "),
          ".\n",
          "NOTE: The selected variables are not exclusive to either household or individual-level datasets. The imported datasets have been defaulted to `household-level`."
        )
      }

      return(list(data = list_with_data, message = message_to_print_in_the_end))
    }
  }
}



#' Subset Datasets Based on Expression
#'
#' @description
#' Internal function that subsets the given datasets based on a filtering expression.
#' Supports both LWS and LIS datasets.
#'
#' @param intermediate_data_to_filter List of data frames to subset.
#' @param lws Logical.
#' @param subset_expr An expression used to filter the datasets.
#'
#' @return List. A subsetted version of the input list based on the provided expression.
#'
#' @keywords internal
subset_datasets <- function(
  intermediate_data_to_filter,
  lws = FALSE,
  subset_expr
) {
  
  dataset_list <- intermediate_data_to_filter

  # Apply filtering logic
  filtered_datasets <- lapply(names(dataset_list), function(dataset_name) {
    data <- dataset_list[[dataset_name]]

    # Check if all variables in the subset condition exist in the dataset
    missing_vars <- setdiff(all.vars(subset_expr), names(data))
    if (length(missing_vars) > 0) {
      message(sprintf(
        "Skipping '%s' because it is missing variables: %s",
        dataset_name,
        paste(missing_vars, collapse = ", ")
      ))
      return(data) # Return unfiltered dataset
    }

    # Check if the dataset contains rows with values matching the condition
    matching_rows <- dplyr::filter(data, !!subset_expr)

    if (is.null(matching_rows) || nrow(matching_rows) == 0) {
      message(sprintf(
        "Skipping filtering for '%s' because no rows match the condition.",
        dataset_name
      ))
      return(data) # Return unfiltered dataset
    }

    # Calculate the percentage of rows deleted
    deleted_percentage <- (nrow(data) - nrow(matching_rows)) / nrow(data) * 100

    # Log the result
    message(sprintf(
      "Applying filtering on '%s'. Rows before: %d, Rows after: %d, Rows deleted: %.2f%%",
      dataset_name,
      nrow(data),
      nrow(matching_rows),
      deleted_percentage
    ))

    return(matching_rows)
  })

  names(filtered_datasets) <- names(dataset_list)

  return(filtered_datasets)
}









