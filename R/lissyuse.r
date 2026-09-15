

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
#' @param database A character value. One of "lis" (default), "lws", or "lcs". Note that this does not eliminate the need to set the ‘Project’ field accordingly in the LISSY remote system.
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
#'lws_datasets <- lissyuse(data = c("us", "uk17", "uk19"), vars = "dnw", from = 2015, to = 2021, database = 'lws')
#'
#'names(lws_datasets)
#'}
lissyuse <- function(
    data = NULL,
    vars = NULL,
    subset = NULL,
    from = NULL,
    to = NULL,
    database = "lis",
    ...
) {

  dots <- list(...)

  if ("lws" %in% names(dots)) {

    if (!is.logical(dots$lws) || length(dots$lws) != 1) {
      stop("'lws' must be TRUE or FALSE.", call. = FALSE)
    }

    # Was `database` explicitly supplied?
    database_supplied <- "database" %in% names(as.list(match.call()))

    # What database does the old `lws` argument imply?
    old_database <- if (isTRUE(dots$lws)) "lws" else "lis"

    # If database was explicitly supplied, check for a conflict
    if (database_supplied && database != old_database) {
      stop(
        "The deprecated argument 'lws' conflicts with ",
        "database = \"", database, "\". ",
        "Please use 'database' instead.",
        call. = FALSE
      )
    }

    # Backwards compatibility: lws determines database
    database <- old_database

    warning(
      "The argument 'lws' is deprecated and no longer used. ",
      "Please use 'database' instead, which accepts ",
      "\"lis\" (default), \"lws\", or \"lcs\".",
      call. = FALSE
    )
  }

  database <- match.arg(database, c("lis", "lws", "lcs"))
  assertthat::assert_that(database %in% c("lis", "lws", "lcs"),
                          msg = glue::glue("'database' must be one of 'lis', 'lws', or 'lcs'. Got '{database}' instead."))
  
  # 0) Define paths and location  ---------------------------------------------------------
  
  if (!exists("define_path")) {
    data_to_load <- import_sample_datasets_to_lissyuse(data, database) # local machine ----> only access to sample datasets
  } else {
    path_to_files <- define_path(database)[[1]]
    location <- define_path(database)[[2]]
    
    # 1) Argument {data}  -------------------------------------
    
    check_empty_data(data, database)
    check_length_iso2(data)
    check_iso2(data, database)
    invalid_ccyy_pairs(data, database)
    
    # Define data to be loaded -----------------------------------
    
    data_to_load <- load_datasets(data, database, from, to)
  }
  
  # 2)  Variable-driven selection of files based on argument {vars}  -------------------------------------
  
  check_invalid_vars(vars, database)
  
  intermediate_data_and_message <- variable_selection_for_lissyuse(
    data_to_load,
    path_to_files,
    vars,
    database
  )
  
  intermediate_data_to_filter <- intermediate_data_and_message$data
  message <- intermediate_data_and_message$message
  
  # 3)  Filtering of rows based on the condition imposed in argument {subset}  -------------------------------------
  
  datasets_final <- if (!is.null(subset)) {
    subset_expr <- rlang::parse_expr(subset)
    subset_datasets(intermediate_data_to_filter, database, subset_expr)
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
  # NOTE: previously inferred "lws" vs "lis" by checking for an 'inum' column,
  # which can't distinguish a 3rd database. Since we already know which
  # database was requested, just record it directly instead of re-guessing.
  
  if (exists("relation", datasets_final[[1]])) {
    attr(datasets_final, "level") <- "p"
    attr(datasets_final, "merged_levels") <- TRUE
  } else {
    attr(datasets_final, "level") <- "h"
    attr(datasets_final, "merged_levels") <- FALSE
  }
  
  attr(datasets_final, "database") <- database
  
  # 6) Print message on the availability of the list with the datasets and its names ----------
  cat(message, "\n")
  
  # 7) Return
  return(datasets_final)
}


#' Load Dataset Names Based on Criteria
#'
#' @description
#' Internal function to filter and return dataset names based on input criteria such as
#' specified iso2 codes or ccyy pairs, database type (LIS, LWS or LCS), and optional year range.
#'
#' @param data Optional character vector specifying ccyy pairs or iso2 codes.
#' @param database A character value. One of "lis" (default), "lws", or "lcs".
#' @param from Optional numeric lower bound for dataset years.
#' @param to Optional numeric upper bound for dataset years.
#'
#' @return A character vector of dataset names matching the criteria.
#'
#' @keywords internal
load_datasets <- function(data = NULL, database = "lis", from = NULL, to = NULL) {
  
  database_upper <- toupper(database)
  
  # Step 1: Split the `data` into series for country and dataset pairs
  entire_series_for_a_country <- data[stringr::str_length(data) == 2]
  ccyy_datasets <- data[stringr::str_length(data) == 4]
  
  # Step 2: Extract dnames for entire series
  entire_series_extract_dname <- lissyrtools::datasets %>%
    dplyr::filter(database == database_upper & iso2 %in% entire_series_for_a_country) %>%
    dplyr::select(dname) %>%
    unique() %>%
    dplyr::pull()
  
  all_dnames <- c(entire_series_extract_dname, ccyy_datasets)
  
  # Step 3: Filter datasets based on the combined list and the year range
  data_to_load <- lissyrtools::datasets %>%
    dplyr::filter(database == database_upper) %>%
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
  
  attributes(data_to_load) <- NULL
  
  if (length(data_to_load) == 0) {
    stop(glue::glue(
      "No datasets matched the provided criteria. Please check the arguments provided, especially 'data'."
    ))
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
#' @param database A character value. One of "lis" (default), "lws", or "lcs".
#'
#' @return A list containing the data frames selected.
#'
#' @keywords internal
variable_selection_for_lissyuse <- function(
    data_to_load,
    path_to_files,
    vars = NULL,
    database = "lis"
) {
  
  cfg <- get_database_config(database)

  is_remote <- exists("define_path")
  
  h_suffix <- paste0(cfg$letter, "h")
  p_suffix <- paste0(cfg$letter, "p")
  
  read_file <- function(dataset_name, suffix, col_select = NULL) {
    if (is_remote) {
      haven::read_dta(file = paste0(path_to_files, dataset_name, suffix, ".dta"), col_select = col_select)
    } else {
      # local sample data: data_to_load is already a named list of data frames
      df <- data_to_load[[dataset_name]]
      if (!is.null(col_select)) df <- dplyr::select(df, dplyr::any_of(col_select))
      df
    }
  }
  
  join_and_clean <- function(df_h, df_p) {
    dplyr::inner_join(df_h, df_p, by = cfg$join_by) %>%
      dplyr::select(-dplyr::ends_with(".y")) %>%
      dplyr::rename_with(~ sub("\\.x$", "", .), dplyr::ends_with(".x"))
  }
  
  dataset_names <- if (is_remote) data_to_load else names(data_to_load)
  
  # -- Branch dispatch (same 6 cases as before, now written once) ---------------
  
  if (is.null(vars)) {
    # 1) no variables specified: load everything, merge h+p
    
    if (is_remote) {
      files_h <- purrr::map(dataset_names, ~ read_file(.x, h_suffix, cfg$household_variables))
      files_p <- purrr::map(dataset_names, ~ read_file(.x, p_suffix, cfg$person_variables))
      list_with_data <- purrr::map2(files_h, files_p, join_and_clean)
      names(list_with_data) <- dataset_names
    } else {
      list_with_data <- data_to_load
    }
    
    msg_label <- if (is_remote) "person-level" else "person-level sample"
    message_to_print_in_the_end <- message(paste0(
      "The list contains the following `", msg_label, "` data frames: ",
      paste(names(list_with_data), collapse = ", "), ".\n",
      "All variables were imported! We recommend specifying a character vector with the desired variables in the argument `vars`."
    ))
    
  } else if (sum(vars %in% setdiff(cfg$household_variables, cfg$person_variables)) > 0 &&
             sum(vars %in% setdiff(cfg$person_variables, cfg$household_variables)) == 0) {
    # 2) household-only variables
    
    vars_h <- vars[vars %in% cfg$household_variables]
    col_select <- unique(c(vars_h, cfg$key_vars_household))
    
    if (is_remote) {
      list_with_data <- purrr::map(dataset_names, ~ read_file(.x, h_suffix, col_select))
      names(list_with_data) <- paste0(dataset_names, "h")
    } else {
      list_with_data <- purrr::map(dataset_names, ~ data_to_load[[.x]] %>%
                                     dplyr::filter(if ("pid" %in% names(.)) pid == 1 else TRUE) %>%
                                     dplyr::select(dplyr::any_of(col_select)))
      names(list_with_data) <- paste0(dataset_names, "h")
    }
    
    msg_label <- if (is_remote) "household-level" else "household-level sample"
    message_to_print_in_the_end <- message(paste0(
      "The list contains the following `", msg_label, "` data frames: ",
      paste(names(list_with_data), collapse = ", "), "."
    ))
    
  } else if (sum(vars %in% setdiff(cfg$household_variables, cfg$person_variables)) == 0 &&
             sum(vars %in% setdiff(cfg$person_variables, cfg$household_variables)) > 0) {
    # 3) person-only variables
    
    vars_p <- vars[vars %in% cfg$person_variables]
    col_select <- unique(c(vars_p, cfg$key_vars_person))
    
    list_with_data <- purrr::map(dataset_names, ~ read_file(.x, p_suffix, col_select))
    names(list_with_data) <- paste0(dataset_names, "p")
    
    msg_label <- if (is_remote) "person-level" else "person-level sample"
    message_to_print_in_the_end <- message(paste0(
      "The list contains the following `", msg_label, "` data frames: ",
      paste(names(list_with_data), collapse = ", "), "."
    ))
    
  } else if (sum(vars %in% setdiff(cfg$household_variables, cfg$person_variables)) > 0 &&
             sum(vars %in% setdiff(cfg$person_variables, cfg$household_variables)) > 0) {
    # 4) both household and person variables: load both, merge
    
    hvars <- vars[vars %in% cfg$household_variables]
    pvars <- vars[vars %in% cfg$person_variables]
    
    if (is_remote) {
      files_h <- purrr::map(dataset_names, ~ read_file(.x, h_suffix, unique(c(hvars, cfg$key_vars_household))))
      files_p <- purrr::map(dataset_names, ~ read_file(.x, p_suffix, unique(c(pvars, cfg$key_vars_person))))
      list_with_data <- purrr::map2(files_h, files_p, join_and_clean)
      names(list_with_data) <- dataset_names
    } else {
      list_with_data <- purrr::map(dataset_names, ~ data_to_load[[.x]] %>%
                                     dplyr::select(dplyr::any_of(unique(c(hvars, pvars, cfg$key_vars_household, cfg$key_vars_person)))))
      names(list_with_data) <- dataset_names
    }
    
    msg_label <- if (is_remote) "person-level" else "person-level sample"
    message_to_print_in_the_end <- message(paste0(
      "The list contains the following `", msg_label, "` data frames: ",
      paste(names(list_with_data), collapse = ", "), "."
    ))
    
  } else if (sum(vars %in% cfg$all_variables) == 0) {
    # 5) no valid variables
    stop(glue::glue("No valid {toupper(database)} variable names specified.")) # already ensured by check_invalid_vars()
    
  } else if (sum(vars %in% cfg$all_variables) > 0) {
    # 6) mix of invalid + "both_hp" variables: default to household
    
    vars_valid <- vars[vars %in% cfg$all_variables]
    assertthat::assert_that(all(vars_valid %in% cfg$both_hp_variables))
    
    col_select <- unique(c(vars_valid, cfg$key_vars_household))
    
    if (is_remote) {
      list_with_data <- purrr::map(dataset_names, ~ read_file(.x, h_suffix, col_select))
    } else {
      list_with_data <- purrr::map(dataset_names, ~ data_to_load[[.x]] %>%
                                     dplyr::filter(if ("pid" %in% names(.)) pid == 1 else TRUE) %>%
                                     dplyr::select(dplyr::any_of(col_select)))
    }
    names(list_with_data) <- paste0(dataset_names, "h")
    
    msg_label <- if (is_remote) "" else "sample "
    message_to_print_in_the_end <- message(paste0(
      "The list contains the following `", msg_label, "data frames: ",
      paste(names(list_with_data), collapse = ", "), ".\n",
      "NOTE: The selected variables are not exclusive to either household or individual-level datasets. The imported datasets have been defaulted to `household-level`."
    ))
  }
  
  return(list(data = list_with_data, message = message_to_print_in_the_end))
}


#' Subset Datasets Based on Expression
#'
#' @description
#' Internal function that subsets the given datasets based on a filtering expression.
#' Supports both LWS and LIS datasets.
#'
#' @param intermediate_data_to_filter List of data frames to subset.
#' @param database A character value. One of "lis" (default), "lws", or "lcs".
#' @param subset_expr An expression used to filter the datasets.
#'
#' @return List. A subsetted version of the input list based on the provided expression.
#'
#' @keywords internal
subset_datasets <- function(
    intermediate_data_to_filter,
    database = "lis",
    subset_expr
) {
  # NOTE: 'database' was already unused inside the original function body —
  # kept as a parameter for API consistency with the rest of the pipeline,
  # but it plays no role in the filtering logic itself.
  
  dataset_list <- intermediate_data_to_filter
  
  filtered_datasets <- lapply(names(dataset_list), function(dataset_name) {
    data <- dataset_list[[dataset_name]]
    
    missing_vars <- setdiff(all.vars(subset_expr), names(data))
    if (length(missing_vars) > 0) {
      message(sprintf(
        "Skipping '%s' because it is missing variables: %s",
        dataset_name, paste(missing_vars, collapse = ", ")
      ))
      return(data)
    }
    
    matching_rows <- dplyr::filter(data, !!subset_expr)
    
    if (is.null(matching_rows) || nrow(matching_rows) == 0) {
      message(sprintf(
        "Skipping filtering for '%s' because no rows match the condition.",
        dataset_name
      ))
      return(data)
    }
    
    deleted_percentage <- (nrow(data) - nrow(matching_rows)) / nrow(data) * 100
    
    message(sprintf(
      "Applying filtering on '%s'. Rows before: %d, Rows after: %d, Rows deleted: %.2f%%",
      dataset_name, nrow(data), nrow(matching_rows), deleted_percentage
    ))
    
    return(matching_rows)
  })
  
  names(filtered_datasets) <- names(dataset_list)
  return(filtered_datasets)
}








