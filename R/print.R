# print.R

#' Print percentiles.
#'
#' \lifecycle{experimental}
#' Computes and displays the percentiles and cumulative percentiles of a
#'   variable.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character vector of length one.
#' @param na.rm A boolean indicating if missing values should be ignored. Defaults to FALSE.
#' @return A tibble with percentile absolute and cummulative values.
#' @examples
#' \dontrun{
#' lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' print_percentiles(lissy_files = lissy_files, variable = "dhi")
#' }
print_percentiles <- function(lissy_files, variable, na.rm = FALSE){

  purrr::imap(lissy_files, .f = function(file, file_name){

    out_ <- compute_percentiles(dataset = x, variable = "dhi", na.rm = na.rm)
    out_[[paste0("cum_", y)]] <- cumsum(out_[["value"]])/sum(out_[["value"]])

    index_col_value <- which(names(out_) == "value")
    names(out_)[2] <- paste0("value_", y)

    return(out_)

  }) %>%
    purrr::reduce(dplyr::left_join, by = "percentile") %>%
    print(n = Inf)

}


#' Print Gini
#'
#' \lifecycle{experimental}
#' Computes and displays the Gini coefficient.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string indicating the aggregate for which the indicator needs to be computed.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
#' @return A named numeric vector with the Gini coefficients.
#' @examples
#' \dontrun{
#' lissy_datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' print_gini(lissy_datasets = lissy_datasets, variable = "dhi", na.rm = TRUE)
#' }
print_gini <- function(lissy_files, variable, na.rm = FALSE){

  lissy_files %>%
    purrr::map_dbl(.f = function(x){

      compute_gini(dataset = x, variable = variable, na.rm = na.rm)

    })

}



#' Print all available files
#'
#' \lifecycle{experimental}
#' Display all available LISSY files.
#'
#' @examples
#' \dontrun{
#' print_all_lissy_files()
#' }
print_all_lissy_files <- function(database){

  if(is_lissy_machine){

    stringr::str_subset(fs::dir_ls(LIS_DIR), pattern = "\\w{2}\\d{2}\\w[phr][.]dta") %>%
      stringr::str_match("(\\w{2}\\d{2}\\w)[phr][.]dta") %>%
      .[,2] %>%
      unique()
  }
}


#' Print an indicator
#'
#' \lifecycle{experimental}
#' Computes and displays an chosen with the 'indicator' argument.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string indicating the aggregate for which the indicator needs to be computed.
#' @param indicator A character string indicating the type of indicator statistic to be computed.
#'   Currently the function supports only 'mean', 'median', 'ratio' and 'gini'.
#' @param files_level  A string indicating the level of the file. Valid inputs are:
#'   'household', 'h', 'person' or 'p'. If NULL (default), the file level will
#'   be retrived from the 'lissy_files' attributes.
#' @param variable_level Level of the variable. Should be either 'household', 'h', 'person' or 'p'.
#'   If NULL (default), the function will try to guess the level of the variable.
#'   This is done by comparing the value in 'variable' with pre-set lists of variables.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @param ratio A vector of two numeric values between 0 and 1.Only used in the computation of 'ratio' indicator.
#'   Defines the percentiles in the numerator and denominator respectively.
#'   E.g. (0.9, 0.1) computes the 90/10 ratio.
#' @param epsilon A numeric vector of length one. Only used in the computation of 'atkinson' indicator'.
#'   The inequality adversion parameter. Needs to be epsilon > 0.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
#'
#' @return A numeric vector.
print_indicator <- function(lissy_files, variable, indicator, files_level = NULL, variable_level = NULL, weight = NULL, ratio = NULL, epsilon = NULL, na.rm = FALSE){

  assertthat::assert_that(indicator %in% c("mean", "median", "ratio", "gini", "atkinson"),
                          msg = "Currently supported indicators are 'mean', 'median', 'ratio', 'atkinson' and 'gini'.")


  if(missing(files_level)){

    level_ <- get_lissy_attributes(lissy_files)[["level"]]

  }else{

    level_ <- files_level

  }


  if(is.null(variable_level)){

    variable_level <- check_variable_level(variable)

  }else{

    assertthat::assert_that(variable_level %in% c("person", "household", "p", "h"),
                            msg = "Argument 'variable_level' can only take 'person', 'p', 'household' or 'h' as values.")
  }


  if(is.null(weight) & variable_level %in% c("person", "p")){

    weight_var <- "pwgt"

  }else if(is.null(weight) & variable_level %in% c("household", "h")){

    weight_var <- "hwgt"

  }else{

    weight_var <- weight

  }

  # for hh-level files, multiply the variable by 'nhhmem'
  if(level_ %in% c("household", "h")){

    assertthat::assert_that(purrr::every(lissy_files,
                                         ~"nhhmem" %in% names(.x) ),
                            msg = "All files in 'lissy_data' should contain the 'nhhmem' variable if 'print_indicator()' is used on household-level files.")

    lissy_files <- lissy_files %>%
      purrr::map(.f = function(file){
        file[[weight_var]] <- file[[weight_var]] * file[["nhhmem"]]
        return(file)
      })

  }


  if(indicator == "mean"){

    purrr::imap_dbl(lissy_files, .f = ~compute_mean(file = ..1, # .x
                                                    file_name = ..2, # .y
                                                    variable = ..3,
                                                    weight = ..4,
                                                    na.rm = ..5),
                    variable, # ..3
                    weight_var, # ..4
                    na.rm # ..5
                    )

  }else if(indicator == "median"){

    purrr::imap_dbl(lissy_files, .f = ~compute_median(file = ..1, # .x
                                                     file_name = ..2, # .y
                                                     variable = ..3,
                                                     weight = ..4,
                                                     na.rm = ..5),
                   variable, # ..3
                   weight_var, # ..4
                   na.rm # ..5
    )

  }else if(indicator == "ratio"){

    if(is.null(ratio)){
      ratio <- c(0.9, 0.1)
      warning("No input was passed to argument 'ratio' so 'ratio = c(0.9, 0.1)' was used as default.")
    }

    purrr::imap_dbl(lissy_files, .f = ~compute_ratio(file = ..1, # .x
                                                    file_name = ..2, # .y
                                                    variable = ..3,
                                                    weight = ..4,
                                                    ratio = ..5,
                                                    na.rm = ..6),
                   variable, # ..3
                   weight_var, # ..4
                   ratio, # ..5
                   na.rm # ..6
                   )

  }else if(indicator == "gini"){

    purrr::imap_dbl(lissy_files,
                    .f = ~compute_gini(file = ..1, # .x
                                       file_name = ..2, # .y,
                                       variable = ..3,
                                       weight = ..4,
                                       na.rm= ..5),
                    variable, # ..3
                    weight_var, # ..4
                    na.rm # ..5
    )

  } else if(indicator == "atkinson"){

    purrr::imap_dbl(lissy_files,
                    .f = ~compute_atkinson(file = ..1, # .x
                                           file_name = ..2, # .y
                                           variable = ..3,
                                           weight = ..4,
                                           epsilon = ..5,
                                           na.rm = ..6),
                    variable, # ..3
                    weight_var, # ..4
                    epsilon, # ..5
                    na.rm # ..6
    )
  }
}

