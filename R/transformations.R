# transformations




#' Equivalise by number of household members.
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' Scales a variable dividing it by 'nhhmem'^eq_scale. Where 'nhhmem' is the LIS
#' or LWS variable measuring the number of members in the household.
#'
#' Throws a warning if the function is applied to a person-level variable.
#'
#' @param lissy_files A list of LIS or LWS files
#' @param variable A character vector of length one with the indicator that needs to be transformed.
#' @param eq_scale A real number.
#'
#' @return A list of tibbles with the transformed variable.
#' @examples
#' \dontrun{
#' lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' transform_equivalise(list_files = lissy_files, variable = "dhi")
#' }
transform_equivalise <- function(lissy_files, variable, eq_scale = 0.5){
  
  .Deprecated("apply_sqrt_equivalisation()", package = "lissyrtools", msg = "The function `transform_equivalise()` is now deprecated, please use `apply_sqrt_equivalisation()` for lists instead.") 
  

  copy_attributes <- get_lissy_attributes(lissy_files)


  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~implement_equivalise(file = .x,
                                                        file_name = .y,
                                                        variable = variable,
                                                        eq_scale = eq_scale),
                             variable, eq_scale)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)

}



#' Apply equivalise by number of household members
#'
#' `r lifecycle::badge("superseded")`
#' Applies the equivalisation by number of household members in a file for a single variable.
#'
#' To be used inside transform_equivalise().
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the variable to which equivalisation should be applied.
#' @param eq_scale A real number. Defaults to 0.5. The variable will be equivalized using n_household_members^eq_scale
#' @return A a file with the equivalised variable.
#'
#' @keywords internal
implement_equivalise <- function(file, file_name, variable, eq_scale = 0.5){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))


  if (variable %in% c(lissyrtools::lis_person_variables,
                      lissyrtools::lws_person_variables,
                      lissyrtools::erflis_person_variables) ){

    # warning if variable is person-level
    warning(glue::glue("'{variable}' is a person-level variable and might not need to be equivalised!"))

  }

  assertthat::assert_that("nhhmem" %in% names(file),
                          msg = glue::glue("'nhhmem' could not be found in '{file_name}'."))


  if(!is.all.na.or.zero(file[[variable]])){
    file[[variable]] <- file[[variable]]/(file[["nhhmem"]]^eq_scale)
  }

  return(file)

}



#' Equivalise with the OECD scale
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' Scales a variable using a weight for the adults (excluding the first one) and
#'   children.
#'
#' Throws a warning if the function is applied to a person-level variable.
#'
#' @param lissy_files A list of LIS or LWS files
#' @param variable A character vector of length one with the indicator that needs to be transformed.
#' @param value_other_adults A real number. Defaults to 0.7. The value assigned to the
#'   second to last adults in the household.
#' @param value_children A real number. Defaults to 0.7. The value assigned to children
#'   in the household.
#'
#' @return A list of tibbles with the transformed variable.
#' @examples
#' \dontrun{
#' lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' transform_equivalise_oecd(list_files = lissy_files, variable = "dhi")
#' }
transform_equivalise_oecd <- function(lissy_files, variable, value_other_adults = 0.7, value_children = 0.5){

  .Deprecated("apply_oecd_equivalisation()", package = "lissyrtools", msg = "The function `transform_equivalise_oecd()` is now deprecated, please use `apply_oecd_equivalisation()` for lists instead.") 
  
  copy_attributes <- get_lissy_attributes(lissy_files)


  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~implement_equivalise_oecd(file = .x,
                                                             file_name = .y,
                                                             variable = variable,
                                                             value_other_adults = value_other_adults,
                                                             value_children = value_children),
                             variable, eq_scale)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)
  
}





#' Apply equivalise with the OECD scale
#'
#' `r lifecycle::badge("superseded")`
#' Applies the equivalisation by number of  adults and children in a household.
#'
#' To be used inside transform_equivalise_oecd().
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the variable to which equivalisation should be applied.
#' @param value_other_adults A real number. Defaults to 0.7. The value assigned to the
#'   second to last adults in the household.
#' @param value_children A real number. Defaults to 0.7. The value assigned to children
#'   in the household.
#'
#' @return A a file with the equivalised variable.
#'
#' @keywords internal
implement_equivalise_oecd <- function(file, file_name, variable, value_other_adults = 0.7, value_children = 0.5){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))


  if (variable %in% c(lissyrtools::lis_person_variables,
                      lissyrtools::lws_person_variables,
                      lissyrtools::erflis_person_variables) ){

    # warning if variable is person-level
    warning(glue::glue("'{variable}' is a person-level variable and might not need to be equivalised!"))

  }

  assertthat::assert_that(all(c("nhhmem", "nhhmem13") %in% names(file)),
                          msg = glue::glue("'nhhmem' and 'nhhmem13' need to be in '{file_name}'."))

  if(!is.all.na.or.zero(file[[variable]])){

    n_other_adults <- file[["nhhmem"]] - file[["nhhmem13"]] - 1
    factor <- 1 + value_other_adults* n_other_adults + file[["nhhmem13"]] * value_children

    file[[variable]] <- file[[variable]]/(factor)
  }

  return(file)

}




#' Recodes zeros into missing values
#'
#' `r lifecycle::badge("superseded")`
#' Recodes all zeros in the selected variable into missing valuse (NAs).
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be adjusted
#' @return A list of tibbles with the adjusted variable.
#' @keywords internal
transform_zeros_to_na <- function(lissy_files, variable){

  copy_attributes <- get_lissy_attributes(lissy_files)


  lissy_files <- purrr::imap(.x = lissy_files,
                                .f = ~implement_zeros_to_na(file = .x,
                                                            file_name = .y,
                                                           variable),
                                variable)


  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)

}



#' Apply recoding of zeros into missing values
#'
#' `r lifecycle::badge("superseded")`
#' Applies the recoding of zeroes into missing values in a file for a single variable.
#'
#' To be used inside transform_zeros_to_na().
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file
#' @param variable A string with the variable to which recoding should be applied.
#' @return A file with the recoded variable.
#'
#' @keywords internal
implement_zeros_to_na <- function(file, file_name, variable){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))

  if( !all(is.na(file[[variable]])) ){

    index_zero <- file[[variable]] == 0 & !is.na(file[[variable]])

    file[index_zero, variable] <- NA

  }

  return(file)


}



#' Adjust aggregates by LIS PPPs
#'
#' @description
#' `r lifecycle::badge("superseded")`
#' Adjusts an aggregate by both the CPI and PPP.
#'
#' For LWS income variables, it takes into account the reference year of the dataset variables.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be adjusted.
#' @param database 'lis' or 'lws' to specify which database the files belong to. If NULL (default)
#'   the function reads the 'database' attribute from the list in 'lissy_files'.
#' @param path_to_ppp_file A character string indicating where the deflator values
#'   can be found. If the value is 'lissyrtools' (default), it will import the data from 'lissyrtools'.
#'   These values are equivalent to the ones in: datacenter.org/resources/ppp-deflators/ .
#'   Specifying 'lissy' will read them from within the LISSY directory. Any other
#'   value requires the full path specification to the deflators file.
#' @param income_variable It is only relevant for LWS files. If the file is
#'   LWS and 'income_variable = TRUE', the function will retrieve the deflator for
#'   the year in which the income data was collected.
#'   This reference year might or might not be the same as the one when the wealth
#'   information was collected (i.e. the year of the file - 2010 for 'fr10wh').
#'   The default NULL checks the name of the variable against the name of the income
#'   variables in LWS files. A vector containing the list of these can be found
#'   in lissyrtools::lws_income_variables. Setting the argument to FALSE forces the adjustment
#'   to use the same year as the year of the file regardless of the value passed to 'variable'.
#' @return A list of tibbles with the adjusted variable.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(magrittr)
#' lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' lissy_files %<>%
#'     transform_adjust_by_lisppp(variable = "dhi")
#'}
transform_adjust_by_lisppp <- function(lissy_files, variable, database = NULL, income_variable = NULL, path_to_ppp_file = "lissyrtools"){

  
  .Deprecated("apply_ppp_adjustment()", package = "lissyrtools", msg = "The function `transform_adjust_by_lisppp()` is now deprecated, please use `apply_ppp_adjustment()` for lists instead.") 
  
  
  if(missing(database)){

    database_ <- get_database(lissy_files)

  }else{

    database_ <- database

  }

  assertthat::assert_that(database_ %in% c('lis', 'lws', 'erflis', 'i', 'w', 'e'),
                          msg = glue::glue("Only 'lis', 'lws', 'erflis', 'i', 'w' and 'e' are valid values for databases. Got '{database_}'."))


  copy_attributes <- get_lissy_attributes(lissy_files)



  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = function(file, file_name, database, variable, income_variable, ppp_data){

                               implement_adjust_by_lisppp(file,
                                                          file_name,
                                                          database = database,
                                                          variable = variable,
                                                          income_variable = income_variable,
                                                          ppp_data = ppp_data)

                             },
                             database_,
                             variable,
                             income_variable,
                             ppp_data = import_lisppp_data(path_to_ppp_file))


  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)
  

}



#' Applies the deflator adjustment to an aggregate
#'
#' `r lifecycle::badge("superseded")`
#'
#' To be used inside transform_adjust_by_lisppp().
#'
#' @param file A list of LIS or LWS file
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the name of the variable to which the adjustment should be applied.
#' @param income_variable Defaults to NULL. Is only relevant for LWS files. If the file is
#'   LWS and 'income_variable = TRUE', the function will retrieve the deflator for
#'   the year in which the income data was collected. This might not be the same
#'   as the year when wealth variables were collected.
#' @param ppp_data An optional file with the deflators. Should be in the same
#'   format as the tibble in 'lissyrtools::deflators'. If "lissyrtools" (default) the deflators
#'   are imported from the package internal data.
#'
#'
#' @keywords internal
implement_adjust_by_lisppp <- function(file, file_name, database, variable, income_variable = NULL, ppp_data = "lissyrtools"){

  assertthat::assert_that(database %in% c('lis', 'lws', 'erflis', 'i', 'w', 'e'),
                          msg = "Incorrect value in 'database' argument.")

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))

  if(!is.all.na.or.zero(file[[variable]])){

    ccyy_format_bool <- stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2}$")

    if(!ccyy_format_bool){

      file_name <- change_file_name_format(file_name, to_format = 'ccyy')

    }


    lisppp <- get_file_lisppp(file_name = file_name, database, variable, income_variable = income_variable, ppp_data = ppp_data)


    file[[variable]] <- file[[variable]] / lisppp

  }

  return(file)

}




#' Recode negative values to zero
#'
#' `r lifecycle::badge("superseded")`
#' Recodes all negative values to zero for all files in a list.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be transformed.
#' @return A list of tibbles with the recoded variable.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(magrittr)
#' lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' lissy_files %<>%
#'     transform_negative_values_to_zero(variable = "dhi")
#'     }
transform_negative_values_to_zero <- function(lissy_files, variable){


  copy_attributes <- get_lissy_attributes(lissy_files)



  lissy_files <-  purrr::imap(.x = lissy_files,
                              .f = function(file, file_name, variable){

                                implement_negative_values_to_zero(file, file_name, variable)

                              },
                              variable
                              )



  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)

}



#' Apply recoding negative values to zero
#'
#' `r lifecycle::badge("superseded")`
#' Applies the recoding of negative values into zeroes in a variable of a single file.
#'
#' To be used inside transform_negative_values_to_zero().
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file
#' @param variable A string with the variable to which recoding should be applied.
#' @return A file with the recoded variable.
#'
#' @keywords internal
implement_negative_values_to_zero <- function(file, file_name, variable){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))


  file[[variable]] <- dplyr::if_else((file[[variable]]< 0) & !is.na(file[[variable]]),
                                        true = 0,
                                        false = as.numeric(file[[variable]]))

  return(file)

}




#' Apply top or bottom coding with log IQR
#'
#' @description
#' `r lifecycle::badge("superseded")`

#' Applies an upper or lower limit on variable values using the Interquartile Range (IQR)
#'   of the variable transformed with the natural logarithm and a scale factor
#'   ('times').
#'
#' If the lissy files passed are at person-level and the variable is household-level,
#'   only household heads are used to compute the IQR of the log transformed variable.
#'   For person-level variables, all individuals in the file are used.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be transformed.
#' @param times A numeric indicating the scale factor for IQR. Defaults to 3.
#' @param files_level A string indicating the level of the file. Valid inputs are:
#'   'household', 'h', 'person' or 'p'. If NULL (default), the file level will
#'   be retrived from the 'lissy_files' attributes.
#' @param variable_level Level of the variable. Should be either 'household', 'h', 'person' or 'p'.
#'   If NULL (default), the function will try to guess the level of the variable.
#'   This is done by comparing the value in 'variable' with pre-set lists of variables.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @return A list of tibbles with the recoded variable.
transform_top_code_with_iqr <- function(lissy_files, variable, times = 3, files_level = NULL, variable_level = NULL, weight = NULL){
  
  .Deprecated("apply_iqr_top_bottom_coding()", package = "lissyrtools", msg = "The function `transform_top_code_with_iqr()` is now deprecated, please use `apply_iqr_top_bottom_coding()` for lists instead.") 

  copy_attributes <- get_lissy_attributes(lissy_files)

  if(missing(files_level)){

    level_ <- copy_attributes[["level"]]

  }else{
    level_ <- files_level
  }

  assertthat::assert_that(!is.null(level_),
                          msg = "'lissy_files' should have a 'level' attribute or this should be specified in 'files_level' argument.")

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~implement_top_code_with_iqr(file = ..1,
                                                               file_name = ..2,
                                                               variable = ..3,
                                                               times = ..4,
                                                               file_level = ..5,
                                                               variable_level = ..6,
                                                               weight = ..7 )
                             ,variable, times, level_, variable_level, weight
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
  
}




#' Apply top or bottom coding with log IQR to a single file
#'
#' `r lifecycle::badge("superseded")`
#' Applies an upper or lower limit on variable values using the Interquartile Range (IQR)
#' of the variable transformed with the natural logarithm and a scale factor
#' ('times'). For 'household level' variables, the IQR of the log transformed variable is computed using only
#' household heads. For 'person level' variables, all individuals in the file are used.
#'
#' To be used inside transform_top_code_with_iqr() and transform_bottom_code_with_iqr() .
#'
#' @param file A LIS or LWS file.
#' @param file_name A string with the name of the LIS or LWS file.
#' @param variable A string with the variable to which top coding should be applied.
#' @param times A numeric indicating the scale factor for IQR. Defaults to 3.
#' @param file_level A string indicating the level of the file. Valid inputs are:
#'   'household', 'h', 'person' or 'p'.
#' @param variable_level A string with the level of the variable. Should be either 'household' or 'person'.
#'   If NULL (default), the function will try to guess the level of the variable.
#'   This is done by comparing the value in 'variable' with pre-set lists of variables.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#'
#' @return A tibble containing the file with the recoded variable.
#'
#' @keywords internal
implement_top_code_with_iqr <- function(file, file_name, variable, times, file_level, variable_level = NULL, weight = NULL){

  assertthat::assert_that(file_level %in% c("person", "household", "p", "h"),
                          msg = "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")


  if(file_level %in% c("household", "h")){

    assertthat::assert_that( is.null(variable_level) || variable_level %in% c("household", "h"),
                             msg = glue::glue("Household-level files such as '{file_name}' should only have household-level variables. Variable '{variable}' was specified as person-level."))

  }

  if(file_level %in% c("person", "p")){

    implement_top_code_with_iqr_pfile(file, file_name, variable, times, variable_level, weight)

  }else{

    implement_top_code_with_iqr_hfile(file, file_name, variable, times, weight)

  }

}



#' Apply top and bottom coding with log IQR to a level-defined file
#'
#' Lower-level functions used within 'implement_top_code_with_iqr()' and
#'   'implement_bottom_code_with_iqr()'.
#'
#' @param file A tibble or data.frame with a LIS or LWS file.
#' @param file_name A string with the name of the LIS or LWS file.
#' @param variable A string with the variable to which top coding should be applied.
#' @param times A numeric indicating the scale factor for IQR. Defaults to 3.
#' @param variable_level A string with the level of the variable. Should be either 'household' or 'person'.
#'   If NULL (default), the function will try to guess the level of the variable.
#'   This is done by comparing the value in 'variable' with pre-set lists of variables.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#'
#' @return A tibble containing the file with the recoded variable.
#'
#' @keywords internal
implement_top_code_with_iqr_pfile <- function(file, file_name, variable, times = 3, variable_level = NULL, weight = NULL){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))

  var_ <- file[[variable]]

  if(!is.all.na.or.zero(var_)){

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

    assertthat::assert_that(all(var_ >= 0 | is.na(var_)),
                            msg = glue::glue("Error in '{file_name}'. The variable where top coding with log IQR is applied can not have negative values."))

    assertthat::assert_that(weight_var %in% names(file),
                            msg = glue::glue("'{weight_var}' could not be found in '{file_name}'."))

    if(variable_level %in% c("household", "h")){

      assertthat::assert_that("relation" %in% names(file),
                              msg = glue::glue("'relation' could not be found in '{file_name}'."))

      missing_values_in_variable_warning(file = file,
                                         file_name = file_name,
                                         variable = "relation")

    }

    missing_values_in_variable_warning(file = file,
                                       file_name = file_name,
                                       variable = weight_var)


    log_var <- dplyr::if_else(var_ > 0,
                              true = log(var_),
                              false = 0,
                              missing = NA_real_)

    index_valid_weights <- !is.na(file[[weight_var]]) # this shouldn't be happening, but it happens for some LIS and LWS Japan files


    if(variable_level == "household"){

      index_hh_head <- get_index_hh_heads(file)

      log_var_for_iqr_computation <- log_var[index_valid_weights & index_hh_head]
      weights_for_iqr_computation <- file[index_valid_weights & index_hh_head, weight_var, drop = TRUE]

    }else{
      log_var_for_iqr_computation <- log_var[index_valid_weights]
      weights_for_iqr_computation <- file[index_valid_weights, weight_var, drop = TRUE]

    }


    if(!is.all.na.or.zero(log_var_for_iqr_computation)){

      log_third_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.75))

      log_first_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.25))

      log_times_iqr <- (log_third_quartile - log_first_quartile) * times

      var_ <- dplyr::if_else(log(var_) > (log_third_quartile + log_times_iqr),
                             true = exp(log_third_quartile + log_times_iqr),
                             false = var_)

    }

    file[[variable]] <- var_

  }

  return(file)

}



#' @rdname implement_top_code_with_iqr_pfile
implement_top_code_with_iqr_hfile <- function(file, file_name, variable, times = 3, weight = NULL){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))

  var_ <- file[[variable]]


  # throw warning if variable_level not "household"

  if(!is.all.na.or.zero(var_)){

    safe_variable_level <- safely_check_variable_level(variable)

    if(!is.null(safe_variable_level[["result"]]) &&
                safe_variable_level[["result"]] %in% c("person", "p") ){

        warning(glue::glue("The variable '{variable}' is at person-level and the file '{file_name}' is at household-level. The methods used to top code might not be correct."))

    }


    if(is.null(weight)){

      weight_var <- "hwgt"

    }else{

      weight_var <- weight

    }

    assertthat::assert_that(all(var_ >= 0 | is.na(var_)),
                            msg = glue::glue("Error in '{file_name}'. The variable where top coding with log IQR is applied can not have negative values."))

    assertthat::assert_that(weight_var %in% names(file),
                            msg = glue::glue("'{weight_var}' could not be found in '{file_name}'."))

    missing_values_in_variable_warning(file = file,
                                       file_name = file_name,
                                       variable = weight_var)

    log_var <- dplyr::if_else(var_ > 0,
                              true = log(var_),
                              false = 0,
                              missing = NA_real_)

    index_valid_weights <- !is.na(file[[weight_var]]) # this shouldn't be happening, but it happens for some LIS and LWS Japan files


    log_var_for_iqr_computation <- log_var[index_valid_weights]
    weights_for_iqr_computation <- file[index_valid_weights, weight_var, drop = TRUE]


    if(!is.all.na.or.zero(log_var_for_iqr_computation)){

      log_third_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.75))

      log_first_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.25))

      log_times_iqr <- (log_third_quartile - log_first_quartile) * times

      var_ <- dplyr::if_else(log(var_) > (log_third_quartile + log_times_iqr),
                             true = exp(log_third_quartile + log_times_iqr),
                             false = var_)

    }

    file[[variable]] <- var_


  }


  return(file)

}




#' @rdname transform_top_code_with_iqr
transform_bottom_code_with_iqr <- function(lissy_files, variable, times = 3, files_level = NULL, variable_level = NULL, weight = NULL){

  copy_attributes <- get_lissy_attributes(lissy_files)

  if(missing(files_level)){

    level_ <- copy_attributes[["level"]]

  }else{
    level_ <- files_level
  }

  assertthat::assert_that(!is.null(level_),
                          msg = "'lissy_files' should have a 'level' attribute or this should be specified in 'files_level' argument.")

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~implement_bottom_code_with_iqr(file = ..1,
                                                               file_name = ..2,
                                                               variable = ..3,
                                                               times = ..4,
                                                               file_level = ..5,
                                                               variable_level = ..6,
                                                               weight = ..7 )
                             ,variable, times, level_, variable_level, weight
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
  
  .Deprecated("apply_iqr_top_bottom_coding()", package = "lissyrtools", msg = "The function `transform_bottom_code_with_iqr()` is now deprecated, please use `apply_iqr_top_bottom_coding()` for lists instead.") 
  

}



#' @rdname implement_top_code_with_iqr
implement_bottom_code_with_iqr <- function(file, file_name, variable, times, file_level, variable_level = NULL, weight = NULL){

  assertthat::assert_that(file_level %in% c("person", "household", "p", "h"),
                          msg = "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")


  if(file_level %in% c("household", "h")){

    assertthat::assert_that( is.null(variable_level) || variable_level %in% c("household", "h"),
                             msg = glue::glue("Household-level files such as '{file_name}' should only have household-level variables. Variable '{variable}' was specified as person-level."))

  }


  if(file_level %in% c("person", "p")){

    implement_bottom_code_with_iqr_pfile(file, file_name, variable, times, variable_level, weight)

  }else{

    implement_bottom_code_with_iqr_hfile(file, file_name, variable, times, weight)

  }

}



#' @rdname implement_top_code_with_iqr_pfile
implement_bottom_code_with_iqr_pfile <- function(file, file_name, variable, times = 3, variable_level = NULL, weight = NULL){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))

  var_ <- file[[variable]]

  if(!is.all.na.or.zero(var_)){

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

    assertthat::assert_that(all(var_ >= 0 | is.na(var_)),
                            msg = glue::glue("Error in '{file_name}'. The variable where top coding with log IQR is applied can not have negative values."))

    assertthat::assert_that(weight_var %in% names(file),
                            msg = glue::glue("'{weight_var}' could not be found in '{file_name}'."))

    if(variable_level %in% c("household", "h")){

      assertthat::assert_that("relation" %in% names(file),
                              msg = glue::glue("'relation' could not be found in '{file_name}'."))

      missing_values_in_variable_warning(file = file,
                                         file_name = file_name,
                                         variable = "relation")

    }

    missing_values_in_variable_warning(file = file,
                                       file_name = file_name,
                                       variable = weight_var)


    log_var <- dplyr::if_else(var_ > 0,
                              true = log(var_),
                              false = 0,
                              missing = NA_real_)

    index_valid_weights <- !is.na(file[[weight_var]]) # this shouldn't be happening, but it happens for some LIS and LWS Japan files


    if(variable_level == "household"){

      index_hh_head <- get_index_hh_heads(file)

      log_var_for_iqr_computation <- log_var[index_valid_weights & index_hh_head]
      weights_for_iqr_computation <- file[index_valid_weights & index_hh_head, weight_var, drop = TRUE]

    }else{

      log_var_for_iqr_computation <- log_var[index_valid_weights]
      weights_for_iqr_computation <- file[index_valid_weights, weight_var, drop = TRUE]

    }


    if(!is.all.na.or.zero(log_var_for_iqr_computation)){

      log_third_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.75))

      log_first_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.25))

      log_times_iqr <- (log_third_quartile - log_first_quartile) * times


      if( (log_first_quartile - log_times_iqr) <= 0){
                              # don't recode 0s to bottom code
                              # (var_ != 0) is needed, else it is replaced with log(0) = -Inf
      var_ <- dplyr::if_else((var_ != 0) & (log(var_) < (log_first_quartile - log_times_iqr)),
                             true = exp(log_first_quartile - log_times_iqr),
                             false = var_)
      }else{
                            # recode 0s to bottom code
        var_ <- dplyr::if_else( (log(var_) < (log_first_quartile - log_times_iqr)),
                               true = exp(log_first_quartile - log_times_iqr),
                               false = var_)

      }

    }

    file[[variable]] <- var_


  }

  return(file)

}




#' @rdname implement_top_code_with_iqr_pfile
implement_bottom_code_with_iqr_hfile <- function(file, file_name, variable, times = 3, weight = NULL){

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("Variable '{variable}' could not be found in '{file_name}'."))

  var_ <- file[[variable]]


  # throw warning if variable_level not "household"

  if(!is.all.na.or.zero(var_)){

    safe_variable_level <- safely_check_variable_level(variable)

    if(!is.null(safe_variable_level[["result"]]) &&
       safe_variable_level[["result"]] %in% c("person", "p") ){

      warning(glue::glue("The variable '{variable}' is at person-level and the file '{file_name}' is at household-level. The methods used to top code might not be correct."))

    }


    if(is.null(weight)){

      weight_var <- "hwgt"

    }else{

      weight_var <- weight

    }

    assertthat::assert_that(all(var_ >= 0 | is.na(var_)),
                            msg = glue::glue("Error in '{file_name}'. The variable where top coding with log IQR is applied can not have negative values."))

    assertthat::assert_that(weight_var %in% names(file),
                            msg = glue::glue("'{weight_var}' could not be found in '{file_name}'."))

    missing_values_in_variable_warning(file = file,
                                       file_name = file_name,
                                       variable = weight_var)

    log_var <- dplyr::if_else(var_ > 0,
                              true = log(var_),
                              false = 0,
                              missing = NA_real_)

    index_valid_weights <- !is.na(file[[weight_var]]) # this shouldn't be happening, but it happens for some LIS and LWS Japan files


    log_var_for_iqr_computation <- log_var[index_valid_weights]
    weights_for_iqr_computation <- file[index_valid_weights, weight_var, drop = TRUE]


    if(!is.all.na.or.zero(log_var_for_iqr_computation)){

      log_third_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.75))

      log_first_quartile <- unname(wtd.quantile(log_var_for_iqr_computation,
                                         weights = weights_for_iqr_computation,
                                         probs = 0.25))

      log_times_iqr <- (log_third_quartile - log_first_quartile) * times

      if( (log_first_quartile - log_times_iqr) <= 0){
        # don't recode 0s to bottom code
        # (var_ != 0) is needed, else it is replaced with log(0) = -Inf
        var_ <- dplyr::if_else((var_ != 0) & (log(var_) < (log_first_quartile - log_times_iqr)),
                               true = exp(log_first_quartile - log_times_iqr),
                               false = var_)
      }else{
        # recode 0s to bottom code
        var_ <- dplyr::if_else( (log(var_) < (log_first_quartile - log_times_iqr)),
                                true = exp(log_first_quartile - log_times_iqr),
                                false = var_)

      }

    }

    file[[variable]] <- var_


  }


  return(file)

}




#' Recode zeros into missing values if all values are zero
#'
#' 
#' @description
#' `r lifecycle::badge("superseded")`
#' Recodes all zeros in the selected variable into missing values (NAs) if (and
#'   only if) all values are zeros.
#'
#' @details
#' Some LIS/LWS datasets have variables with only '0' values. These do not
#'   represent 0s but NAs. This function transform the 0s into NAs if it finds
#'   that all values are 0s and there are no other valid values.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be adjusted.
#' @return A list of tibbles with the adjusted variable.
#' @keywords internal
transform_false_zeros_to_na <- function(lissy_files, variable){

  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~implement_false_zeros_to_na(file = .x,
                                                               file_name = .y,
                                                               variable),
                             variable)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)

}


#' Apply the recoding of zeros into missing values if all values are zero
#'
#' Lower-level function used within 'transform_false_zeros_to_na()' .
#'
#' @param file A tibble or data.frame with a LIS or LWS file.
#' @param file_name A string with the name of the LIS or LWS file.
#' @param variable A string with the variable to which top coding should be applied.
#'
#' @return A tibble containing the file with the recoded variable.
#'
#' @keywords internal
implement_false_zeros_to_na <- function(file, file_name, variable){

  assertthat::has_name(file, variable)

  if(all(file[[variable]] == 0 | is.na(file[[variable]])) ){

    file[[variable]] <- NA

  }

  return(file)
}




#' Multiply by household size
#'
#' `r lifecycle::badge("superseded")`
#' Multiplies the household weights ('hwgt') by the number of individuals in the
#'   household ('nhhmem'). It is used for computations at household level.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @return A list of tibbles with the modified 'hwgt' variable.
transform_weight_by_hh_size <- function(lissy_files){

  copy_attributes <- get_lissy_attributes(lissy_files)


  lissy_files <- purrr::imap(lissy_files, .f = function(file, file_name){

    assertthat::assert_that("hwgt" %in% names(file),
                            msg = glue::glue("'hwgt' could not be found in '{file_name}'."))

    assertthat::assert_that("nhhmem" %in% names(file),
                            msg = glue::glue("'nhhmem' could not be found in '{file_name}'."))


    file[["hwgt"]] <- file[["hwgt"]] * file[["nhhmem"]]

    return(file)

  })

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)

}



#' Import lisppp dataset
#'
#' `r lifecycle::badge("deprecated")`
#' Retrieves the lisppp deflators dataset.
#'
#' @param path_to_ppp_file A character string indicating where the deflator values
#'   can be found. If the value is 'lissyrtools', it will import the data from 'lissyrtools'.
#'   Specifying 'lissy' (default) will read them from within the LISSY directory. Any other
#'   value requires the full path specification to the deflators file.
#'
#' @return A tibble with the lisppp deflators.
#'
#' @keywords internal
import_lisppp_data <- function(path_to_ppp_file = "lissy"){

  if(path_to_ppp_file == "lissyrtools"){

    ppp_data <- lissyrtools::deflators

  }else if(path_to_ppp_file == "lissy"){

    assertthat::assert_that(is_lissy_machine(), msg = "path_to_ppp_file = 'lissy' can only be specified when using LISSY.")

    path_to_ppp_file <- paste0(INC_DIR, "ppp_2017.txt")

    ppp_data <- readr::read_delim(path_to_ppp_file,
                                  delim = ",",
                                  col_types = readr::cols(
                                    cname = readr::col_character(),
                                    iso2 = readr::col_character(),
                                    iso3 = readr::col_character(),
                                    year = readr::col_double(),
                                    cpi = readr::col_double(),
                                    ppp = readr::col_double(),
                                    lisppp = readr::col_double()
                                  ))

  }else{

    ppp_data <- readr::read_delim(path_to_ppp_file,
                                  delim = ",",
                                  col_types = readr::cols(
                                    cname = readr::col_character(),
                                    iso2 = readr::col_character(),
                                    iso3 = readr::col_character(),
                                    year = readr::col_double(),
                                    cpi = readr::col_double(),
                                    ppp = readr::col_double(),
                                    lisppp = readr::col_double()
                                  ))
  }

  ppp_data <- dplyr::select(ppp_data, iso2, lisppp, year)
  ppp_data <- dplyr::mutate(ppp_data, file = stringr::str_c(iso2, stringr::str_sub(year, -2, -1) ))
  ppp_data <- dplyr::select(ppp_data, file, lisppp)

  return(ppp_data)

}


#' Retrieve the reference year for LWS files
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param file_name String with the name of the dataset in one of the following formats:
#'   \enumerate{
#'   \item 'ccyy' Two digit country, two digit year. E.g. "fr84"
#'   \item 'ccyyyy' Two digit country, four digit year. E.g. "fr1984"
#'   \item 'ccyyl' Two digit country, two digit year, one digit level. E.g. "fr84h"
#'   \item 'ccyyyyl' Two digit country, four digit year, one digit household/person ('h' or 'p'). E.g. "fr1984h"
#'   \item 'ccyydl' Two digit country, two digit year, one digit indicating LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g. "fr84ih"
#'   \item 'ccyyyydl' Two digit country, two digit year,one digit indicating LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g. "fr1984ih"
#'     }
#'
#' @keywords internal
get_lws_file_income_reference_year <- function(file_name){

  if(!stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2,4}$")){

    file_name <- read_format_names(file_name)

  }

  country <- stringr::str_extract(file_name, pattern = "^\\w{2}")
  year_2d <- stringr::str_sub(stringr::str_match(file_name, pattern = "^\\w{2}(\\d{2,4})")[,2],
                              start = -2, end = -1)

  dname <- paste0(country, year_2d)

  assertthat::assert_that(dname %in% lissyrtools::data_inc_ref_year[["dname"]],
                          msg = glue::glue("{dname} was not found in lissyrtools::data_inc_ref_year."))

  reference_year <- lissyrtools::data_inc_ref_year[lissyrtools::data_inc_ref_year[["dname"]] == dname,
                                 "income_reference_year",
                                 drop = TRUE]

  return(reference_year)

}









#' Retreive the 'lisppp' deflator for a given file and variable.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Retrieves the 'lisppp' deflator for a file. It takes into account that the reference year
#' of income variables for LWS files might differ from the year of the file.
#'
#' @param file_name A sting with the file name with format 'ccyy'.
#' @param variable A string with the variable name. Defaults to NULL as it is
#'   optional for 'lis' files. It is required if 'database' = 'lws''.
#' @param income_variable Defaults to NULL. Is only relevant for LWS files. If the file is
#'   LWS and 'income_variable = TRUE', the function will retrieve the deflator for
#'   the year in which the income data was collected. This might not be the same
#'   as the year when wealth variables were collected.
#' @param database 'lis' or 'lws'.
#' @param ppp_data An optional file with the deflators. Should be in the same
#'   format as the tibble in 'lissyrtools::deflators'. If "lissyrtools" (default) the deflators
#'   are imported from the package internal data.
#' @return A numeric value with the'lisppp' deflator for the file
#' @keywords internal
#' 
#' 
get_file_lisppp <- function(file_name, database, variable = NULL, income_variable = NULL, ppp_data = "lissyrtools"){

  assertthat::assert_that(stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2}$"),
                          msg = "File name {file_name} is not in 'ccyy' format.")

  assertthat::assert_that(database %in% c('lis', 'lws', 'erflis', 'i', 'w', 'e'),
                          msg = "Incorrect value in 'database' argument.")


  if(is.character(ppp_data) && ppp_data == "lissyrtools"){
    ppp_data <- import_lisppp_data(path_to_ppp_file = "lissyrtools")
  }else{
    assertthat::assert_that(all(c("file", "lisppp") %in% names(ppp_data)),
                            msg = "The dataset with deflators passed to argument 'ppp_data' must have columns named 'file' and 'lisppp' from which the function will retreive the deflator.")

  }


  if(database %in% c("w", "lws")){

    assertthat::assert_that(!is.null(variable),
                            msg = "The argument 'variable' can not be NULL (default) if 'database = 'lws''")

    var_in_inc_vars <- variable %in% lissyrtools::lis_income_variables
    var_in_non_inc_vars <- variable %in% setdiff(lissyrtools::lws_variables, lissyrtools::lis_income_variables)

    if(!var_in_inc_vars & !var_in_non_inc_vars & is.null(income_variable)){
      stop(glue::glue("The function could not figure out if '{variable}' is an income variable or not. The variable was not found in lissyrtools::lws_income_variables nor in lissyrtools::lws_non_income_variables and the argument 'income_variable' was not specified. /n Specify 'income_variable'."))
    }

    if(is.null(income_variable)){income_variable <- NA}

    if((!is.na(income_variable) & income_variable) | (var_in_inc_vars & is.na(income_variable)) ){

      reference_year <- get_lws_file_income_reference_year(file_name = file_name)
      file_name <- paste0(stringr::str_sub(file_name, start = 1, end = 2), stringr::str_sub(reference_year, start = -2, end = -1))

    }



  }

  assertthat::assert_that(file_name %in% ppp_data[["file"]],
                          msg = glue::glue("File {file_name} wasn't found in PPP file"))

  lisppp <- ppp_data %>%
    dplyr::filter(file %in% file_name) %>%
    purrr::pluck("lisppp")

  return(lisppp)

}


#' Checks if the variable is 'household' or 'person' level.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Throws an error if it couldn't be checked.
#'
#' @param variable A String with the name of the variable. E.g. 'pi11', 'dhi'.
#'
#' @keywords internal
check_variable_level <- function(variable){

  is_household <- variable %in% c(lissyrtools::lis_household_variables,
                  lissyrtools::lws_household_variables,
                  lissyrtools::erflis_household_variables)

  is_person <- variable %in% c(lissyrtools::lis_person_variables,
                  lissyrtools::lws_person_variables,
                  lissyrtools::erflis_person_variables)

  if(is_household & !is_person){

    return("household")

  }else if(is_person & !is_household){

    return("person")

  } else if(is_person & is_household){

    stop("The variable does not have a level and it appears both in 'p' and 'h-level' files.")

  }

  stop("The variable level could not be guessed by matching the variable name with predefined lists of variables. Please specify the variable level manually.")

}


#' @rdname check_variable_level
safely_check_variable_level <- purrr::safely(check_variable_level)
