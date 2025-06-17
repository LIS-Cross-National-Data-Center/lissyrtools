# transform_subset.R


#' Filter a sample by age
#'
#' `r lifecycle::badge("deprecated")`
#' @description
#' 'transform_filter_age()' filter the rows in a LIS/LWS file so all cases have
#'   ages between two values (both included), returning a list with the files
#'   containing only rows where the age is between those values. I.e. a subset
#'   of rows.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param from An integer with the lower boundary value (included in the sample).
#' @param to An integer with the higher boundary value (included in the sample).
#'
#' @return A list of tibbles with the subset of rows.
transform_filter_age <- function(lissy_files, from, to){

  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~ implement_filter_age(file = ..1,
                                                         file_name = ..2,
                                                         from = ..3,
                                                         to = ..4),
                             from, to)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)

}




#' Filter a variable by age
#'
#' `r lifecycle::badge("deprecated")`
#' @description
#' 'transform_restrict_age()' recodes the indicated variable to NA when age is
#'   outside the boundaries. Cases where age is NA are also recoded to NA.
#' To remove rows outside of age boundaries use 'transform_filter_age()' instead.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be adjusted.
#'
#' @return A list of tibbles with the adjusted variable.
transform_restrict_age <- function(lissy_files, variable, from, to){

  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~ implement_restrict_age(file = ..1,
                                                           file_name = ..2,
                                                           variable = ..3,
                                                           from = ..4,
                                                           to = ..5),
                             variable, from, to)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)

}


#' Apply 'transform_filter_age()' to a single file
#'
#' `r lifecycle::badge("deprecated")`
#' Applies 'transform_filter_age()' to a single LIS/LWS file.
#'
#' To be used inside 'transform_filter_age()' and 'transform_restrict_age()'.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param from An integer with the lower boundary value (included in the sample).
#' @param to An integer with the higher boundary value (included in the sample).
#'
#' @return A a file with the filtered age variable.
#'
#' @keywords internal
implement_filter_age <- function(file, file_name, from, to){

  assertthat::assert_that("age" %in% names(file),
                          msg = glue::glue("'age' is not a variable in '{file_name}'."))

  return(
    dplyr::filter(file, !is.na(age) & dplyr::between(age, from, to))
  )

}


#' Apply 'transform_restrict_age()' to a single file
#'
#' `r lifecycle::badge("deprecated")`
#' Applies 'transform_restrict_age()' to a single LIS/LWS file.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character vector of length one with the variable that
#'   will be recoded to NA when age is outside the boundaries (both included).
#' @param from An integer with the lower boundary value (included in the sample).
#' @param to An integer with the higher boundary value (included in the sample).
#'
#' @return A a file with the filtered age variable.
#'
#' @keywords internal
implement_restrict_age <- function(file, file_name, variable, from, to){

  assertthat::assert_that("age" %in% names(file),
                          msg = glue::glue("'age' is not a variable in '{file_name}'."))
  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("'{variable}' is not a variable in '{file_name}'."))

  file[((!dplyr::between(file[["age"]], from, to)) | is.na(file[["age"]])), variable] <- NA

  return(file)

}


#' Filter a variable for household heads
#'
#' @description
#' Recodes the specified variable to NA for non-household heads.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param variable A character string with the name of the variable that should
#'   be recoded for non-household heads.
#'
#' @return A list of tibbles with the adjusted variable.
transform_restrict_to_household_heads <- function(lissy_files, variable){

  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = ~ implement_restrict_to_household_heads(file = ..1,
                                                           file_name = ..2,
                                                           variable = ..3),
                             variable)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)

}


#' Apply 'transform_restrict_to_household_heads()' to a single file
#'
#' `r lifecycle::badge("deprecated")`
#' Applies 'transform_restrict_to_household_heads()' to a single LIS/LWS file.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character vector of length one with the variable that
#'   will be recoded to NA when relation is missing or not equal to 1000 (household head).
#'
#' @return A a file with the filtered age variable.
#'
#' @keywords internal
implement_restrict_to_household_heads <- function(file, file_name, variable){

  assertthat::assert_that("relation" %in% names(file),
                          msg = glue::glue("'relation' is not a variable in '{file_name}'."))

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("'{variable}' is not a variable in '{file_name}'."))

  assertthat::assert_that(any(file[["relation"]] == 1000),
                          msg = glue::glue("None of the values in 'relation' variable were equal to 1000 in '{file_name}'."))

  file[(is.na(file[["relation"]]) | file[["relation"]] != 1000), variable] <- NA

  return(file)

}



#' Select cases based on house ownership
#'


#' Select cases based on age
#'
#'Should only work for individual datasets


#' Select cases based on sex



#' Select only household heads
#'
#'


#' Select on household type
#'


#' Select based on education
#'


#' Select based on immigration
#'


#' Select based on urban or rural
#'


#' Select based on income percentile
#'


