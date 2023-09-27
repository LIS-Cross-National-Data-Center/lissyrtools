# transform_subset.R


#' Subset a sample by age
#'
#' \lifecycle{experimental}
#' @description
#' 'transform_subset_age()' subsets the rows in a LIS/LWS file so all cases have
#'   ages between two values (both included), returning a list with the files
#'   containing only rows where the age is between those values. I.e. a subset
#'   of rows.
#' It can also be called with 'transform_filter_age()'.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param from An integer with the lower boundary value (included in the sample).
#' @param to An integer with the higher boundary value (included in the sample).
#'
#' @return A list of tibbles with the subset of rows.
transform_subset_age <- function(lissy_files, from, to) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_filter_age(
      file = ..1,
      file_name = ..2,
      from = ..3,
      to = ..4
    ),
    from, to
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)


  return(lissy_files)
}

#' @rdname
transform_filter_age <- transform_subset_age


#' Subset a sample by age
#'
#' \lifecycle{experimental}
#' @description
#' 'transform_restrict_age()' recodes the indicated variable to NA when age is
#'   outside the boundaries. Cases where age is NA are also recoded to NA.
#' To remove rows outside of age boundaries use 'transform_subset_age()' instead.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be adjusted.
#'
#' @return A list of tibbles with the adjusted variable.
transform_restrict_age <- function(lissy_files, variable, from, to) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_restrict_age(
      file = ..1,
      file_name = ..2,
      variable = ..3,
      from = ..4,
      to = ..5
    ),
    variable, from, to
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
}


#' Apply 'transform_filter_age()' to a single file
#'
#' \lifecycle{experimental}
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
implement_filter_age <- function(file, file_name, from, to) {
  assertthat::assert_that("age" %in% names(file),
    msg = glue::glue("'age' is not a variable in '{file_name}'.")
  )

  return(
    dplyr::filter(file, !is.na(age) & dplyr::between(age, from, to))
  )
}


#' Apply 'transform_restrict_age()' to a single file
#'
#' \lifecycle{experimental}
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
implement_restrict_age <- function(file, file_name, variable, from, to) {
  assertthat::assert_that("age" %in% names(file),
    msg = glue::glue("'age' is not a variable in '{file_name}'.")
  )
  assertthat::assert_that(variable %in% names(file),
    msg = glue::glue("'{variable}' is not a variable in '{file_name}'.")
  )

  file[((!dplyr::between(file[["age"]], from, to)) | is.na(file[["age"]])), variable] <- NA

  return(file)
}


#' Subset a sample by household heads
#'
#' @description
#' Recodes the specified variable to NA for non-household heads.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param variable A character string with the name of the variable that should
#'   be recoded for non-household heads.
#'
#' @return A list of tibbles with the adjusted variable.
transform_restrict_to_household_heads <- function(lissy_files, variable) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_restrict_to_household_heads(
      file = ..1,
      file_name = ..2,
      variable = ..3
    ),
    variable
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
}

#' Apply 'transform_restrict_to_household_heads()' to a single file
#'
#' \lifecycle{experimental}
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
implement_restrict_to_household_heads <- function(file, file_name, variable) {
  assertthat::assert_that("relation" %in% names(file),
    msg = glue::glue("'relation' is not a variable in '{file_name}'.")
  )

  assertthat::assert_that(variable %in% names(file),
    msg = glue::glue("'{variable}' is not a variable in '{file_name}'.")
  )

  assertthat::assert_that(any(file[["relation"]] == 1000),
    msg = glue::glue("None of the values in 'relation' variable were equal to 1000 in '{file_name}'.")
  )

  file[(is.na(file[["relation"]]) | file[["relation"]] != 1000), variable] <- NA

  return(file)
}

#' Subset a sample by house ownership
#'
#' \lifecycle{experimental}
#' @description
#' 'transform_subset_ownership()' subsets the rows in a LIS/LWS file so all cases have
#'   house ownership status equal to one of the specified values, returning a list with the files
#'   containing only rows where the house ownership status is equal to one of the specified values. I.e. a subset
#'   of rows.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param ownership_status_categories A character vector with the house ownership status categories that should be included in the sample.
#'
#' @return A list of tibbles with the subset of rows.
transform_subset_ownership <- function(lissy_files, ownership_status_categories) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_subset_ownership(
      file = ..1,
      file_name = ..2,
      ownership_status_categories = ..3
    ),
    ownership_status_categories
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
}

#' @rdname transform_subset_ownership
transform_filter_ownership <- transform_subset_ownership

#' Restrict a sample by house ownership
#'
#' \lifecycle{experimental}
#' @description
#' 'transform_restrict_ownership()' recodes the indicated variable to NA when house ownership status is
#'   not equal to 1000. Cases where house ownership status is NA are also recoded to NA.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param variable A character string with the name of the variable that should be adjusted.
#'
#' @return A list of tibbles with the adjusted variable.
transform_restrict_ownership <- function(lissy_files, variable) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_restrict_ownership(
      file = ..1,
      file_name = ..2,
      variable = ..3
    ),
    variable
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
}

#' Apply 'transform_subset_ownership()' to a single file
#'
#' \lifecycle{experimental}
#' Applies 'transform_subset_ownership()' to a single LIS/LWS file.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param ownership_status_categories A character vector with the house ownership status categories that should be included in the sample.
#'
#' @return A a file with the filtered house ownership status variable.
#'
#' @keywords internal
implement_subset_ownership <- function(file, file_name, ownership_status_categories) {
  assertthat::assert_that("own" %in% names(file),
    msg = glue::glue("'own' is not a variable in '{file_name}'.")
  )

  return(
    dplyr::filter(file, own %in% ownership_status_categories)
  )
}

#' Apply 'transform_restrict_ownership()' to a single file
#'
#' \lifecycle{experimental}
#' Applies 'transform_restrict_ownership()' to a single LIS/LWS file.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character vector of length one with the variable that
#'   will be recoded to NA when house ownership status is not equal to 1000.
#'
#' @return A a file with the filtered house ownership status variable.
#'
#' @keywords internal
implement_restrict_ownership <- function(file, file_name, variable) {
  assertthat::assert_that("own" %in% names(file),
    msg = glue::glue("'own' is not a variable in '{file_name}'.")
  )
  assertthat::assert_that(variable %in% names(file),
    msg = glue::glue("'{variable}' is not a variable in '{file_name}'.")
  )

  file[(is.na(file[["relation"]]) | file[["relation"]] != 1000), variable] <- NA

  return(file)
}



#' Select cases based on age
#'
#' Should only work for individual datasets


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



#' Subset a sample using a categorical variable
#'
#' \lifecycle{experimental}
#' @description
#' transform_subset_variable_categorical()' subsets the rows in a LIS/LWS file so all cases
#' have values between two values (both included), returning a list with the
#' files containing only rows where the variable is between those values. I.e.
#' a subset of rows.
#' It can also be called with 'transform_filter_variable()'.
#'
#' @details
#' The function needs an argument either for 'categories' or 'from' and 'to'.
#' If 'categories'
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param variable A character string with the name of the variable that should
#'  be adjusted.
#' @param categories A character vector with the categories that should be
#' included in the sample.
#' @param from An integer with the lower boundary value (included in the sample).
#' @param to An integer with the higher boundary value (included in the sample).
#'
#' @return A list of tibbles with the subset of rows.
transform_subset_variable_categorical <- function(lissy_files, variable, categories = NULL) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_filter_variable(
      file = ..1,
      file_name = ..2,
      variable = ..3,
      categories = ..4
    ),
    variable, categories
  )



  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
}


#' Subset a sample using a categorical variable
#'
#' \lifecycle{experimental}
#' @description
#' 'implement_subset_variable_categorical()' subsets the rows in a LIS/LWS file so all cases
#' have values in the specified categories, returning a tibble with only the
#' rows where the variable is in those categories.
#' It is intended to be used inside 'transform_subset_variable_categorical()'.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character string with the name of the variable that should
#'  be adjusted.
#' @param categories A character vector with the categories that should be
#' included in the sample.
#'
#' @return A tibble with the subset of rows.
#'
#' @keywords internal
implement_subset_variable_categorical <- function(file, file_name, variable, categories) {
  assertthat::assert_that(variable %in% names(file),
    msg = glue::glue("'{variable}' is not a variable in '{file_name}'.")
  )

  return(file[file[[variable]] %in% categories, ])
}


#' Subset a sample using a continuous variable
#'
#' \lifecycle{experimental}
#' @description
#' 'transform_subset_variable_continuous()' subsets the rows in a LIS/LWS file so all cases
#' have values between two values (both included), returning a list with the
#' files containing only rows where the variable is between those values. I.e.
#' a subset of rows.
#' It can also be called with 'transform_filter_variable()'.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param variable A character string with the name of the variable that should
#'  be adjusted.
#' @param from A numeric value with the lower boundary value (included in the sample).
#' @param to A numeric value with the higher boundary value (included in the sample).
#'
#' @return A list of tibbles with the subset of rows.
transform_subset_variable_continuous <- function(lissy_files, variable, from, to) {
  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(
    .x = lissy_files,
    .f = ~ implement_filter_variable(
      file = ..1,
      file_name = ..2,
      variable = ..3,
      from = ..4,
      to = ..5
    ),
    variable, from, to
  )

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)
}


#' Subset a sample using a continuous variable
#'
#' \lifecycle{experimental}
#' @description
#' 'implement_subset_variable_continuous()' subsets the rows in a LIS/LWS file so all cases
#' have values between two values (both included), returning a tibble with only the
#' rows where the variable is between those values.
#' It is intended to be used inside 'transform_subset_variable_continuous()'.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character string with the name of the variable that should
#'  be adjusted.
#' @param from A numeric value with the lower boundary value (included in the sample).
#' @param to A numeric value with the higher boundary value (included in the sample).
#'
#' @return A tibble with the subset of rows.
#'
#' @keywords internal
implement_subset_variable_continuous <- function(file, file_name, variable, from, to) {
  assertthat::assert_that(variable %in% names(file),
    msg = glue::glue("'{variable}' is not a variable in '{file_name}'.")
  )

  return(file[dplyr::between(file[[variable]], from, to), ])
}
