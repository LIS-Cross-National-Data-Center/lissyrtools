# compute_functions_helpers.R

#' Perform checks for 'compute' functions.
#'
#' `r lifecycle::badge("deprecated")`
#' Perform checks within 'compute_*()' functions. The checks performed are:
#'   \itemize{
#'   \item The file is a data.frame type of object.
#'   \item 'varaible' is a column in file
#'   \item 'variable' is a numeric variable.
#'   \item The weight variable is a column in file.
#'   \item 'weight' variable contains valid values (i.e. different than NAs and 0s).
#'     }
#'
#' The checks related to the 'weight' variable are only performed if argument
#'   'weight' is not NULL.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the name of the variable for which checks should be computed.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights. If NULL (default), checks for weight variable are not performed.
#'
#' @keywords internal
checks_compute_functions <- function(file, file_name, variable, weight = NULL){

  assertthat::assert_that(is.data.frame(file),
                          msg = glue::glue("Argument 'file' in '{file_name}' needs to be a data.frame/tibble type of object."))

  assertthat::assert_that(nrow(file) > 0,
                          msg = glue::glue("Argument 'file' in '{file_name}' should be have 1 or more rows."))

  assertthat::assert_that(variable %in% names(file),
                          msg = glue::glue("'{variable}' was not found in '{file_name}'."))

  assertthat::assert_that(is.numeric(file[[variable]]),
                          msg = glue::glue("The argument 'variable' needs to be numeric. '{variable}' in '{file_name}' is not numeric."))


  if(!is.null(weight)){

    assertthat::assert_that(weight %in% names(file),
                            msg = glue::glue("'{weight}' was not found in '{file_name}'."))


    assertthat::assert_that(!is.all.na.or.zero(file[[weight]]),
                            msg = glue::glue("The variable for weights ('{weight}') contains only NAs and/or 0s. The equivalised percentiles could not be computed."))
  }

  return(NULL)

}
