# warning_options.R

# Some warnings should only be triggered once per session.
#   Create options to manage these.



#' Missing values in variable warning
#'
#' Triggers a warning if there are missing values in 'variable' and the
#'   warning has not been triggered before.
#'
#' Uses an option to store the names of the files and variables with missing values
#'   that have already triggered a warning before. The option is stored as:
#'   '{file_name}_warning_NAs_{variable}'.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the variable name.
#'
#'
#' @return NULL
#' @keywords internal
missing_values_in_variable_warning <- function(file, file_name, variable){

  if(any(is.na(file[[variable]])) & is.null(rlang::peek_option(glue::glue("{file_name}_warning_NAs_{variable}"))) ){

    warning(glue::glue("The variable '{variable}' contains missing values in '{file_name}'."))

    define_option(option_name = glue::glue("{file_name}_warning_NAs_{variable}"),
                  value = TRUE)

  }

  return(NULL)

}
