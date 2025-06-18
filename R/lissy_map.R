# lissy_map.R

#' Wrap over 'purrr::imap()' for lissy files
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' A wrap over 'purrr::imap()' that keeps the lissy attributes on the output
#'   list.
#'
#' @param lissy_files A list of LIS or LWS p-level files.
#' @param .f A function, formula, or vector (not necessarily atomic) as in
#'   the argument from purrr::map with the same name.
#'
#' @return A list like lissy_files
lissy_map <- function(lissy_files, .f){

  copy_attributes <- get_lissy_attributes(lissy_files)

  lissy_files <- purrr::imap(.x = lissy_files,
                             .f = .f)

  lissy_files <- paste_lissy_attributes(lissy_files, copy_attributes)

  return(lissy_files)

}
