#' Bind multiple LIS or LWS datasets
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' Converts a list of LIS or LWS datasets into a single file. Creates a new 'file' variable with the
#'   name of the dataset and computes unique person and household
#'   identifiers if 'create_unique_id = TRUE' (default).
#'
#' Adds the attribute 'binded = TRUE' to the resulting tibble.
#'
#' @param lissy_files A list of LIS or LWS files.
#' @param create_unique_id Computes new variables 'unique_hid' and 'unique_pid'
#'   as unique row identifiers. Defaults to TRUE.
#' @return A tibble with the stacked LIS or LWS files.
#' @examples
#' \dontrun{
#' lissy_datasets_as_list <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#'  lissy_datasets_as_tibble <- bind_lissy_files(lissy_datasets = lissy_datasets_as_list, create_unique_id = TRUE)
#' }
bind_lissy_files <- function(lissy_files, create_unique_id = TRUE){

  copy_attributes <- get_lissy_attributes(lissy_files)

  if(create_unique_id){

    lissy_files <- purrr::imap(lissy_files,
                               ~add_unique_ids(file = ..1,
                                               file_name = ..2,
                                               file_level = ..3),
                               get_lissy_attributes(lissy_files)[["level"]] #..3 not iterated
                               )

  }

  binded_dataset <- dplyr::bind_rows(lissy_files, .id = "file")

  binded_dataset <- paste_lissy_attributes(binded_dataset, copy_attributes)

  attr(binded_dataset, "binded") <- TRUE

  .Deprecated("purrr::list_rbind", package = "lissyrtools")
  return(binded_dataset)

}




#' Add unique ID variables when binding LIS or LWS files.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param file_level A string indicating the level of the file. Valid inputs are:
#'   'household', 'h', 'person' or 'p'.
#'
#'
#' @keywords internal
add_unique_ids  <- function(file, file_name, file_level){

  assertthat::assert_that(file_level %in% c("person", "household", "p", "h"),
                          msg = "Argument 'file_level' in can only take 'person', 'p', 'household' or 'h' as values.")

  assertthat::assert_that(all(c("did", "hid") %in% names(file)),
                          msg = glue::glue("If 'create_unique_id' is set to TRUE, all collapsed datasets must contain variables 'did', 'hid'. \n This is not the case of '{file_name}'."))

  file[["unique_hid"]] <- stringr::str_c(file[["did"]], file[["hid"]], sep = "_")


  if(file_level %in% c("person", "p")){

    assertthat::assert_that("pid" %in% names(file),
                            msg = glue::glue("If 'create_unique_id' is set to TRUE and the files are not at household level, all collapsed datasets must contain the variable 'pid'. \n This is not the case of '{file_name}'."))

    file[["unique_pid"]] <- stringr::str_c(file[["did"]], file[["pid"]], sep = "_")

  }

  return(file)

}
