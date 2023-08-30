# merge_dataset_levels.R



#' Merge household and person-level files
#'
#' Merges the household and person-level files of datasets.
#'
#' The level of the files in 'lissy_hfiles' and 'lissy_pfiles' is determined by
#'   the 'level' attribute of the files.
#'
#' @param lissy_hfiles A list with household-level files.
#' @param lissy_pfiles A list with person-level files.
#'
#' @return A list with the merged files.
merge_dataset_levels <- function(lissy_hfiles, lissy_pfiles){


  # the lists of files must have names.
  assertthat::assert_that(!is.null(names(lissy_hfiles)),
                          msg = "'lissy_hfiles' needs to be a named list containing h-level files.")

  assertthat::assert_that(!is.null(names(lissy_pfiles)),
                          msg = "'lissy_pfiles' needs to be a named list containing p-level files.")


  # both lists need to have an argument called 'level'
  assertthat::assert_that(!is.null(attr(lissy_hfiles, "level")),
                          msg = "Argument 'lissy_hfiles' needs to have a 'level' attribute.")

  assertthat::assert_that(!is.null(attr(lissy_pfiles, "level")),
                          msg = "Argument 'lissy_pfiles' needs to have a 'level' attribute.")


  # both lists need to have an argument called 'database'
  assertthat::assert_that(!is.null(attr(lissy_hfiles, "database")),
                          msg = "Argument 'lissy_hfiles' needs to have a 'database' attribute.")

  assertthat::assert_that(!is.null(attr(lissy_pfiles, "database")),
                          msg = "Argument 'lissy_pfiles' needs to have a 'database' attribute.")

  assertthat::assert_that(attr(lissy_hfiles, "database") == attr(lissy_pfiles, "database"),
                          msg = "'lissy_hfiles' and 'lissy_pfiles' should have the same value in 'database' attribute.")


  # lists can not be already merged
  assertthat::assert_that(!is.null(attr(lissy_hfiles, "merged_levels")) &&
                            attr(lissy_hfiles, "merged_levels") == FALSE &&
                            !is.null(attr(lissy_pfiles, "merged_levels")) &&
                            attr(lissy_pfiles, "merged_levels") == FALSE,
                          msg = "Arguments 'lissy_hfiles' and 'lissy_pfiles' need to have a 'merged_levels' argument equal to FALSE.")


  # both lists need to have the right value in 'level' attribute
  assertthat::assert_that(attr(lissy_hfiles, "level") == "h",
                          msg = "Argument 'lissy_hfiles' needs to contain household-level files and have attribute 'level' equal to 'h'.")

  assertthat::assert_that(attr(lissy_pfiles, "level") == "p",
                          msg = "Argument 'lissy_pfiles' needs to contain person-level files and have attribute 'level' equal to 'p'.")


  # both lists need to have valid values in 'database' attribute
  assertthat::assert_that( ( length(attr(lissy_hfiles, "database")) == 1 ) && (attr(lissy_hfiles, "database") %in% c("i", "e", "w")),
                           msg = "Unexpected value in 'database' attribute. Only 'i', 'e' and 'w' are accepted as valid.")

  assertthat::assert_that(attr(lissy_hfiles, "database") == attr(lissy_pfiles, "database"),
                          msg = "'lissy_hfiles' and 'lissy_pfiles' must have the same 'database' attribute value.")



  # all elements in both lists have the same format (e.g. 'ccyydl' or 'ccyyyydl')
  format_names_lissy_hfiles <- purrr::map_chr(names(lissy_hfiles), ~read_file_name_format(.x))
  format_names_lissy_pfiles <- purrr::map_chr(names(lissy_pfiles), ~read_file_name_format(.x))

  assertthat::assert_that( is.all.same.value(format_names_lissy_hfiles),
                           msg = "The format of 'lissy_hfiles' file names must be the same for all elements.")

  assertthat::assert_that( is.all.same.value(format_names_lissy_pfiles),
                           msg = "The format of 'lissy_pfiles' file names must be the same for all elements.")

  assertthat::assert_that(unique(format_names_lissy_hfiles) == unique(format_names_lissy_pfiles),
                          msg = "The format of file names in 'lissy_hfiles' must be the same as that in 'lissy_pfiles'.")

  assertthat::assert_that( unique(format_names_lissy_hfiles) %in% c('ccyydl', 'ccyyyydl'),
                          msg = "The format of file names in 'lissy_hfiles' and 'lissy_pfiles' must be either 'ccyydh' or 'ccyyyydh'.")


  # names of files are the same
  check_file_lists_contain_same_datasets(lissy_hfiles, lissy_pfiles)

  # sort lissy_pfiles to have the same order as lissy_hfiles
  lissy_pfiles <- sort_lissy_list(list_to_sort = lissy_pfiles,
                                  according_to = names(lissy_hfiles))

  # read 'database' attribute
  database_ <- attr(lissy_hfiles, "database")

  if(database_ %in% c("i", "e")){

    lissy_mfiles <- purrr::pmap(.l = list(hfile = lissy_hfiles,
                          pfile = lissy_pfiles,
                          hfile_name = names(lissy_hfiles),
                          pfile_name = names(lissy_pfiles)),
                .f = merge_lis_dataset_levels)

  }else if(database_ %in% c("w")){

    lissy_mfiles <- purrr::pmap(.l = list(hfile = lissy_hfiles,
                                          pfile = lissy_pfiles,
                                          hfile_name = names(lissy_hfiles),
                                          pfile_name = names(lissy_pfiles)),
                                .f = merge_lws_dataset_levels)

  }


  names(lissy_mfiles) <- purrr::map(names(lissy_mfiles),
                                    ~change_file_name_format(.x, to_format = stringr::str_sub(unique(format_names_lissy_hfiles), 1, -2))) # removes 'l' from format

  attr(lissy_mfiles, "database") <- database_
  attr(lissy_mfiles, "level") <- "p" # it is, in the end, a 'person-level' dataset.
  attr(lissy_mfiles, "merged_levels") <- TRUE

  return(lissy_mfiles)

}



#' Merges a hfile with its pfile
#'
#' To be used only within 'merge_dataset_levels()'
#'
#' @keywords internal
merge_lis_dataset_levels <- function(hfile, pfile, hfile_name, pfile_name){


  ## hfiles and pfiles need to contain 'hid'
  assertthat::assert_that("hid" %in% names(hfile),
                          msg = "'hfile' should contain 'hid' column.")

  assertthat::assert_that("hid" %in% names(pfile),
                          msg = "'pfile' should contain 'hid' column.")

  # check 'hid's
  if( !all( hfile[["hid"]] %in% pfile[["hid"]] ) ){
    warning(glue::glue("Not all 'hid's in '{hfile_name}' appear in '{pfile_name}'."))
  }

  if( !all( pfile[["hid"]] %in% hfile[["hid"]]  ) ){
    warning(glue::glue("Not all 'hid's in '{pfile_name}' appear in '{hfile_name}'."))
  }

  # common variables are dropped from pfile
  variables_to_drop <- intersect(names(hfile), names(pfile))
  variables_to_drop <- variables_to_drop[variables_to_drop != "hid"]

  output_ <- dplyr::inner_join(hfile,
                               dplyr::select(pfile, -tidyselect::one_of(variables_to_drop)),
                               by = "hid")

  return(output_)

}




#' @rdname merge_lis_dataset_levels
merge_lws_dataset_levels <- function(hfile, pfile, hfile_name, pfile_name){

  ## hfiles and pfiles need to contain 'hid' and 'inum'
  assertthat::assert_that("hid" %in% names(hfile),
                          msg = "'hfile' should contain 'hid' column.")

  assertthat::assert_that("hid" %in% names(pfile),
                          msg = "'pfile' should contain 'hid' column.")

  assertthat::assert_that("inum" %in% names(hfile),
                          msg = "'hfile' should contain 'inum' column.")

  assertthat::assert_that("inum" %in% names(pfile),
                          msg = "'pfile' should contain 'inum' column.")

  # check 'hid's and 'hid+inum'
  if( !all( hfile[["hid"]] %in% pfile[["hid"]] ) ){
    warning(glue::glue("Not all 'hid's in '{hfile_name}' appear in '{pfile_name}'."))
  }

  if( !all( pfile[["hid"]] %in% hfile[["hid"]]  ) ){
    warning(glue::glue("Not all 'hid's in '{pfile_name}' appear in '{hfile_name}'."))
  }

  if( !all( stringr::str_c(hfile[["hid"]], hfile[["inum"]], sep = "_") %in% stringr::str_c(pfile[["hid"]], pfile[["inum"]], sep = "_") ) ){
    warning(glue::glue("Not all combinations of 'inum' and 'hid's in '{hfile_name}' appear in '{pfile_name}'."))
  }

  if( !all( stringr::str_c(pfile[["hid"]], pfile[["inum"]], sep = "_") %in% stringr::str_c(hfile[["hid"]], hfile[["inum"]], sep = "_" ) ) ){
    warning(glue::glue("Not all combinations of 'inum' and 'hid's in '{pfile_name}' appear in '{hfile_name}'."))
  }

  # common variables are dropped from pfile
  variables_to_drop <- intersect(names(hfile), names(pfile))
  variables_to_drop <- variables_to_drop[!variables_to_drop %in% c("hid", "inum")]

  output_ <- dplyr::inner_join(hfile,
                               dplyr::select(pfile, -tidyselect::one_of(variables_to_drop)),
                               by = c("inum", "hid"))

  return(output_)

}




#' Checks if two lists have the same datasets
#'
#' To be used only within 'merge_dataset_levels()'
#'
#' @keywords internal
check_file_lists_contain_same_datasets <- function(lissy_hfiles, lissy_pfiles){

  hfiles_sorted_names <- sort(purrr::map_chr(names(lissy_hfiles), ~change_file_name_format(.x, "ccyyyy")))
  pfiles_sorted_names <- sort(purrr::map_chr(names(lissy_pfiles), ~change_file_name_format(.x, "ccyyyy")))


  if(!identical(hfiles_sorted_names, pfiles_sorted_names)
  ){

    hfiles_sorted_names <- sort(purrr::map_chr(names(lissy_hfiles), ~change_file_name_format(.x, "ccyyyy")))
    pfiles_sorted_names <- sort(purrr::map_chr(names(lissy_pfiles), ~change_file_name_format(.x, "ccyyyy")))

    if(!all(hfiles_sorted_names %in% pfiles_sorted_names)){

      missing_hfiles <- stringr::str_c(hfiles_sorted_names[!hfiles_sorted_names %in% pfiles_sorted_names], collapse = ", ")
      stop(glue::glue("Arguments 'lissy_hfiles' and 'lissy_pfiles' need to contain files from the same datasets. The following datasets were found in 'lissy_pfiles' but not in 'lissy_hfiles' argument names: {missing_hfiles}") )

    }else if(!all(pfiles_sorted_names %in% hfiles_sorted_names)){

      missing_pfiles <- stringr::str_c(pfiles_sorted_names[!pfiles_sorted_names %in% hfiles_sorted_names], collapse = ", ")
      stop(glue::glue("Arguments 'lissy_hfiles' and 'lissy_pfiles' need to contain files from the same datasets. The following datasets were found in 'lissy_hfiles' but not in 'lissy_pfiles' argument names: {missing_pfiles}") )

    }else{

      stop("Unexpected error")

    }

  }

  return(TRUE)

}



#' Sort a list of lissy files
#'
#' To be used only within 'merge_dataset_levels()'
#'
#' Sorts a list of lissy household or person-level files according to the
#'   names passed in argument 'according_to'.
#'
#' @param list_to_sort A list with pfiles or hfiles. Names of these files should be in 'ccyyyydl' format.
#' @param according_to A character vector with names on which the list should be sorted.
#'   These can be in any format that can be transformed to 'ccyyyyd'.
#'
#' @keywords internal
sort_lissy_list <- function(list_to_sort, according_to){

  format_names_list_to_sort <- purrr::map_chr(names(list_to_sort), ~read_file_name_format(.x))

  assertthat::assert_that( all(format_names_list_to_sort %in% c('ccyyyydl', 'ccyydl') ),
                          msg = "All elements in 'list_to_sort' need to be in 'ccyyyydl' or 'ccyydl' formats.")

  assertthat::assert_that( length(unique(format_names_list_to_sort)) == 1,
                          msg = "All elements in 'list_to_sort' need to be in the same format.")


  # names_to_sort <- change_file_name_format(file_names = according_to,
  #                         to_format = 'ccyyyyd')

  names_to_sort <- change_file_name_format(file_names = according_to,
                                           to_format = stringr::str_sub(unique(format_names_list_to_sort), 1, -2) )

  list_to_sort_level <- extract_information_from_file_name(names(list_to_sort)[1],
                                     lissyrtools::read_file_name_format(names(list_to_sort)[1]))[["level_"]] # this won't work if level is not the same for all elements


  names_to_sort <- stringr::str_c(names_to_sort, list_to_sort_level)

  return(list_to_sort[names_to_sort])

}
