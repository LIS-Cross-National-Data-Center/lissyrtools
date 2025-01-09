#' Retrieve names of key variables
#'
#' \lifecycle{experimental}
#' Retrieve the names of key variables for a given database and level. To be
#'   used only within 'read_lissy_files()' and 'read_lissy_files_locally()'.
#'
#' @param database A string indicating the database of the key variables to be
#'   retrieved (e.g. 'lis', 'i', 'lws', 'w')
#' @param level A string indicating the level of the key variables to be
#'   retrieved (e.g. 'person', 'p', 'household', 'h')
#'
#' @return A character vector with the names of key variables for a given
#'   database and level.
#'
#' @keywords internal
retrieve_names_key_variables <- function(database, level){

  if(database %in% c("lis", "erflis", "i", "e")){

    if(level %in% c("person", "p")){

      return(lissyrtools::key_vars_person_lis)

    }else if(level %in% c("household", "h")){

      return(lissyrtools::key_vars_household_lis)

    }

  }else if(database %in% c("lws", "w")){

    if(level %in% c("person", "p")){

      return(lissyrtools::key_vars_person_lws)

    }else if(level %in% c("household", "h")){

      return(lissyrtools::key_vars_household_lws)

    }
  }

  stop(glue::glue("No key variables were found for database '{database}' and level '{level}'."))

}



#' Read files in LISSY
#'
#' \lifecycle{experimental}
#' Reads multiple LIS, LWS or ERFLIS files in the LISSY interface.
#'
#' All files need to be from the same database (e.g. LIS, LWS) and level
#' (e.g. 'household', 'person') Computes the full year with four digits by default.
#'
#' @param files A character vector containing file names in 'ccyyl' format.
#'   All files need to be at the same level (i.e. 'household', 'person').
#'   E.g. c("it14h", "us16h")
#' @param col_select A character vector with the name of the variables
#'   which should be selected from the files. E.g. c("hid", "dhi", "hifactor").
#' @param full_year_names A boolean. Should the name of the imported file be changed to 'ccyyyyl' format.
#' @return A named list with the loaded files.
#' @examples
#' \dontrun{
#' read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' }
read_lissy_files <- function(files, col_select = NULL, full_year_names = TRUE){

  files <- tolower(files)

  ## Check that argument 'files' is correct

  ## has a level
  assertthat::assert_that(all(stringr::str_detect(files, pattern = "\\w{2}\\d{2,4}([iwe])?[hpr]")),
                          msg = "Argument 'files' should specify file names in 'ccyyl' format.")


  ## 'database' is unique
  database_ <- unique(purrr::map_chr(files, ~extract_information_from_file_name(file_name =.x,
                                                            current_format = read_file_name_format(.x))[["database_"]]))

  assertthat::assert_that(is.all.same.value(database_),
                          msg = "All files need to be from the same database.")

  ## 'database' matches that in directory
  database_char <- retrieve_database_from_directory()

  if(database_ != "" && database_ != database_char){

    database_files <- database_character_to_name(database_)
    database_dir <- database_character_to_name(database_char)

    stop(glue::glue("Database element in the environment ('{database_dir}') needs to be the same as in file names ('{database_files}')."))

  }

  ## 'level' is unique
  level_ <- unique(purrr::map_chr(files, ~ extract_information_from_file_name(file_name = .x,
                                                                              current_format = "ccyyl")[["level_"]]))

  assertthat::assert_that(is.all.same.value(level_),
                          msg = "All files need to have the same level. If you are trying to read files from multiple levels,
         please use different calls of 'read_lissy_files()'.")

  # define 'vars' if 'col_select' is not NULL
  if(!is.null(col_select)){

  vars <- c(col_select,
            retrieve_names_key_variables(database = database_char,
                                         level = level_))

  }else{

    vars <- NULL

  }


  ## if files are not in 'ccyyl' format, try to convert them.
  if(!purrr::every(files, ~read_file_name_format(.x) == "ccyyl")){
    files <- purrr::map_chr(files, ~change_file_name_format(.x, to_format = "ccyyl"))
  }

  files <- stringr::str_c(stringr::str_sub(files, 1, 4), # 'ccyy'
                          database_char, # 'd'
                          stringr::str_sub(files, 5)) # 'l'


  output_list <- purrr::map(files, function(file, files_directory, col_select){

    read_LIS(LIS_DIR = files_directory, file_name = file, col_select = col_select)

    }, get_directory(), # files_directory
    vars # col_select
    )


  ## add attributes
  attr(output_list, "merged_levels") <- FALSE
  attr(output_list, "level") <- level_
  attr(output_list, "database") <- database_char


  if(full_year_names){

  names(output_list) <- purrr::map_chr(files, ~change_file_name_format(.x, 'ccyyyydl'))


  }else{

    names(output_list) <- files

  }

  return(output_list)

}





#' Read files locally
#'
#' \lifecycle{experimental}
#' Reads multiple LIS or LWS files outside of the LISSY interface.
#'
#' @param files A character vector containing file names. These need to contain
#'   the country, year, database and level elements at the beginning of their name in 'ccyydl', or 'ccyyyydl' format.
#'   E.g. c("it14ih", "us16ih") or c("it14ih_modified.dta", "us16ih_mydata.dta")
#' @param path_to_files A character string with the directory in which the files can be found.
#'   The order should match that of the files passed in 'files'.
#' @param col_select A character vector with the name of the variables
#'   which should be selected from the files. E.g. c("hid", "dhi", "hifactor").
#' @param full_year_names A boolean. Should the name of the imported file be changed to 'ccyyyyl' format.
#' @return A named list with the loaded files.
#' @examples
#' \dontrun{
#' read_lissy_files_locally(c("fr84h", "fr94h", "fr10h"), path_to_files = "/home/user/files/")
#' }
read_lissy_files_locally <- function(files, path_to_files, col_select = NULL, full_year_names = TRUE){

  files <- tolower(files)


  ## has level and database elements
  assertthat::assert_that(purrr::every(files, ~stringr::str_detect(.x, pattern = "\\w{2}\\d{2,4}[iwe][hpr]")),
                          msg = "Argument 'files' should specify file names in 'ccyydl' or 'ccyyyydl' format.")


  ## 'database' is unique
  file_name_elements <- stringr::str_extract(files, pattern = "\\w{2}(\\d{2}|\\d{4})[iwe][hpr]")

  database_ <- unique(purrr::map_chr(file_name_elements, ~extract_information_from_file_name(file_name =.x,
                                                                                current_format = read_file_name_format(.x))[["database_"]]))

  assertthat::assert_that(is.all.same.value(database_),
                          msg = "All files need to be from the same database.")


  ## 'level' is unique
  level_ <- unique(purrr::map_chr(file_name_elements, ~ extract_information_from_file_name(file_name = .x,
                                                                              current_format = "ccyyl")[["level_"]]))

  assertthat::assert_that(is.all.same.value(level_),
                          msg = "All files need to have the same level. If you are trying to read files from multiple levels, please use different calls of 'read_lissy_files_locally()'.")

  # define 'vars' if 'col_select' is not NULL
  if(!is.null(col_select)){

    vars <- c(col_select,
              retrieve_names_key_variables(database = database_,
                                           level = level_))

  }else{

    vars <- NULL

  }

  ## compute ccyydl element
  files_ccyydl <- purrr::map_chr(stringr::str_extract(file_name_elements, pattern = "^\\w{2}(\\d{4}|\\d{2})[iwe][hpr]"),
                                 ~change_file_name_format(.x, to_format = "ccyydl")
                                 )


  ## make sure path ends with '/'
  if(stringr::str_sub(path_to_files,-1) != "/"){
    path_to_files <- stringr::str_c(path_to_files, "/")
  }


  ## read files
  output_list <- purrr::map(file_name_elements, function(file, path_to_files, col_select){

    read_LIS(LIS_DIR = path_to_files, file_name = file, col_select = col_select)

  }, path_to_files, vars)


  ## add attributes
  attr(output_list, "merged_levels") <- FALSE
  attr(output_list, "level") <- level_
  attr(output_list, "database") <- database_

  if(full_year_names){

    names(output_list) <- purrr::map_chr(files_ccyydl, ~change_file_name_format(.x, 'ccyyyydl'))

  }else{

    names(output_list) <- files_ccyydl

  }


  # get level from file_names


  return(output_list)

}



#' Read the format of files argument
#'
#' @description
#'\lifecycle{superseded}
#'   Use 'read_file_name_format' instead.
#'
#'   Detects the format of the 'file' argument passed to 'read_lissy_files_locally()'
#'   and returns it in a 'two digit country, two digit year, one digit level (optional)'
#'   format.
#'   The names of the files this function can read need to indicate the country ('c'),
#'   year ('y') and if it's a household or person level dataset ('l').
#'   In addition, it can optionally indicate if it's a LIS, LWS or ERFLIS dataset ('d').
#'   This information can be in the following formats:
#'   \itemize{
#'   \item 'ccyyl' Two digit country, two digit year, one digit level. E.g. "fr84h"
#'   \item 'ccyyyyl' Two digit country, four digit year, one digit household/person ('h' or 'p'). E.g. "fr1984h"
#'   \item 'ccyydl' Two digit country, two digit year, one digit indicating LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g. "fr84ih"
#'   \item 'ccyyyydl' Two digit country, two digit year,one digit indicating LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g. "fr1984ih"
#'     }
#'   It can contain any printable characters after the pattern (e.g. 'fr84ih_new').
#'
#'   @return A file name with the format 'ccyydl' or 'ccyyl' depending if information on the
#'   database is provided or not.
#'
#'
#' @keywords internal
read_format_names <- function(file_name){

  ## check format

  correct_input_format <- stringr::str_detect(file_name, pattern = "[a-z]{2}(\\d{2}|\\d{4})([iwer])?[ph].*")

  assertthat::assert_that(correct_input_format,
                          msg = glue::glue("The inputs to 'file' argument are not correct. The format of {file_name} couldn't be guessed.
                          \n Please see documentation for accepted formats of file names."))

  country_ <- stringr::str_extract(file_name, "^[A-z]{2}")
  year_ <- stringr::str_match(file_name, "^[A-z]{2}([0-9]{2,4})")[,2]
  database_ <- stringr::str_match(file_name, "^[A-z]{2}[0-9]{2,4}([iwer])")[,2]
  level_ <- stringr::str_match(file_name, "^[A-z]{2}[0-9]{2,4}[iwer]?([ph])")[,2]

  # change to two digit year
  year_ <- stringr::str_sub(year_, start = -2, end = -1)

  # database to "" if NA
  database_ <- dplyr::if_else(is.na(database_),
                              true = "",
                              false = database_)

  output <- stringr::str_c(country_, year_, database_, level_)

  return(output)

}




#' Rename read files
#'
#' @description'
#' \lifecycle{experimental}
#' Uses a character vector to manually change the names files that have been previously read.
#'
#' @param list_files A list of LIS or LWS datasets.
#' @param new_names A character string with the new names of files.
#' @return A list of loaded files with the new names.
#' @examples
#' \dontrun{
#' files <- read_lissy_files_locally(c("fr84h", "fr94h", "fr10h"))
#' files %<>%
#'     read_rename_files(new_names = c("France_1984_household",
#'                                     "France_1994_household",
#'                                     "France_2010_household"))
#' }
read_rename_files <- function(list_files, new_names){

  assertthat::assert_that(assertthat::are_equal(length(list_files), length(new_names)),
                        msg = "The length of 'new_names' should be the same as the lengh of the list of files.")

  assertthat::assert_that(all(!is.na(new_names)),
                        msg = "'new_names' should not contain NAs.")

  assertthat::assert_that(all(!is.null(new_names)),
                          msg = "'new_names' should not contain NULL.")

  names(list_files) <- new_names

  return(list_files)

}


#' Read LIS
#'
#' @description
#'\lifecycle{experimental}
#'   A modified version of the original read.LIS function.
#'
#'   Is used as a lower-level function to read single files in 'read_lissy_files'
#'     and 'read_lissy_files_locally'.
#'
#'   Contains the following amendments to the original function:
#'   \itemize{
#'
#'    \item used as a lower-level function that takes a single file name and reads
#'      a single file at a time.
#'    \item uses 'haven::read_dta()' function to read the data instead of 'readstata13::read.dta13()'.
#'    \item incorporates the code from the previous lower-level function 'read.LIS.data' (modified).
#'    \item more informative errors when the file name is passed is not correct.
#'    \item more informative errors when there are multiple possible files to import.
#'    \item 'subset' argument is eliminated as not so many users might be
#'      selecting subsets of rows.
#'   }
#'
#' @param LIS_DIR A string with the directory where the file to read is stored.
#' @param file_name A string with the name of the file to import in 'ccyydl' or 'ccyyyydl' format. E.g 'fr84ih', 'fr1984ih'.
#' @param col_select A string vector with the names of the variables to read. If NULL (default), all variables in the file are read.
#'
#' @keywords internal
read_LIS <- function(LIS_DIR, file_name,  col_select = NULL){

  # force LIS_DIR to end with '/'
  if(stringr::str_sub(LIS_DIR, -1) != "/"){
    LIS_DIR <- stringr::str_c(LIS_DIR, "/")
  }

  assertthat::assert_that(stringr::str_detect(file_name, pattern = "^\\w{2}(\\d{2}|\\d{4})[iwe][hpr]$"),
                          msg = glue::glue("'{file_name}' is an incorrect 'file_name'. Please specify a file name in format 'ccyydl'. E.g. 'fr84ih'."))

  file_name_ccyydl <- stringr::str_c(change_file_name_format(file_name, to_format = 'ccyydl'), ".dta")

  files_in_LIS_DIR  <- list.files(path = LIS_DIR)

  files_in_LIS_DIR <- stringr::str_subset(files_in_LIS_DIR, pattern = "\\.dta$")

  assertthat::assert_that(length(files_in_LIS_DIR) > 0,
                          msg = glue::glue("No files were found in '{LIS_DIR}'"))

  matching_files <- suppressWarnings(stringr::str_subset(files_in_LIS_DIR, pattern = glue::glue("^{file_name_ccyydl}$")))

  if(length(matching_files) > 1){

    stop(glue::glue("Multiple files matched the file_name '{file_name}'."))

  } else if(length(matching_files) == 0){

    stop(glue::glue("No files matched the file_name '{file_name}'."))

  }


  if(!is.null(col_select)){

  data <- suppressWarnings(as.data.frame(haven::read_dta(file = stringr::str_c(LIS_DIR, file_name_ccyydl), encoding = "UTF-8", col_select = col_select)))

  }else{

    data <- suppressWarnings(as.data.frame(haven::read_dta(file = stringr::str_c(LIS_DIR, file_name_ccyydl), encoding = "UTF-8")))

  }

  return(data)

}


# private_functions.R

#' Get directory from parameters file
#'
#' @return A string with the LIS, LWS or ERFLIS directory.
#'
#' @keywords internal
get_directory <- function(){

  get(x = "LIS_DIR", envir = .GlobalEnv)

}


#' Get the database from a directory string
#' 
#' @keywords internal
retrieve_database_from_directory <- function(){

  dir <- get_directory()

  if(stringr::str_detect(dir, pattern = "/erflis")){
    return("e")
  }else if(stringr::str_detect(dir, pattern = "/lis")){
    return("i")
  }else if(stringr::str_detect(dir, pattern = "/lws")){
    return("w")
  }else{
    stop(glue::glue("Database couldn't be retrieved from '{dir}'"))
  }

}


