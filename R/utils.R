# utils


#' Test if all objects are NAs or 0
#'
#' @keywords internal
is.all.na.or.zero <- function(x){

 return(length(x[!is.na(x) & x != 0]) == 0)

}

#' @importFrom magrittr %>%
#' @export
`%>%` <- magrittr::`%>%`

#' @importFrom magrittr %<>%
#' @export
`%<>%` <- magrittr::`%<>%`



#' Split vector into consecutive values
#'
#' Split a vector into groups of consecutive values.
#'
#' @param x a vector with values that should be split into groups.
#'
#' @keywords internal
split_consecutive_values <- function(x){

  split(x, cumsum(diff(c(-Inf, x)) != 1))

}



#' Number of elements smaller than input
#'
#' Computes the number of elements in 'numbers' smaller than each of the
#' elements in input 'x'.
#'
#' @param x A numeric vector with input elements
#' @param numbers A numeric vector with numbers which
#'
#' @return A vector of integer values of same length as x
#'
#' @keywords internal
n_numbers_smaller <- function(x, numbers){

  purrr::map_int(x, ~ sum(numbers < .x))

}




#' Interval has single value
#'
#' Check if interval beginning and end are equal and thus it's composed
#' of a single value.
#'
#' To be used inside 'compute_lorenz_curve()'
#'
#' @param A string with the interval.
#'
#' @return A boolean indicating if the first and second element of the interval are
#' the same value.
#'
#' @keywords internal
intervals_with_single_value <- function(x){

  x <- stringr::str_remove_all(x, pattern = "[\\(\\)\\]\\[]")

  split_x <- stringr::str_split(string = x,
                     pattern = ",",
                     simplify = TRUE)

  assertthat::assert_that(length(split_x) == 2)

  return( isTRUE(all.equal(split_x[,1], split_x[,2])) )

}



#' Add leading 0s
#'
#'
#' To be used inside 'compute_lorenz_curve()'
#'
#' @keywords internal
add_leading_0s <- function(dataset, n_0s){

  output_dataset <- tibble::tibble(percentile = rep("0", times = n_0s),
                                  wght_sum_agg_in_percentile = rep(0, times = n_0s)) %>%
    dplyr::bind_rows(dataset)

  return(output_dataset)

}



#' All values are equal
#' \lifecycle{experimental}
#'
#' Like base::all.equal(), but ignoring attributes by default.
#'
#' @keywords internal
all.values.equal <- function(target, current){

  all.equal(target = target, current = current,
            check.attributes = FALSE, check.names = FALSE)

}


#' Extend the minimum and maximum of a vector used for breaks
#'
#' The 'cut' function can sometimes fail to include the minimum and maximum
#' values into intervals due to 'the floating point trap'.
#' This function modifies the breaks by extending the
#' lower and uppermost values so it always includes the minimum and maximum.
#'
#' @param x Numeric vector with all values that intervals should include.
#' @param breaks Numeric vector with the breaks that might need to be extended.
#' @param extension A real number by which the maximum and minimum values should
#'     be extended if the originals don't include all values in 'x'.
#'
#' @keywords internal
extend_breaks_min_and_max <- function(x, breaks, extension = 0.0001){

  index_max_breaks <- length(breaks)

  if(isTRUE(all.values.equal(min(x), breaks[1])) & (min(x) < breaks[1]) ){
    breaks[1] <- breaks[1]-extension
  }

  if(isTRUE(all.values.equal(max(x), breaks[index_max_breaks] )) & (max(x) > breaks[index_max_breaks]) ){
    breaks[index_max_breaks] <- breaks[index_max_breaks]+extension
  }

  if(!all(dplyr::between(x, breaks[1], breaks[index_max_breaks]))){
    warning("Not all values of 'x' are included within the breaks after extending these.")
  }

  return(breaks)

}


#' Checks if the dataset is at household level
#'
#' This function checks if there is an 'h' in the name of a LIS or LWS dataset.
#'
#'@param dataset_name The name of a LIS or LWS dataset.
#'
#'@return A boolean.
#'
#'@keywords internal
is.household.dataset <- function(dataset_name){

  stringr::str_detect(dataset_name,
                      pattern = "[a-z]{2}\\d{2,4}[iw]{0,1}h")

}





#' Define an option
#'
#' This function defines an option by using the name of the new option and its
#' value. Can be used to define options programmatically.
#'
#'@param option_name The name of the option.
#'@param option_name The value of the option.
#'
#'
#'@keywords internal
define_option <- function(option_name, value){

  opt_ <- options(option_name)
  opt_[[option_name]] <- value
  options(opt_)

}


#' Change the format of a file name
#'
#' \lifecycle{experimental}
#'
#' Converts a character vector with a file names to a different format.
#'
#' Is used within 'implement_adjust_by_lisppp()', .
#'
#'
#' @param file_names A string with the name of the file.
#' @param to_format A string with the desired format. This can be one of the following:
#' \itemize{
#'   \item 'ccyy' Two digit country, two digit year. E.g. "fr84"
#'   \item 'ccyyyy' Two digit country, four digit year. E.g. "fr1984"
#'   \item 'ccyyl' Two digit country, two digit year, one digit level ('h' or 'p' for household/person). E.g. "fr84h"
#'   \item 'ccyyyyl' Two digit country, four digit year, one digit level. E.g. "fr1984h"
#'   \item 'ccyyd' Two digit country, two digit year, one digit database (LIS, LWS or ERFLIS - i/w/e). E.g. "fr84i"
#'   \item 'ccyyyyd' Two digit country, four digit year, one digit database (LIS, LWS or ERFLIS - i/w/e). E.g. "fr1984i"
#'   \item 'ccyydl' Two digit country, two digit year, one digit indicating LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g. "fr84ih"
#'   \item 'ccyyyydl' Two digit country, two digit year,one digit indicating LIS, LWS or ERFLIS (i/w/e/r), one digit household/person. E.g. "fr1984ih"
#' }
#'
#'@keywords internal
change_file_name_format <- function(file_names, to_format){

  assertthat::assert_that(to_format %in% c('ccyyyydl', 'ccyydl', 'ccyyyyd',
                                           'ccyyd', 'ccyyyyl', 'ccyyl',
                                           'ccyyyy', 'ccyy'),
                          msg = "The format passed in argument 'to_format' needs to be one of the following: /n 'ccyyyydl', 'ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'")

  current_formats <- purrr::map_chr(file_names,
                                    ~read_file_name_format(.x))

  if(all(current_formats == to_format)){

    return(file_names)
  }

  assertthat::assert_that(purrr::every(current_formats,
                                         ~is_change_possible(.x, to_format)),
                          msg = "The change in format of the file name is not possible.")


  output_ <- purrr::map2_chr(file_names, current_formats,
                         .f = function(file_name, current_format, to_format){

                           temp_list_information_from_file <- extract_information_from_file_name(file_name = file_name,
                                                                                                 current_format)

                           return(return_desired_format(country = temp_list_information_from_file["country_"],
                                                        year = temp_list_information_from_file["year_"],
                                                        database = temp_list_information_from_file["database_"],
                                                        level = temp_list_information_from_file["level_"],
                                                        to_format = to_format))
                         },
                         to_format)

  return(output_)

}


#' Read the format of a file name
#'
#' @param file_name A string with the name of the file.
#'
#' @return A string indicating the format of the file.
#'
#' @keywords internal
read_file_name_format <- function(file_name){

  if(stringr::str_detect(file_name, pattern = "^\\w{2}\\d{4}[iwe][hp]$") ){
    return("ccyyyydl")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2}[iwe][hp]$") ){
    return("ccyydl")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{4}[hp]$") ){
    return("ccyyyyl")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2}[hp]$") ){
    return("ccyyl")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{4}[iwe]$") ){
    return("ccyyyyd")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2}[iwe]$") ){
    return("ccyyd")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{4}$") ){
    return("ccyyyy")
  }else if( stringr::str_detect(file_name, pattern = "^\\w{2}\\d{2}$") ){
    return("ccyy")
  }else{
    stop(glue::glue("'{file_name}' is not a valid file name format."))
  }

}



#' Is possible to produce the desired format change
#'
#' To be used inside 'change_file_name_format()'
#'
#' @param current_format A string with the currant format.
#' @param to_format A string with the desired format.
#'
#' @return A boolean.
#'
#' @keywords internal
is_change_possible <- function(current_format, to_format){

  possible_list <- list(
    'ccyyyydl' = c('ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'),
    'ccyydl' = c('ccyyyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'),
    'ccyyyyd' = c('ccyyd', 'ccyyyy', 'ccyy'),
    'ccyyd' = c('ccyyyyd', 'ccyyyy', 'ccyy'),
    'ccyyyyl' = c('ccyyl', 'ccyyyy', 'ccyy'),
    'ccyyl' = c('ccyyyyl', 'ccyyyy', 'ccyy'),
    'ccyyyy' = c('ccyy'),
    'ccyy' = c('ccyyyy')
  )

  return((to_format == current_format) | (to_format %in% possible_list[[current_format]]))

}




#' Extract information from file name
#'
#' Extracts the information regarding country, year, database and level. The
#'   latter two only if available.
#'
#' To be used inside 'change_file_name_format()'.
#'
#' @param file_name A string with the name of the file.
#' @param current_format A string with the currant format.
#'
#' @return A list with the following elements:
#'   \itemize{
#'    \item 'country_' ('cc')
#'    \item 'year_' ('yyyy')
#'    \item 'database_' ('d')
#'    \item 'level_' ('l')
#'    }
#'
#' @keywords internal
extract_information_from_file_name <- function(file_name, current_format){

  assertthat::assert_that(current_format %in% c('ccyyyydl', 'ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'),
                          msg = "Argument 'current_format' needs to be one of the following: 'ccyyyydl', 'ccyydl', 'ccyyyyd', 'ccyyd', 'ccyyyyl', 'ccyyl', 'ccyyyy', 'ccyy'")

  # extract information from current format
  country_ <- stringr::str_extract(file_name, "^[A-z]{2}")

  ## how many 'y's does 'current_format' have?
  if(stringr::str_count(current_format, "y") == 2){

    year_ <- stringr::str_match(file_name, "^[A-z]{2}([0-9]{2})")[,2]

    year_ <- four_digit_year(year_)

  }else if(stringr::str_count(current_format, "y") == 4){

    year_ <- stringr::str_match(file_name, "^[A-z]{2}([0-9]{4})")[,2]

  }

  if(stringr::str_detect(current_format, "d")){

    database_ <- stringr::str_match(file_name, "^[A-z]{2}[0-9]{2,4}([iwe]{1})")[,2]

  }else{

    database_ <- ""

  }

  if(stringr::str_detect(current_format, "l")){

    level_ <- stringr::str_extract(file_name, "[hpr]$")

  }else{

    level_ <- ""

  }

  # assert that no element is NA

  assertthat::assert_that(!is.na(country_) && !is.na(year_) && !is.na(database_) && !is.na(level_),
                          msg = glue::glue("Some extracted elements are NA. Please make sure that '{file_name}' has format '{current_format}'."))


  return(
    list(
      country_ = country_,
      year_ = year_,
      database_ = database_,
      level_ = level_
    )
  )

}




#' Return a file name with the requested format
#'
#' Combines the information passed and returns it in the format stipulated in
#'   argument 'to_format'.
#'
#' To be used inside 'change_file_name_format()'.
#'
#' @param country A string with the country as 'cc'.
#' @param year A string with four digit year as 'yyyy'.
#' @param database A string with database as 'd'.
#' @param level A string with level as 'l'.
#' @param to_format A string with the desired format.
#'
#' @return A string with the file name in the stipulated format.
#'
#' @keywords internal
return_desired_format <- function(country, year, database, level, to_format){

  if(stringr::str_count(to_format, "y") == 2){

    year_out <- stringr::str_sub(year,
                                 start = -2, end = -1)

  }else if(stringr::str_count(to_format, "y") == 4){

    year_out <- stringr::str_sub(year,
                                 start = -4, end = -1)
  }


  if(stringr::str_detect(to_format, "d")){

    database_out <- database

  }else{

    database_out <- ""

  }


  if(stringr::str_detect(to_format, "l")){

    level_out <- level

  }else{

    level_out <- ""

  }

  return(stringr::str_c(country, year_out, database_out, level_out))

}




#' Test if all elements of a vector have the same value
#'
#' @keywords internal
is.all.same.value <- function(x, na.rm = FALSE){

  assertthat::assert_that(rlang::is_vector(x) & !rlang::is_list(x),
                          msg = "The element passed to argument 'x' must be a vector and can not be a list.")

  if(!na.rm){
    assertthat::assert_that(all(!is.na(x)),
                            msg = "'x' can not have NAs if 'na.rm = FALSE'")
  }else if (na.rm & any(is.na(x))){

    x <- x[!is.na(x)]

  }

  assertthat::assert_that(!is.null(x),
                          msg = "'x' can not be null.")


  assertthat::assert_that(length(x) > 0,
                          msg = "'x' can not have length 0.")

  if(is.numeric(x)){

    return(zero_range(x)) # avoid problem with floating point

  }else{

    return(length(unique(x)) == 1)

  }

}

# copy pasted from:
# https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-numeric-vector
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}



#' Compute four digit year from two digit one.
#'
#' \lifecycle{experimental}
#' Computes the four digit year from a string with two digits year.
#'
#' To be used within 'extract_information_from_file_name()'.
#'
#' @param two_digit_year An integer or string value with two digits (from 00 to 99).
#' @return A strign with four digit year.
#'
#' @keywords internal
four_digit_year <- function(two_digit_year){

  assertthat::assert_that( stringr::str_detect(two_digit_year, pattern = "^\\d{2}$"),
                           msg = "Argument 'two_digit_year' should be two digits (00 to 99).")

  dplyr::if_else(dplyr::between(as.numeric(stringr::str_sub(two_digit_year, 1, 2)), 0, 50),
                 true = stringr::str_c("20", stringr::str_sub(two_digit_year, 1, 2) ),
                 false = stringr::str_c("19", stringr::str_sub(two_digit_year, 1, 2)))

}



#' Converts 'i', 'w', 'e' into 'LIS', 'LWS' 'ERFLIS'
#'
#' @keywords internal
database_character_to_name <- function(x){

  x <- tolower(x)
  assertthat::assert_that(stringr::str_detect(x, pattern = "[iwe]"),
                          msg = glue::glue("'{x}' is not a valid database character. Only 'i', 'w' and 'e' are."))


  return(
    switch(EXPR = x,
           'i' = "LIS",
           'w' = "LWS",
           'e' = "ERFLIS"
    )
  )

}



#' Get the database from a list of files
#'
#' Retrieves the 'database' attribute from a list of files.
#'
#' @param lissy_files A list of lissy h-level, p-level or merged files.
#'
#' @keywords internal
get_database <- function(lissy_files){

  assertthat::assert_that(!is.null(attr(lissy_files, "database")),
                          msg = "Attribute 'database' is NULL.")

  database_ <- attr(lissy_files, "database")

  assertthat::assert_that(length(database_) == 1 && !is.null(database_),
                          msg = "Attribute 'database' must have length 1.")

  assertthat::assert_that(database_ %in% c("lis", "lws", "erflis", "i", "w", "e"),
                          msg = glue::glue("Only 'lis', 'lws', 'erflis', 'i', 'w' and 'e' are valid values for databases. Got '{database_}'."))

  return(database_)

}


#' Get the index of hh-heads
#'
#' @description
#' Returns a logical vector indicating which rows correspond to household
#'   heads.
#'
#' @param file A tibble or data.frame with a LIS or LWS file.
#'
#' @return A logical vector with TRUE for hh-heads and FALSE otherwise.
#' @keywords internal
get_index_hh_heads <- function(file){

  relation_ <- as.double(file[["relation"]])
  
  return(!is.na(relation_) & relation == 1000)


}


#' Is a LISSY machine
#'
#' @description
#' Returns a boolean indicating if the code is running on a LISSY machine
#'
#' @return a boolean.
#' @keywords internal
is_lissy_machine <- function(){

  Sys.info()[["effective_user"]] == "lissy"

}


#' Is Canada LWS on the loaded data ?
#'
#' @description
#' Auxilliary function to remove any p-level Canadian datasets in LWS, from the list of loaded datasets, as it has no weights 
#'
#' @return a list
#' @keywords internal
remove_dname_with_missings_in_weights <- function(list, wgt_name) {
 
  # p-level data from Canada/LWS have missing weights on those whose relation is not 1000 reference person
  # be aware of other datasets that might entail these missings in weights.
  
  if (all(c("relation", "inum")  %in% names(list[[1]])) & !is.null(wgt_name)) {
    clean_list <- purrr::discard(list, stringr::str_detect(names(list), "ca")) 
    message(
      "Note: Canadian LWS datasets contain missing weights for individuals not identified as the reference person. These datasets were excluded to ensure valid weighted calculations."
    )
    return(clean_list)
  
  } 
  
  else {
    clean_list <- list
    return(clean_list)
  }
}


#' Validate the Weight Variable Input
#'
#' @description
#' This function checks the validity of the weight variable provided by the user, issuing a helpful message if the variable name
#' does not end with "wgt", as expected by LIS conventions.
#'
#' @param weight_var A character string of length 1 indicating the name of the weight variable used. 
#'
#' @return No return value. The function is used for validation and emits a message if the weight variable name seems unusual.
#' @keywords internal
check_input_in_weight_argument <- function(wgt_name) {
  
  
  if (!is.null(wgt_name) && !stringr::str_detect(wgt_name, "wgt")) {
    message(
      "LIS advice: Please check whether you have used one of the following variables in the `wgt_name` argument:\n",
      "  - \"hwgt\", \"hpopwgt\", \"hwgta\", \"pwgt\", \"ppopwgt\", or \"pwgta\".\n\n",
      "If your data was loaded at the household level instead of the person level, you may need to use a multiple of one of these variables, such as `nhhmem * hwgt`."
    )
  }
  
}



 
