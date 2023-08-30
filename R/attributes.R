# attributes.R

# tools to deal with loss of attributes.

#' Get the lissy attributes
#'
#' Retrieves the 'level', 'merged_levels' and 'database' attributes from list of tibbles or a tibble.
#'
#' @param lissy_object A list of tibbles or a tibble.
#'
#' @return A list with three elements.
#'
#' @keywords internal
get_lissy_attributes <- function(lissy_object){

  if(!is.null(attributes(lissy_object)) && any( c("level", "merged_levels", "database") %in% names(attributes(lissy_object))   )){

    attributes_ <- attributes(lissy_object)[c("level", "merged_levels", "database")]

    names(attributes_) <- c("level", "merged_levels", "database")

    return(attributes_)

  }else{

    return(NULL)

  }
}


#' Set lissy attributes
#'
#' Sets the 'level', 'merged_levels' and 'database' attributes of an object equal to
#'   the specified ones.
#'
#' @param lissy_object A list of tibbles or a tibble.
#' @param lissy_attributes A list with three elements: 'level', 'merged_levels'
#'   and 'database'.
#'
#' @return The same type of object as 'lissy_object'.
#'
#' @keywords internal
set_lissy_attributes <- function(lissy_object, lissy_attributes){

  assertthat::assert_that(rlang::is_list(lissy_attributes) && all(c("level", "merged_levels", "database") %in% names(lissy_attributes)),
                          msg = "'lissy_attributes' must be a list with the following elements: 'level', 'merged_levels', 'database'.")

  attr(lissy_object, "level") <- lissy_attributes[["level"]]
  attr(lissy_object, "merged_levels") <- lissy_attributes[["merged_levels"]]
  attr(lissy_object, "database") <- lissy_attributes[["database"]]

  return(lissy_object)

}


#' A list with lissy attributes
#'
#' @param lissy_object A list of tibbles or a tibble.
#'
#' @return A boolean.
#'
#' @keywords internal
has_lissy_attributes <- function(lissy_object){

  return( !is.null(attributes(lissy_object)) && any( c("level", "merged_levels", "database") %in% names(attributes(lissy_object))) )

}



#' Paste lissy attributes
#'
#' Implements returns the 'lissy_object' with the 'lissy_attributes' if
#'   'lissy_attributes is not NULL. Else, it just returns 'lissy_object'.
#'
#' @param lissy_object A list of tibbles or a tibble.
#' @param lissy_attributes A list of attributes or a NULL value, as obtained from 'get_lissy_attributes()'.
#'
#' @return A tibble.
#'
#' @keywords internal
paste_lissy_attributes <- function(lissy_object, lissy_attributes){

  if(!is.null(lissy_attributes)){

    lissy_object <- set_lissy_attributes(lissy_object, lissy_attributes)

  }

  return(lissy_object)

}

