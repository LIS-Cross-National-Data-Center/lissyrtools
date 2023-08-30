#' Compute percentiles.
#'
#' \lifecycle{experimental}
#' Compute the  weighted percentages for a LIS or LWS variable.
#'
#' It uses 'pwgt' or 'hwgt' to weight the indicator.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character vector of length one with the indicator that needs to be transformed.
#' @param breaks A numeric vector specifying the percentiles that should be computed. Defaults to all integers from 0 to 100.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @param na.rm A boolean indicating if the computation should ignore missing
#'   values in 'variable' and 'weight'.
#'
#' @return A tibble with two columns: 'percentile' and 'value'. The first contains
#'   the label of the percentiles computed (e.g. '0.5' for median, '0.2' for first quintile).
#'   The second contains the values of these in the distribution of 'variable'.
#'
#' @examples
#' \dontrun{
#' lissy_datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' compute_percentiles(lissy_datasets[["fr1984h"]], variable = "dhi", na.rm = TRUE)
#' }
compute_percentiles <- function(file, file_name, variable, breaks = seq(0, 1, 0.1), weight = NULL, na.rm = FALSE){

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  var <- file[[variable]]

  if(is.null(weight)){
    weight_ <- rep(1, length(var))
  }else{
    weight_ <- file[[weight]]
    missing_values_in_variable_warning(file = file,
                                       file_name = file_name,
                                       variable = weight)
  }

  if(is.all.na.or.zero(file[variable])){
    return(
      tibble::tibble(
        percentile = breaks,
        value = rep(NA_real_, length(breaks))
      )
    )
  }

  if(na.rm){

      index_missing <- is.na(var) | is.na(weight_)
      var <- var[!index_missing]
      weight_ <- weight_[!index_missing]

  }

  if(any(is.na(var))){

    warning(glue::glue("'{variable}' in '{file_name}' contains NAs. Use na.rm = TRUE to ignore them."))
    return(NA)

  }else if(any(is.na(weight_))){

    warning(glue::glue("There are NAs in the weighting variable '{weight}' in '{file_name}'. Use na.rm = TRUE to ignore them."))
    return(NA)

  }else{

    tibble::tibble(
      percentile = breaks,
      value = unname(wtd.quantile(x = var, weights = weight_, probs = breaks,
                   na.rm = na.rm, normwt = TRUE))
    )
  }

}





#' Compute gini index.
#'
#' \lifecycle{experimental}
#' Compute the gini index with  weights.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the name of the variable for which gini should be computed.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
#'
#' @return A numeric vector with the gini index.
#'
#' @example
#' \dontrun{
#' lissy_datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' compute_gini(lissy_datasets[["fr1984h"]], "dhi", na.rm = TRUE)
#' }
compute_gini <- function(file, file_name, variable, weight = NULL, na.rm = FALSE) {

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  var <- file[[variable]]

  if(is.all.na.or.zero(var)){return(NA_real_)}

  if(!is.null(weight)){

    weight_ <- file[[weight]]

  }else{

    weight_ <- rep(1, times = length(var))

  }

  if(na.rm){

    index_nas <- is.na(var)
    var <- var[!index_nas]
    weight_ <- weight_[!index_nas]

  }

  if(any(is.na(var))){

    warning(glue::glue("'{variable}' in '{file_name}' contains NAs. Use na.rm = TRUE to ignore them."))
    return(NA_real_)

  }else if(any(is.na(weight_))){

    warning(glue::glue("There are NAs in the weighting variable '{weight}' in '{file_name}'. Use na.rm = TRUE to ignore them."))
    return(NA_real_)


  }else{

    assertthat::assert_that(!is.all.na.or.zero(var),
                            msg = "'variable' does not contain any valid value.")

    ovar <- order(var)
    var <- var[ovar]
    weight_ <- weight_[ovar]/sum(weight_)
    p <- cumsum(weight_)
    nu <- cumsum(weight_*var)
    n <- length(nu)
    nu <- nu/nu[n]
    res <- sum(nu[-1]*p[-n])-sum(nu[-n]*p[-1])

    return(res)

  }

}



#' Compute standard indicators.
#'
#' \lifecycle{experimental}
#' Compute the weighted mean, median or percentile ratios for a variable.
#'
#' @param file A tibble or data.frame with a LIS or LWS file.
#' @param file_name A string with the name of the LIS or LWS file.
#' @param variable A string with the name of the variable for which the
#' indicator should be computed.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
#' @param ratio A vector of two numeric values between 0 and 1. Defines the
#' percentiles in the numerator and denominator respectively. E.g. (0.9, 0.1)
#' computes the 90/10 ratio.
#'
#' @return A numeric vector.
compute_mean <- function(file, file_name, variable, weight = NULL, na.rm = FALSE){

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  if(is.all.na.or.zero(file[variable])){return(NA_real_)}

  if(missing(weight) || is.null(weight)){

    weight_ <- rep(1, length(file[[variable]]))

  }else{

    assertthat::assert_that(weight %in% names(file),
                            msg = glue::glue("'{weight}' could not be found in '{file_name}'."))

    weight_ <- file[[weight]]

  }

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

    wtd.mean(x = file[[variable]],
             weights = weight_,
             na.rm = na.rm)

}



#' @rdname compute_mean
compute_median <- function(file, file_name, variable, weight = NULL, na.rm = FALSE){

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  if(is.all.na.or.zero(file[variable])){return(NA_real_)}

  if(is.null(weight)){

    weight_ <- rep(1, length(file[[variable]]))

  }else{

    assertthat::assert_that(weight %in% names(file),
                            msg = glue::glue("'{weight}' could not be found in '{file_name}'."))

    weight_ <- file[[weight]]

  }


  # the original function from Hsmisc is incredibly buggy. It would return a result with NAs and na.rm = F.
  if(!na.rm & (any(is.na(file[[variable]])) | any(is.na(weight_)) )){
    return(NA)
  }

  unname(matrixStats::weightedMedian(x = file[[variable]],
                      w = weight_,
                      na.rm = na.rm))

}



#' @rdname compute_mean
compute_ratio <- function(file, file_name, variable, ratio = c(0.9, 0.1), weight = NULL, na.rm = FALSE){

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  var <- file[[variable]]

  if(is.all.na.or.zero(file[variable])){return(NA_real_)}

  assertthat::assert_that(length(ratio) == 2,
                          msg = "'ratio' should be a vector with length 2.")
  assertthat::assert_that(all(dplyr::between(ratio, 0, 1)),
                          msg = "Values in 'ratio' must be between 0 and 1.")

  if(is.null(weight)){

    weight_ <- rep(1, length(file[[variable]]))

  }else{

    weight_ <- file[[weight]]
    missing_values_in_variable_warning(file = file,
                                       file_name = file_name,
                                       variable = weight)

  }

  if(na.rm){

    index_missing <- is.na(var) | is.na(weight_)
    var <- var[!index_missing]
    weight_ <- weight_[!index_missing]

  }


  if(any(is.na(var))){

    warning(glue::glue("'{variable}' in '{file_name}' contains NAs. Use na.rm = TRUE to ignore them."))
    return(NA)

  }else if(any(is.na(weight_))){

    warning(glue::glue("There are NAs in the weighting variable '{weight}' in '{file_name}'. Use na.rm = TRUE to ignore them."))
    return(NA)

  }else{

  unname(wtd.quantile(var,
               weights = weight_,
               probs = c(ratio[1]), na.rm = na.rm, normwt = TRUE)/wtd.quantile(var,
                                                                weights = weight_,
                                                                probs = c(ratio[2]),
                                                                na.rm = na.rm, normwt = TRUE))
  }
}



#' Compute Atkinson index.
#'
#' \lifecycle{experimental}
#' Compute the Atkinson index with  weights.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A string with the name of the variable for which Atkinson should be computed.
#' @param epsilon A number with the inequality adversion parameter. Needs to be epsilon > 0.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @param na.rm A boolean. Indicates if NAs should be ignored. Defaults to FALSE.
#'
#' @return A numeric vector with the gini index.
compute_atkinson <- function(file, file_name, variable, epsilon, weight = NULL, na.rm = FALSE){

  assertthat::assert_that(epsilon > 0,
                          msg = "'epsilon' needs to be larger than 0.")

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  var <- file[[variable]]

  if(is.all.na.or.zero(var)){return(NA_real_)}

  if(!is.null(weight)){

    weight_ <- file[[weight]]

  }else{

    weight_ <- rep(1, times = length(var))

  }

  if(na.rm){

    index_nas <- is.na(var)
    var <- var[!index_nas]
    weight_ <- weight_[!index_nas]

  }

  if(any(is.na(var))){

    warning(glue::glue("'{variable}' in '{file_name}' contains NAs. Use na.rm = TRUE to ignore them."))
    return(NA_real_)

  }else if(any(is.na(weight_))){

    warning(glue::glue("There are NAs in the weighting variable '{weight}' in '{file_name}'. Use na.rm = TRUE to ignore them."))
    return(NA_real_)

  }

  if(any(var < 0)){
    stop(glue::glue("There were negative values in '{variable}' '{file_name}'. Negative values are not allowed in 'compute_atkinson()'."))
  }


  if(epsilon >= 1 && any(var == 0)){ # remove 0s if epsilon > 1
    warning(glue::glue("0s in '{variable}' were removed before computing the Atkinson index for '{file_name}'. 0s are not allowed if epsilon >= 1."))
    index_0s <- var == 0
    var <- var[!index_0s]
    weight_ <- weight_[!index_0s]

  }

  var   <- var/mean(var)
  weight_ <- weight_/sum(weight_)
  return(
    dplyr::if_else(epsilon==1,
                   true = 1 - (prod(exp(weight_*log(var)))/sum(var*weight_/sum(weight_))),
                   false = 1 - (sum(((var/sum(var*weight_/sum(weight_)))^(1 - epsilon))*weight_/sum(weight_)))^(1/(1-epsilon)))
  )
}


#' Compute poverty rate.
#'
#' \lifecycle{experimental}
#' Computes the weighted poverty rate for a LIS or LWS variable.
#'
#' It uses 'pwgt' or 'hwgt' to weight the indicator.
#'
#' @param file A LIS or LWS file.
#' @param file_name The name of the LIS or LWS file.
#' @param variable A character vector of length one with the indicator that needs to be transformed.
#' @param times_median A number with the factor by which the median should be multiplied
#'   E.g. 0.5 for a poverty rate of 50% the median.
#' @param weight A string with the name of the variable in 'file' that should be
#'   used as sample weights.
#' @param na.rm A boolean indicating if the computation should ignore missing
#'   values in 'variable' and 'weight'.
#'
#' @return A tibble with two columns: 'percentile' and 'value'. The first contains
#'   the label of the percentiles computed (e.g. '0.5' for median, '0.2' for first quintile).
#'   The second contains the values of these in the distribution of 'variable'.
#'
#' @examples
#' \dontrun{
#' lissy_datasets <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
#' compute_poverty_rate(lissy_datasets[["fr1984h"]], file_name = "fr84h", variable = "dhi", times_median = 0.5, na.rm = TRUE)
#' }
compute_poverty_rate <- function(file, file_name, variable, times_median, weight = NULL, na.rm = FALSE){

  checks_compute_functions(file = file,
                           file_name = file_name,
                           variable = variable,
                           weight = weight)

  if(is.all.na.or.zero(file[variable])){return(NA_real_)}

  pov_line <- lissyrtools::compute_median(file = file, file_name = file_name, variable = variable, weight = weight, na.rm = na.rm) * times_median

  if(is.null(weight)){

    return(sum(file[[variable]] < pov_line, na.rm = na.rm)/length(file[[variable]]))

  }else{

    assertthat::assert_that(weight %in% names(file),
                            msg = glue::glue("'{weight}' could not be found in '{file_name}'."))

    weight_ <- file[[weight]]

    return(sum(weight_[file[[variable]] < pov_line], na.rm = na.rm)/sum(weight_, na.rm = na.rm))

  }
}
