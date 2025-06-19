#' CPI and PPP deflators.
#'
#' A dataset containing the Consumer Price Index (CPI) and Purchasing Power Parity (PPP)
#' deflators that can be used to directly compare absolute monetary values across different
#' LIS and LWS datasets.
#'
#' @format A data frame with 7 variables:
#'
#' @source \url{https://www.lisdatacenter.org/resources/ppp-deflators/}
"deflators"


#' LIS and LWS datasets
#'
#' A data frame containing all the LIS and LWS datasets 
#' that can be used through LISSY, including some of their characteristics.
#'
#' @format A data frame with 7 variables:
"datasets"

#' LWS income reference years
#'
#' A data frame containing the reference years for the income variables for each LWS dataset. 
#'
#' @format A data frame with 2 variables:
"data_inc_ref_year"


#' Variable labels
#'
#' A data frame containing all LIS and LWS variables and its labels. 
#'
#' @format A data frame with 2 variables:
"data_vars_labels"

#' Variable notes
#'
#' A data frame indicating whether a given variable in one of the LIS/LWS datasets has a note or not for a specific year.
#'
#' @format A data frame with 6 variables:
"data_with_warnings"

#' Variable Status Data Frame
#'
#' A data frame that identifies whether a variable consists only of zeros or missing values for a given country and year.
#'
#' @format A data frame with 6 variables:
"missing_or_zero_vars_all"


#' Value Labels for Country-Specific Variables in LIS and LWS Datasets
#'
#' A data frame containing the categorized values of country-specific variables in the LIS and LWS datasets, organized by country and year.
#'
#' @format A data frame with 9 variables:
"value_label_c_data"

#' LIS Variables 
#'
#' A character vector containing the names of the variables available in LIS.
#'
#' @format A character vector of length 199.
"lis_variables"

#' LWS Variables 
#'
#' A character vector containing the names of the variables available in LWS.
#'
#' @format A character vector of length 336.
"lws_variables"


#'  Value Labels for Country-Specific Variables in LIS and LWS Datasets
#'
#' A data frame containing the categorized values of country-specific variables in the LIS and LWS datasets, organized by country and year.
#'
#' @format A data frame with 9 variables:
"value_label_c_data"

#' Sample Household-Level LIS Dataset - Italy 2014
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' it14_h_lis
#' }
"it14_h_lis"

#' Sample Individual-Level LIS Dataset - Italy 2014

#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' it14_p_lis
#' }
"it14_p_lis"

#' Sample Household-Level LIS Dataset - Italy 2016
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' it16_h_lis
#' }
"it16_h_lis"

#' Sample Individual-Level LIS Dataset - Italy 2016
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' it16_p_lis
#' }
"it16_p_lis"

#' Sample Household-Level LIS Dataset - Italy 2020
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' it20_h_lis
#' }
"it20_h_lis"

#' Sample Individual-Level LIS Dataset - Italy 2020
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' it20_p_lis
#' }
"it20_p_lis"

#' Sample Household-Level LIS Dataset - Mexico 2014
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' mx14_h_lis
#' }
"mx14_h_lis"

#' Sample Individual-Level LIS Dataset - Mexico 2014
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' mx14_p_lis
#' }
"mx14_p_lis"

#' Sample Household-Level LIS Dataset - Mexico 2016
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' mx16_h_lis
#' }
"mx16_h_lis"

#' Sample Individual-Level LIS Dataset - Mexico 2016
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' mx16_p_lis
#' }
"mx16_p_lis"

#' Sample Household-Level LIS Dataset - Mexico 2018
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' mx18_h_lis
#' }
"mx18_h_lis"

#' Sample Individual-Level LIS Dataset - Mexico 2018
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' mx18_p_lis
#' }
"mx18_p_lis"

#' Sample Household-Level LIS Dataset - US 2014
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' us14_h_lis
#' }
"us14_h_lis"

#' Sample Individual-Level LIS Dataset - US 2014
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' us14_p_lis
#' }
"us14_p_lis"

#' Sample Household-Level LIS Dataset - US 2016
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' us16_h_lis
#' }
"us16_h_lis"

#' Sample Individual-Level LIS Dataset - US 2016
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' us16_p_lis
#' }
"us16_p_lis"

#' Sample Household-Level LIS Dataset - US 2018
#'
#' @format Data frame with 1,000 rows and 112 columns.
#' @examples
#' \dontrun{
#' us18_h_lis
#' }
"us18_h_lis"

#' Sample Individual-Level LIS Dataset - US 2018
#'
#' @format Data frame with 1,000 rows and 97 columns.
#' @examples
#' \dontrun{
#' us18_p_lis
#' }
"us18_p_lis"

#' Sample Household-Level LWS Dataset - Italy 2014
#'
#' @format Data frame with 1,000 rows and 214 columns.
#' @examples
#' \dontrun{
#' it14_h_lws
#' }
"it14_h_lws"

#' Sample Individual-Level LWS Dataset - Italy 2014
#'
#' @format Data frame with 2,328 rows and 133 columns.
#' @examples
#' \dontrun{
#' it14_p_lws
#' }
"it14_p_lws"

#' Sample Household-Level LWS Dataset - Italy 2016
#'
#' @format Data frame with 1,000 rows and 214 columns.
#' @examples
#' \dontrun{
#' it16_h_lws
#' }
"it16_h_lws"

#' Sample Individual-Level LWS Dataset - Italy 2016
#'
#' @format Data frame with 2,215 rows and 133 columns.
#' @examples
#' \dontrun{
#' it16_p_lws
#' }
"it16_p_lws"

#' Sample Household-Level LWS Dataset - US 2016
#'
#' @format Data frame with 1,000 rows and 214 columns.
#' @examples
#' \dontrun{
#' us16_h_lws
#' }
"us16_h_lws"

#' Sample Individual-Level LWS Dataset - US 2016
#'
#' @format Data frame with 2,396 rows and 133 columns.
#' @examples
#' \dontrun{
#' us16_p_lws
#' }
"us16_p_lws"

#' Sample Household-Level LWS Dataset - US 2019
#'
#' @format Data frame with 1,000 rows and 214 columns.
#' @examples
#' \dontrun{
#' us19_h_lws
#' }
"us19_h_lws"

#' Sample Individual-Level LWS Dataset - US 2019
#'
#' @format Data frame with 2,273 rows and 133 columns.
#' @examples
#' \dontrun{
#' us19_p_lws
#' }
"us19_p_lws"



#' ERFLIS Household Variables
#'
#' A character vector containing the names of household-level variables used in ERFLIS datasets.
#'
#' @keywords internal
#' @format Character vector
"erflis_household_variables"

#' ERFLIS Person Variables
#'
#' A character vector containing the names of person-level variables used in ERFLIS datasets.
#'
#' @keywords internal
#' @format Character vector
"erflis_person_variables"

#' Key Household Variables in LIS
#'
#' A character vector of key household variables specific to LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"key_vars_household_lis"

#' Key Household Variables in LWS
#'
#' A character vector of key household variables specific to LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"key_vars_household_lws"

#' Key Person Variables in LIS
#'
#' A character vector of key person variables specific to LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"key_vars_person_lis"

#' Key Person Variables in LWS
#'
#' A character vector of key person variables specific to LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"key_vars_person_lws"

#' LIS Both Household and Person Variables
#'
#' A character vector combining both household and person variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_both_hp_variables"

#' LIS Categorical Variables
#'
#' A character vector containing the names of categorical variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_categorical_variables"

#' LIS Continuous Variables
#'
#' A character vector containing the names of continuous variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_continuous_variables"

#' LIS Country-Specific Variables
#'
#' A character vector of variables specific to certain countries in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_country_specific_variables"

#' LIS Household Variables
#'
#' A character vector containing the names of household-level variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_household_variables"

#' LIS ID Variables
#'
#' A character vector containing the ID variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_id_variables"

#' LIS Income Variables
#'
#' A character vector containing income-related variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_income_variables"

#' LIS Person Variables
#'
#' A character vector containing the names of person-level variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_person_variables"

#' LIS Technical Variables
#'
#' A character vector of technical variables used in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_technical_variables"

#' LIS Weight Variables
#'
#' A character vector of weight variables in LIS datasets.
#'
#' @keywords internal
#' @format Character vector
"lis_weight_variables"

#' LWS Both Household and Person Variables
#'
#' A character vector combining household and person variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_both_hp_variables"

#' LWS Household Variables
#'
#' A character vector containing household-level variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_household_variables"

#' LWS ID Variables
#'
#' A character vector containing ID variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_id_variables"

#' LWS Person Variables
#'
#' A character vector containing person-level variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_person_variables"

#' LWS Wealth Categorical Variables
#'
#' A character vector of categorical wealth-related variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_wealth_categorical_variables"

#' LWS Wealth Continuous Variables
#'
#' A character vector of continuous wealth-related variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_wealth_continuous_variables"

#' LWS Wealth Country-Specific Variables
#'
#' A character vector of country-specific wealth variables in LWS datasets.
#'
#' @keywords internal
#' @format Character vector
"lws_wealth_country_specific_variables"
