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


#' Sample Household-Level LIS Dataset (1,000 Rows)
#'
#' This dataset provides household-level data from the LIS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever Italy is selected along with at least one household-level 
#' variable (e.g., "region_c").
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `it14h`, with 1,000 rows and 112 columns.
#' @examples
#' italy_14_lis_h
"italy_14_lis_h"


#' Sample Individual-Level LIS Dataset (1,000 Rows)
#'
#' This dataset provides individual-level data from the LIS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever Italy is selected along with at least one individual-level 
#' variable (e.g., "age").
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `it14p`, with 1,000 rows and 112 columns.
#' @examples
#' italy_14_lis_p
"italy_14_lis_p"


#' Sample Household-Level LIS Dataset (1,000 Rows)
#'
#' This dataset provides household-level data from the LIS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever United States is selected along with at least one household-level 
#' variable (e.g., "region_c").
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `us16h`, with 1,000 rows and 112 columns.
#' @examples
#' united_states_16_lis_h
"united_states_16_lis_h"

#' Sample Individual-Level LIS Dataset (1,000 Rows)
#'
#' This dataset provides individual-level data from the LIS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever United States is selected along with at least one individual-level 
#' variable (e.g., "age").
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `us16p`, with 1,000 rows and 112 columns.
#' @examples
#' united_states_16_lis_p
"united_states_16_lis_p"

#' Sample Household-Level LIS Dataset (1,000 Rows)
#'
#' This dataset provides household-level data from the LIS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever Mexico is selected along with at least one household-level 
#' variable (e.g., "region_c").
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `mx18h`, with 1,000 rows and 112 columns.
#' @examples
#' mexico_18_lis_h
"mexico_18_lis_h"

#' Sample Individual-Level LIS Dataset (1,000 Rows)
#'
#' This dataset provides individual-level data from the LIS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever Mexico is selected along with at least one individual-level 
#' variable (e.g., "age").
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `mx18p`, with 1,000 rows and 112 columns.
#' @examples
#' mexico_18_lis_p
"mexico_18_lis_p"


#' Sample Household-Level LWS Dataset (1,000 Rows)
#'
#' This dataset provides household-level data from the LWS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever Italy is selected along with at least one household-level 
#' variable (e.g., "region_c") and the `lws` argument is set to `TRUE`.
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `it14h`, with 1,000 rows and 214 columns.
#' @examples
#' italy_14_lws_h
"italy_14_lws_h"

#' Sample Individual-Level LWS Dataset (1,000 Rows)
#'
#' This dataset provides individual-level data from the LWS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever Italy is selected along with at least one individual-level 
#' variable (e.g., "age") and the `lws` argument is set to `TRUE`.
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `it14p`, with 1,000 rows and 133 columns.
#' @examples
#' italy_14_lws_p
"italy_14_lws_p"


#' Sample Household-Level LWS Dataset (1,000 Rows)
#'
#' This dataset provides household-level data from the LWS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever United States is selected along with at least one household-level 
#' variable (e.g., "region_c") and the `lws` argument is set to `TRUE`.
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `it14h`, with 1,000 rows and 214 columns.
#' @examples
#' united_states_16_lws_h
"united_states_16_lws_h"


#' Sample Individual-Level LWS Dataset (1,000 Rows)
#'
#' This dataset provides individual-level data from the LWS database with 1,000 rows, 
#' designed for exploration and user interaction. It is automatically loaded with 
#' `lissyuse_locally()` whenever United States is selected along with at least one individual-level 
#' variable (e.g., "age") and the `lws` argument is set to `TRUE`.
#'
#' The primary purpose of this dataset, and the `lissyuse_locally()` function, is to allow 
#' users to build and test code locally, simulating its execution before running it on LISSY 
#' (the LIS user interface) using the `lissyuse()` function.  
#'
#' @format A list containing a single data frame, `it14p`, with 1,000 rows and 133 columns.
#' @examples
#' united_states_16_lws_p
"united_states_16_lws_p"
