# Compute (weighted) counts or percentages from a list of data frames

This function calculates (weighted) category counts or percentages for a
given categorical variable across a list of data frames (e.g., by
country or year). Optionally, results can be grouped by another
categorical variable.

## Usage

``` r
run_weighted_count(
  data_list,
  var_name,
  wgt_name = NULL,
  na.rm = FALSE,
  by = NULL,
  percent = FALSE
)
```

## Arguments

- data_list:

  A named list of data frames, (e.g., across countries or years).

- var_name:

  A string specifying the name of the categorical variable for which
  counts or percentages are to be computed. This must be listed in
  [`lissyrtools::lis_categorical_variables`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lis_categorical_variables.md)
  or
  [`lissyrtools::lws_wealth_categorical_variables`](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lws_wealth_categorical_variables.md).

- wgt_name:

  (Optional) A string specifying the name of the weight variable to
  apply. If `NULL`, unweighted counts are used.

- na.rm:

  Logical; if `TRUE`, observations with missing values in `var_name` are
  removed before computing counts or percentages.

- by:

  (Optional) Optional string giving the name of a categorical variable
  to split the data within each data frame before computing statistics.

- percent:

  Logical; if `TRUE`, the function returns weighted (or unweighted)
  percentages. If `FALSE`, it returns simple category counts.

## Value

A named list.

- If `by` is `NULL`: each list element is named by country and contains
  a named numeric vector, where the names are years and the values are
  counts or percentages.

- If `by` is not `NULL`: each list element is named by `ccyy`
  (country-year) identifiers and contains a named numeric vector, where
  the names represent the `by`-categories (e.g., gender, region) and the
  values are the corresponding counts or percentages.

## Details

- Any data frame where the `by` variable contains only `NA`s is dropped,
  with a warning.

## Examples

``` r
if (FALSE) { # \dontrun{ 
library(lissyrtools)
library(purrr)
library(dplyr)

data <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi", "region_c", "area_c", "educ", "emp"), from = 2016)


run_weighted_count(
 data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
 var_name ="educ", 
 by = "emp", 
 percent = FALSE, 
 na.rm = TRUE
)

# Specify `percent` = TRUE, to output percentages, unweighted or weighted.
run_weighted_count(
 data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
 var_name ="region_c", 
 percent = TRUE, 
 na.rm = FALSE
)

# It is also possible to check the share of missings. 
run_weighted_count(
 data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
 var_name ="region_c", 
 percent = TRUE, 
 na.rm = TRUE
)  


# When `percent` = FALSE, and `wgt_name` is specified, it will be ignore and an unweighted count will be applied.
run_weighted_count(
 data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
 var_name ="region_c", 
 wgt_name = "hpopwgt",
 percent = FALSE,
 na.rm = TRUE
) 

#  Datasets where the variable in the `var_name` argument is only made of NA's will not be considered.
run_weighted_count(
 data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
 var_name ="area_c", 
 percent = FALSE,
 na.rm = TRUE
) 

# The same logic is applied with the `by` argument.
run_weighted_count(
data[names(data)[stringr::str_sub(names(data),3,4) == "18"]], 
"educ", 
na.rm = TRUE, 
by = "area_c"
)

} # }
```
