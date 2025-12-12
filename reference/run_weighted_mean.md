# Compute Weighted Mean Across a List of Data Frames (with optional grouping)

Compute Weighted Mean Across a List of Data Frames (with optional
grouping)

## Usage

``` r
run_weighted_mean(
  data_list,
  var_name,
  wgt_name = NULL,
  na.rm = TRUE,
  by = NULL
)
```

## Arguments

- data_list:

  A named list of data frames, (e.g., across countries or years).

- var_name:

  A string specifying the variable name (e.g., "dhi", "pilabour") to
  compute the mean on.

- wgt_name:

  An optional string specifying the weight variable to be used. If
  `NULL`, equal weights are assumed.

- na.rm:

  Logical. If `TRUE`, missing values in `var_name` or `wgt_name` are
  removed.

- by:

  Optional string giving the name of a categorical variable to split the
  data within each data frame before computing the mean.

## Value

A named list.

- If `by` is `NULL`: each list element is named by country and contains
  a named numeric vector, where the names are years and the values are
  the computed statistics.

- If `by` is not `NULL`: each list element is named by `ccyy`
  (country-year) identifiers and contains a named numeric vector, where
  the names represent the `by`-categories (e.g., gender, region) and the
  values are the corresponding statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(purrr)
library(dplyr)

data <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi", "age", "pi11", "region_c", "area_c", "educ", "emp"), from = 2016)

data %>% 
purrr::map(~ .x %>% filter(relation == 1000)) %>%
run_weighted_mean("dhi", "hpopwgt")

data %>% 
purrr::map(~ .x %>% filter(age > 25 & age <65)) %>%
run_weighted_mean("pi11", "ppopwgt", by = "educ")

} # }
```
