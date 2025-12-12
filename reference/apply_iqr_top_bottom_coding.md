# Apply IQR-Based Top and Bottom Coding to LIS/LWS Variables

This function performs top and/or bottom coding on a specified variable
across a list of LIS/LWS datasets. It applies an interquartile range
(IQR)-based rule on the [`log()`](https://rdrr.io/r/base/Log.html)
transformation of the variable. Optionally, weights can be supplied, and
the transformation can be one- or two-sided.

## Usage

``` r
apply_iqr_top_bottom_coding(
  data_list,
  var_name,
  wgt_name = NULL,
  times = 3,
  one_sided = NULL,
  type = c("type_4", "type_2")
)
```

## Arguments

- data_list:

  A named list of data frames, from LIS or LWS microdata.

- var_name:

  Character string. Name of the variable to code (e.g., "dhi").

- wgt_name:

  Optional character string. Name of the weight variable to use in
  computing weighted percentiles.

- times:

  Numeric. The IQR multiplier for determining bounds (default is 3).

- one_sided:

  Character. Set to `"top"`, `"bottom"`, or `NULL` for two-sided coding.

- type:

  Character. Type of quantile estimator to use (default is `"type_4"`).

## Value

A list of data frames with the same structure as `data_list`, where
`var_name` has been adjusted by bounding extreme values according to an
IQR rule.

## Details

The function:

- Transforms the variable to [`log()`](https://rdrr.io/r/base/Log.html)
  scale (logarithmic transformation).

- Replaces invalid log-values (e.g. from log(0) or negatives) with zero.

- Computes the IQR (interquartile range) on the log-transformed variable
  using weighted percentiles.

- Caps values beyond \\\[Q1 - times \* IQR, Q3 + times \* IQR\]\\ on the
  original scale using [`exp()`](https://rdrr.io/r/base/Log.html).

Regarding LWS datasets:

- Datasets with multiple imputations (via `inum`) are detected
  automatically.

- Top and bottom coding is applied **within each imputation group** in
  such datasets.

- Datasets without `inum`, or with only a single imputation, are
  processed normally.

A warning is issued if the variable level (e.g. household vs individual)
seems inconsistent with the dataset structure.

## Examples

``` r
if (FALSE) { # \dontrun{ 
library(lissyrtools)
library(dplyr)

# Import data, ideally at the level of the variable of interest.
data_hhd <- lissyuse("au", vars = c("dhi"), from = 2016)

# Default case, where top and bottom coding is performed simultaneously
data_hhd[1]  %>%
 purrr::map(~ .x[!is.na(.x$dhi), ]) %>%
 purrr::map(~ .x %>% mutate(new_wgt = nhhmem * hwgt)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", times = 3) %>% 
 run_weighted_mean("dhi", "new_wgt")

# Example with the use or arguments `one_sided` = {"top", "bottom"} and `type`
data_hhd[1]  %>%
 purrr::map(~ .x[!is.na(.x$dhi), ]) %>%
 purrr::map(~ .x %>% mutate(new_wgt = nhhmem * hwgt)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", one_sided = "top", type = "type_2") %>% 
 run_weighted_mean("dhi", "new_wgt")

# Load individual-level datasets by selecting individual-level variables, if the target variable is at the individual level (e.g., "pilabour")
data_ind <- lissyrtools::lissyuse("au", vars = c("pilabour", "emp"), from = 2016)

data_ind[1]  %>%
 purrr::map(~ .x[!is.na(.x$pilabour), ] %>% filter(emp ==1)) %>%
 apply_iqr_top_bottom_coding("pilabour", "ppopwgt") %>% 
 run_weighted_percentiles("pilabour", "ppopwgt", probs = seq(0.1, 0.9,0.1))
} # }
```
