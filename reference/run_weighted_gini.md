# Compute Weighted Gini Index Across a List of Data Frames

Compute Weighted Gini Index Across a List of Data Frames

## Usage

``` r
run_weighted_gini(data_list, var_name, wgt_name = NULL, na.rm = TRUE)
```

## Arguments

- data_list:

  A named list of data frames, (e.g., across countries or years).

- var_name:

  A string specifying the variable name (e.g., "dhi", "pilabour") to
  compute the Gini index on.

- wgt_name:

  An optional string specifying the weight variable to be used. If
  `NULL`, equal weights are assumed.

- na.rm:

  Logical. If `TRUE`, missing values in `var_name` and `wgt_name` are
  removed.

## Value

A named list. Each list element is named by country and contains a named
numeric vector, where the names are years and the values are the
computed statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(purrr)
library(dplyr)

datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)

datasets %>% 
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%  
 apply_iqr_top_bottom_coding("dhi", "hwgt") %>%  
 apply_sqrt_equivalisation("dhi") %>% 
 run_weighted_gini("dhi", "new_wgt")
} # }
```
