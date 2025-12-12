# Compute Percentile Ratios

This function calculates the ratio between two percentiles (e.g.,
P90/P10) for each dataset in a list.

## Usage

``` r
run_weighted_ratios(
  data_list,
  var_name,
  wgt_name = NULL,
  upper_percentile,
  lower_percentile,
  type = c("type_4", "type_2"),
  na.rm = TRUE
)
```

## Arguments

- data_list:

  A named list of data frames.

- var_name:

  A string. The name of the variable to analyze (e.g., "dhi", "income").

- wgt_name:

  A string (optional). The name of the weight variable. If `NULL`, equal
  weights are assumed.

- upper_percentile:

  A numeric scalar (between 0 and 1). The higher percentile (e.g., 0.9).

- lower_percentile:

  A numeric scalar (between 0 and 1). The lower percentile (e.g., 0.1).

- type:

  A character string indicating the percentile type used in
  `compute_weighted_percentiles`. Defaults to `"type_4"`.

- na.rm:

  Logical. Should missing values be removed before computation? Default
  is `TRUE`.

## Value

A named list. Each element is named by country and contains a named
numeric vector, where names are years and values are the percentile
ratios.

## Details

If `upper_percentile` is less than `lower_percentile`, the values are
automatically swapped and a warning is issued.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(purrr)
library(dplyr)

datasets <- lissyrtools::lissyuse(data = c("de", "es", "uk"), vars = c("dhi"), from = 2016)

datasets <- datasets %>% 
 map(~ .x %>% filter(!is.na(dhi))) %>%
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%
 apply_iqr_top_bottom_coding("dhi", "hwgt", type = "type_2") %>%
 apply_sqrt_equivalisation("dhi")


# Compute dhi  ratios

p90_p10 <- run_weighted_ratios(datasets, var_name = "dhi", wgt_name = "hwgt", 
                                upper_percentile = 0.9, lower_percentile = 0.1)
                                
p90_p50 <- run_weighted_ratios(datasets, var_name = "dhi", wgt_name = "hwgt", 
                                upper_percentile = 0.9, lower_percentile = 0.5) 
                                                                
p80_p20 <- run_weighted_ratios(datasets, var_name = "dhi", wgt_name = "hwgt", 
                                upper_percentile = 0.8, lower_percentile = 0.2) 
print(p90_p10)
print(p90_p50) 
print(p80_p20)
} # }
```
