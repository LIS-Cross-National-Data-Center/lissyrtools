# Compute Weighted Atkinson Across a List of Data Frames

Compute Weighted Atkinson Across a List of Data Frames

## Usage

``` r
run_weighted_atkinson(
  data_list,
  var_name,
  wgt_name = NULL,
  epsilon,
  na.rm = TRUE
)
```

## Arguments

- data_list:

  A named list of data frames, (e.g., across countries or years).

- var_name:

  A string specifying the variable name (e.g., "dhi", "pilabour") to
  compute the Atkinson index on.

- wgt_name:

  An optional string specifying the weight variable to be used. If
  `NULL`, equal weights are assumed.

- epsilon:

  A positive inequality aversion parameter. Must be greater than 0.

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
 run_weighted_atkinson("dhi", "new_wgt", epsilon = 0.5)
 
 # Negative values are not allowed in the variable for which we are computing the Atkinson index.
 # If we remove the top and bottom coding stage from the example above, we will get an error with a warning regarding the datasets containing negative values.
 
datasets[1:4] %>% 
 map(~ .x %>% mutate(new_wgt = hwgt * nhhmem)) %>%  
 apply_sqrt_equivalisation("dhi") %>% 
 run_weighted_atkinson("dhi", "new_wgt", epsilon = 0.5)  
} # }
```
