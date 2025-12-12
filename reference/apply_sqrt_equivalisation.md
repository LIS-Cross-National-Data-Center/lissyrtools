# Apply Square-Root Equivalisation to a Variable

This function adjusts a household-level variable (e.g., "dhi",
"hicapital") for household size using a square-root scale equivalisation
method. It divides the variable by the number of household members
raised to a power (`eq_scale`), which defaults to 0.5 (i.e., the square
root equivalence scale).

## Usage

``` r
apply_sqrt_equivalisation(data_list, var_name, eq_scale = 0.5)
```

## Arguments

- data_list:

  A named list of data frames, from LIS or LWS microdata.

- var_name:

  A string. The name of the variable to be equivalised.

- eq_scale:

  A numeric value between 0 and 1. The equivalence scale to apply.
  Defaults to `0.5`, corresponding to square-root equivalisation.

## Value

A list of data frames with the equivalised variable.

## Details

The function assumes that the dataset contains a column named `nhhmem`
representing the number of household members. If this column is missing,
the function stops with an error.

It also issues a warning if the target variable is detected as an
individual-level variable, since equivalisation is typically applied to
household-level measures.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(dplyr)

# Import data, ideally at the level of the variable of interest.
data_hhd <- lissyuse("it", vars = c("dhi"), from = 2010)

# No equivalisation case
data_hhd  %>%
 purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
 run_weighted_mean("dhi", "hpopwgt") 

# Default case: square-root equivalisation
data_hhd  %>%
 purrr::map(~ .x %>% filter(!is.na(dhi))) %>%
 apply_sqrt_equivalisation("dhi") %>% 
 run_weighted_mean("dhi", "hpopwgt")
} # }
```
