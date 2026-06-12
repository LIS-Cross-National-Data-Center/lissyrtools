# Inspect the Labels of LIS and LWS variables

Inspect the Labels of LIS and LWS variables

## Usage

``` r
variable_labels(vars = NULL, pattern = NULL)
```

## Arguments

- vars:

  A character vector containing LIS/LWS variables or the output list
  from lissyuse.

- pattern:

  A character.

## Value

A character vector with the corresponding labels for the selected
variables, or where the pattern was detected in its label.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1) Without any argument:
variable_labels()

# 2) Using with the outputed list from lissyuse:
lis_datasets <- lissyuse(data = c("uk"), vars = c("hpub_i","hpub_u", "hi42", "hi421", "hi422", "hi43"), from = 2016)
variable_labels(vars = lis_datasets)

# 3) Using a character vector with LIS/LWS variables:
variable_labels(vars = c("fyft", "basb", "hxremit", "bafi1_c", "pasodc"))

# 4) Using pattern argument: 
variable_labels(pattern = "occupa")
variable_labels(pattern = "capital")
} # }
```
