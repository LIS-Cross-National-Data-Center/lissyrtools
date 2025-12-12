# Equivalise by number of household members.

**\[superseded\]** Scales a variable dividing it by 'nhhmem'^eq_scale.
Where 'nhhmem' is the LIS or LWS variable measuring the number of
members in the household.

Throws a warning if the function is applied to a person-level variable.

## Usage

``` r
transform_equivalise(lissy_files, variable, eq_scale = 0.5)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files

- variable:

  A character vector of length one with the indicator that needs to be
  transformed.

- eq_scale:

  A real number.

## Value

A list of tibbles with the transformed variable.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
transform_equivalise(list_files = lissy_files, variable = "dhi")
} # }
```
