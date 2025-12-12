# Equivalise with the OECD scale

**\[superseded\]** Scales a variable using a weight for the adults
(excluding the first one) and children.

Throws a warning if the function is applied to a person-level variable.

## Usage

``` r
transform_equivalise_oecd(
  lissy_files,
  variable,
  value_other_adults = 0.7,
  value_children = 0.5
)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files

- variable:

  A character vector of length one with the indicator that needs to be
  transformed.

- value_other_adults:

  A real number. Defaults to 0.7. The value assigned to the second to
  last adults in the household.

- value_children:

  A real number. Defaults to 0.7. The value assigned to children in the
  household.

## Value

A list of tibbles with the transformed variable.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_files <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
transform_equivalise_oecd(list_files = lissy_files, variable = "dhi")
} # }
```
