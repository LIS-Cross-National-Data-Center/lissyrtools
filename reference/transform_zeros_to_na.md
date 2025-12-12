# Recodes zeros into missing values

**\[superseded\]** Recodes all zeros in the selected variable into
missing valuse (NAs).

## Usage

``` r
transform_zeros_to_na(lissy_files, variable)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string with the name of the variable that should be
  adjusted

## Value

A list of tibbles with the adjusted variable.
