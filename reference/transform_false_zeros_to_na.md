# Recode zeros into missing values if all values are zero

**\[superseded\]** Recodes all zeros in the selected variable into
missing values (NAs) if (and only if) all values are zeros.

## Usage

``` r
transform_false_zeros_to_na(lissy_files, variable)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string with the name of the variable that should be
  adjusted.

## Value

A list of tibbles with the adjusted variable.

## Details

Some LIS/LWS datasets have variables with only '0' values. These do not
represent 0s but NAs. This function transform the 0s into NAs if it
finds that all values are 0s and there are no other valid values.
