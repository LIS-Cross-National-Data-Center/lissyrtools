# Filter a variable for household heads

**\[deprecated\]** Recodes the specified variable to NA for
non-household heads.

## Usage

``` r
transform_restrict_to_household_heads(lissy_files, variable)
```

## Arguments

- lissy_files:

  A list of LIS or LWS p-level files.

- variable:

  A character string with the name of the variable that should be
  recoded for non-household heads.

## Value

A list of tibbles with the adjusted variable.
