# Filter a variable by age

**\[deprecated\]** 'transform_restrict_age()' recodes the indicated
variable to NA when age is outside the boundaries. Cases where age is NA
are also recoded to NA. To remove rows outside of age boundaries use
'transform_filter_age()' instead.

## Usage

``` r
transform_restrict_age(lissy_files, variable, from, to)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- variable:

  A character string with the name of the variable that should be
  adjusted.

- from:

  Low age

- to:

  High age

## Value

A list of tibbles with the adjusted variable.
