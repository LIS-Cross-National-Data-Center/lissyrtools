# Apply equivalise with the OECD scale

**\[superseded\]** Applies the equivalisation by number of adults and
children in a household.

## Usage

``` r
implement_equivalise_oecd(
  file,
  file_name,
  variable,
  value_other_adults = 0.7,
  value_children = 0.5
)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A string with the variable to which equivalisation should be applied.

- value_other_adults:

  A real number. Defaults to 0.7. The value assigned to the second to
  last adults in the household.

- value_children:

  A real number. Defaults to 0.7. The value assigned to children in the
  household.

## Value

A a file with the equivalised variable.

## Details

To be used inside transform_equivalise_oecd().
