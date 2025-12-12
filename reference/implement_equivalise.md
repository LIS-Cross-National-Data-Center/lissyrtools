# Apply equivalise by number of household members

**\[superseded\]** Applies the equivalisation by number of household
members in a file for a single variable.

## Usage

``` r
implement_equivalise(file, file_name, variable, eq_scale = 0.5)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A string with the variable to which equivalisation should be applied.

- eq_scale:

  A real number. Defaults to 0.5. The variable will be equivalized using
  n_household_members^eq_scale

## Value

A a file with the equivalised variable.

## Details

To be used inside transform_equivalise().
