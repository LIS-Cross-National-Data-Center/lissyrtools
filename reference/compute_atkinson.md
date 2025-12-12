# Compute Atkinson index.

**\[superseded\]**

Compute the Atkinson index with weights.

## Usage

``` r
compute_atkinson(
  file,
  file_name,
  variable,
  epsilon,
  weight = NULL,
  na.rm = FALSE
)
```

## Arguments

- file:

  A LIS or LWS file.

- file_name:

  The name of the LIS or LWS file.

- variable:

  A string with the name of the variable for which Atkinson should be
  computed.

- epsilon:

  A number with the inequality adversion parameter. Needs to be epsilon
  \> 0.

- weight:

  A string with the name of the variable in 'file' that should be used
  as sample weights.

- na.rm:

  A boolean. Indicates if NAs should be ignored. Defaults to FALSE.

## Value

A numeric vector with the gini index.
