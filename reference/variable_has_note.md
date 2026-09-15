# Verify if a given variable has a note, for a selected group of countries, in LIS or LWS databases.

Verify if a given variable has a note, for a selected group of
countries, in LIS or LWS databases.

## Usage

``` r
variable_has_note(variable, iso2, database = "lis", ...)
```

## Arguments

- variable:

  A unit-length character vector containing specified LIS/LWS variables.

- iso2:

  A character vector with valid iso2 codes of countries present in
  LIS/LWS.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

## Value

A list, made of character vectors. Each elements corresponds to a
country in LIS or LWS databases.

## Examples
