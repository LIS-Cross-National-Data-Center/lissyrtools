# Load Dataset Names Based on Criteria

Internal function to filter and return dataset names based on input
criteria such as specified iso2 codes or ccyy pairs, database type (LWS
or LIS), and optional year range.

## Usage

``` r
load_datasets(data = NULL, lws = FALSE, from = NULL, to = NULL)
```

## Arguments

- data:

  Optional character vector specifying ccyy pairs or iso2 codes.

- lws:

  Logical indicating whether to use the LWS or LIS database.

- from:

  Optional numeric lower bound for dataset years.

- to:

  Optional numeric upper bound for dataset years.

## Value

A character vector of dataset names matching the criteria.
