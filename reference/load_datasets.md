# Load Dataset Names Based on Criteria

Internal function to filter and return dataset names based on input
criteria such as specified iso2 codes or ccyy pairs, database type (LIS,
LWS or LCS), and optional year range.

## Usage

``` r
load_datasets(data = NULL, database = "lis", from = NULL, to = NULL)
```

## Arguments

- data:

  Optional character vector specifying ccyy pairs or iso2 codes.

- database:

  A character value. One of "lis" (default), "lws", or "lcs".

- from:

  Optional numeric lower bound for dataset years.

- to:

  Optional numeric upper bound for dataset years.

## Value

A character vector of dataset names matching the criteria.
