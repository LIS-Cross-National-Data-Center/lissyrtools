# Bind multiple LIS or LWS datasets

**\[deprecated\]** Converts a list of LIS or LWS datasets into a single
file. Creates a new 'file' variable with the name of the dataset and
computes unique person and household identifiers if 'create_unique_id =
TRUE' (default).

Adds the attribute 'binded = TRUE' to the resulting tibble.

## Usage

``` r
bind_lissy_files(lissy_files, create_unique_id = TRUE)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- create_unique_id:

  Computes new variables 'unique_hid' and 'unique_pid' as unique row
  identifiers. Defaults to TRUE.

## Value

A tibble with the stacked LIS or LWS files.

## Examples

``` r
if (FALSE) { # \dontrun{
lissy_datasets_as_list <- read_lissy_files(c("fr84h", "fr94h", "fr10h"))
 lissy_datasets_as_tibble <- bind_lissy_files(lissy_datasets = lissy_datasets_as_list, create_unique_id = TRUE)
} # }
```
