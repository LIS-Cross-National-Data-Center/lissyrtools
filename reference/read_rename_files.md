# Rename read files

' **\[superseded\]** Uses a character vector to manually change the
names files that have been previously read.

## Usage

``` r
read_rename_files(list_files, new_names)
```

## Arguments

- list_files:

  A list of LIS or LWS datasets.

- new_names:

  A character string with the new names of files.

## Value

A list of loaded files with the new names.

## Examples

``` r
if (FALSE) { # \dontrun{
files <- read_lissy_files_locally(c("fr84h", "fr94h", "fr10h"))
files %<>%
    read_rename_files(new_names = c("France_1984_household",
                                    "France_1994_household",
                                    "France_2010_household"))
} # }
```
