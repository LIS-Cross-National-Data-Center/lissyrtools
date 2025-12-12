# Determine the level of the file

**\[deprecated\]**

## Usage

``` r
determine_file_level(lissy_files, files_level)
```

## Arguments

- lissy_files:

  A list of LIS or LWS files.

- files_level:

  A string indicating the level of the file. Valid inputs are:
  'household', 'h', 'person' or 'p'. If NULL (default), the file level
  will be retrived from the 'lissy_files' attributes.

## Value

A character vector with the level of the file
