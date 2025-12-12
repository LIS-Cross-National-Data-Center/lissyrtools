# Retrieve the current version of lissyrtools from the GitHub repository

Fetches the latest version of the `lissyrtools` package from its
DESCRIPTION file on GitHub. This allows comparison with the locally
installed version. Using an outdated version may result in missing
support for the most recent countries, datasets, or variable revisions,
which can cause inaccurate outputs in functions that rely on up-to-date
metadata.

## Usage

``` r
check_github_version()
```

## Value

A character string indicating the current version on GitHub.

## Examples

``` r
if (FALSE) { # \dontrun{
library(lissyrtools)
library(utils)
library(assertthat)

local_version <- as.character(utils::packageVersion("lissyrtools"))
remote_github_version <- check_github_version()

assertthat::assert_that(local_version == remote_github_version)
} # }
```
