# lissyrtools

## Build & Testing Status
<!-- badges: start -->
  [![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/nationalaccountslis/lissyrtools)
  [![codecov](https://codecov.io/gh/LIS-Cross-National-Data-Center/lissyrtools/graph/badge.svg?token=kd2zXPsfWz)](https://codecov.io/gh/LIS-Cross-National-Data-Center/lissyrtools)
  <!-- badges: end -->

## Overview
Tools for computing inequality estimates in the [LIS Data Center](https://www.lisdatacenter.org/) LISSY environment.

It allows users to:
* Read LIS data within the LISSY environment.
* Carry out commonly performed data cleaning tasks.
* Compute and plot estimates from microdata.

## Version
This package is currently in Beta version.

For questions and help, email usersupport(add)lisdatacenter.org

## Installation
The package is already installed in LISSY by the LIS Data Center team.

You can install the package locally to work with your own data or with the [LIS Sample Datasets](https://www.lisdatacenter.org/resources/self-teaching/) from this GitHub repo with:
```r
devtools::install_github("https://github.com/LIS-Cross-National-Data-Center/lissyrtools")
```


## Usage

### LISSY version
```r
library(lissyrtools)
library(magrittr)

# Read the datasets
files_h <- read_lissy_files(c("ca14h", "ca15h", "ca16h", "ca17h", "ca18h", "ca19h"))
files_p <- read_lissy_files(c("ca14p", "ca15p", "ca16p", "ca17p", "ca18p", "ca19p"))

# Merge household and person-level files
lissy_datasets <- merge_dataset_levels(files_h, files_p)

# Clean target variables:
## pi11
lissy_datasets_transformed <- lissy_datasets %>%
  transform_false_zeros_to_na("pi11") %>%
  transform_negative_values_to_zero("pi11") %>%
  transform_zeros_to_na("pi11") %>%
  transform_top_code_with_iqr("pi11") %>%
  transform_bottom_code_with_iqr("pi11") %>%
  transform_adjust_by_lisppp("pi11") %>%
  transform_restrict_age("pi11", from = 16, to = 64)

## dhi
lissy_datasets_transformed <- lissy_datasets_transformed %>%
  transform_false_zeros_to_na("dhi") %>%
  transform_negative_values_to_zero("dhi") %>%
  transform_top_code_with_iqr("dhi") %>%
  transform_bottom_code_with_iqr("dhi") %>%
  transform_equivalise("dhi") %>%
  transform_adjust_by_lisppp("dhi")

# Compute indicators
print_indicator(lissy_datasets_transformed,
                             variable = "dhi",
                             indicator = "gini",
                             na.rm = TRUE)
                             
print_indicator(lissy_datasets_transformed,
                             variable = "pi11",
                             indicator = "gini",
                             na.rm = TRUE)

# Compute and plot indicators                        
plot_indicator(lissy_datasets_transformed, variable = "dhi",
                             indicator = "gini",
                             na.rm = TRUE)

```

### Local version

When working with `lissyrtools` locally, use `read_lissy_files_locally()`
instead of `read_lissy_files()`. The file names then be passed with the
`ccyydl` (e.g. 'us16ih') format instead of `ccyyl` ('us16h'). The path to the 
files should also be specified. E.g. 

```r
files_h <- read_lissy_files_locally(c("it14ih", "us16ih", "mx18ih"),
                                    path_to_files = "path/to/your/directory/")
files_p <- read_lissy_files_locally(c("it14ip", "us16ip", "mx18ip"),
                                    path_to_files = "path/to/your/directory/")
```

## Documentation and Support
Please visit https://lis-cross-national-data-center.github.io/lissyrtools/ for documentation and vignettes with examples.

