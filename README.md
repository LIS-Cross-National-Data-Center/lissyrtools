# lissyrtools <a href="https://lis-cross-national-data-center.github.io/lissyrtools/"><img src="man/figures/lissyrtools_badge_classic.png" align="right" height="195" style="float:right; height:195px;"/></a>

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.1.2-blue.svg)](https://github.com/LIS-Cross-National-Data-Center/lissyrtools) [![codecov](https://codecov.io/gh/LIS-Cross-National-Data-Center/lissyrtools/graph/badge.svg?token=kd2zXPsfWz)](https://codecov.io/gh/LIS-Cross-National-Data-Center/lissyrtools)

<!-- badges: end -->

<br>

## Overview

A package with the tools needed to develop scripts with LIS data

It allows users to:

\* Read LIS data within the LISSY environment, or to use LIS sample files locally.

\* Carry out commonly performed data cleaning tasks.

\* Compute estimates from microdata.

## Installation

The package is already installed in LISSY by the LIS Data Center team.

You can install the package locally to work with your own data or with the [LIS Sample Datasets](https://www.lisdatacenter.org/resources/self-teaching/) from this GitHub repo with:

``` r
devtools::install_github("https://github.com/LIS-Cross-National-Data-Center/lissyrtools")
```

## Usage

**lissyrtools** provides its users with a set of functions and embedded objects designed to help users access and manipulate data in LIS's remote execution system: [LISSY](https://www.lisdatacenter.org/data-access/lissy/). By providing built-in sample datasets in **lissyrtools**, we also encourage users to develop their LISSY scripts locally, where debugging and writing R code are more efficient in IDEs like RStudio.

Data first needs to be loaded using the [lissyuse()](https://lis-cross-national-data-center.github.io/lissyrtools/reference/lissyuse.html) function, which creates the list `lis_datasets` or `lws_datasets` if its argument `lws` = TRUE. Subsequently, the list can be transformed using other functions from **lissyrtools** in a pipeline structure, enabling users to generate aggregated figures for the entire dataset or specific subgroups.

### LISSY version

``` r
library(lissyrtools)
library(dplyr)

# Load the datasets 
# The output is a list whose elements are the datasets available in the LIS database for the countries selected within the specified time frame. 

lis_datasets <- lissyuse(
  data = "ca", 
  vars = c("dhi", "pi11", "age"), 
  from = 2014, 
  to = 2019
  ) 


# Example of further data cleaning using `transform_` functions:

lissy_datasets_transformed <- lis_datasets %>%
  transform_false_zeros_to_na("pi11") %>%
  transform_negative_values_to_zero("pi11") %>%
  transform_zeros_to_na("pi11") %>%
  transform_top_code_with_iqr("pi11") %>%
  transform_bottom_code_with_iqr("pi11") %>%
  transform_adjust_by_lisppp("pi11") %>%
  transform_restrict_age("pi11", from = 16, to = 64)


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


# To load LWS datasets, set the argument `lws` == TRUE:
lws_datasets <- lissyuse(
  data = c("us", "uk17", "uk19"), 
  vars = "dnw", 
  from = 2015, 
  to = 2021,
  lws = TRUE
)

names(lws_datasets)
```

### Local version

When working with `lissyrtools` locally, use `lissyuse()`, along with the [sample files](https://www.lisdatacenter.org/resources/self-teaching/) made available in the package. For LIS we have the following, for LWS we have this other ones .....

``` r
lissyuse(data = c("it14", "us16", "mx18"), lws = FALSE)
                              
```

## User Support

If you encounter any bugs, typos, or experience any issue while running jobs including this packages' tools, please email us at: [usersupport\@lisdatacenter.org](mailto:usersupport@lisdatacenter.org){.email}.

For more information about LIS, visit our [website](https://www.lisdatacenter.org/), explore [METIS](https://www.lisdatacenter.org/frontend#/home) for metadata, and check out our best practices for [job submission in LISSY](https://www.lisdatacenter.org/data-access/lissy/syntax/).

Recommended checks on other packages that offer tools for data manipulation and list handling: [magrittr](https://magrittr.tidyverse.org/), [dplyr](https://dplyr.tidyverse.org/) and [purrr](https://purrr.tidyverse.org/)
