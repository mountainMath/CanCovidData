# CanCovidData

<!-- badges: start -->
<!-- badges: end -->

The goal of CanCovidData is to provide a collection of covid-19 data import functions for international
and Canadian data, as well as some helper functions for data processing and graphing.

## Installation

You can install the released version of CanCovidData from [GitHub](https://github.com/mountainMath/CanCovidData) with:

``` r
remotes::install_github("mountainmath/CanCovidData")
```

## Example



``` r
library(CanCovidData)
provincial_data <- get_canada_official_provincial_data() 

country_data <- get_country_timeline_ecdc_jhs_data()
```

