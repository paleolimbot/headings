
<!-- README.md is generated from README.Rmd. Please edit that file -->

# headings

<!-- badges: start -->

[![R-CMD-check](https://github.com/paleolimbot/headings/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/headings/actions)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/headings/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/headings?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of headings is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/headings")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(headings)

# totally bogus since 0 and 360 are identical!
mean(c(350, 355, 360, 0, 5, 10))
#> [1] 180
sd(c(350, 355, 360, 0, 5, 10))
#> [1] 191.7551

# everybody wins!
hdg_mean(c(350, 355, 360, 0, 5, 10))
#> [1] 0
hdg_sd(c(350, 355, 360, 0, 5, 10))
#> [1] 7.071068
```

Practically, heading arithmetic can be used to summarize climate and/or
current directions over time.

``` r
library(tidyverse)
library(weathercan)

# get some climate data
kam <- weathercan::weather_dl(
  station_ids = 51423,
  start = "2016-01-01",
  end = "2016-12-31"
)
#> As of weathercan v0.3.0 time display is either local time or UTC
#> See Details under ?weather_dl for more information.
#> This message is shown once per session

kam %>% 
  group_by(month) %>% 
  summarise(
    mean(wind_dir, na.rm = TRUE),
    sd(wind_dir, na.rm = TRUE)
  )
#> `summarise()` ungrouping output (override with `.groups` argument)
#> # A tibble: 12 x 3
#>    month `mean(wind_dir, na.rm = TRUE)` `sd(wind_dir, na.rm = TRUE)`
#>    <chr>                          <dbl>                        <dbl>
#>  1 01                              17.0                        10.6 
#>  2 02                              14.6                         8.88
#>  3 03                              16.3                         9.43
#>  4 04                              19.1                        10.1 
#>  5 05                              20.3                         9.73
#>  6 06                              17.8                         9.66
#>  7 07                              19.5                         9.38
#>  8 08                              21.5                         9.10
#>  9 09                              19.2                        10.6 
#> 10 10                              15.7                         9.49
#> 11 11                              13.1                         8.19
#> 12 12                              18.9                        11.2
```
