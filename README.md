
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vistas

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/vistas)](https://CRAN.R-project.org/package=vistas)
<!-- badges: end -->

The vistas package provides tools for graphically analysing time series,
with exploration of trend and seasonality. It utilises the tsibble data
format for time series and produces plots with ggplot2.

## Installation

You can install the development version of vistas from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tidyverts/vistas")
```

## Example

``` r
library(vistas)
library(tsibble)
#> 
#> Attaching package: 'tsibble'
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, union
tsibbledata::aus_production %>% 
  autoplot(Bricks)
#> Warning: Removed 20 rows containing missing values (`geom_line()`).
```

<img src="man/figures/README-example-1.png" width="100%" />
