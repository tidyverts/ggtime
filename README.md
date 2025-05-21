

<!-- README.md is generated from README.qmd. Please edit that file -->

# ggtime

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggtime.png)](https://CRAN.R-project.org/package=ggtime)
[![R-CMD-check](https://github.com/tidyverts/ggtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverts/ggtime/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The ggtime package provides tools for graphically analysing time series,
with exploration of trend and seasonality. It utilises the tsibble data
format for time series and produces plots with ggplot2.

## Installation

You can install the development version of ggtime from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tidyverts/ggtime")
```

## Example

``` r
library(ggtime)
library(tsibble)
library(ggplot2)
tsibbledata::aus_production %>% 
  autoplot(Bricks)
```

<img src="man/figures/README-timeplot-1.png" style="width:100.0%" />

``` r
tsibbledata::aus_production %>% 
  gg_season(Bricks)
```

<img src="man/figures/README-seasonplot-1.png" style="width:100.0%" />
