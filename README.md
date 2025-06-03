

<!-- README.md is generated from README.qmd. Please edit that file -->

# ggtime

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggtime.png)](https://CRAN.R-project.org/package=ggtime)
[![R-CMD-check](https://github.com/mitchelloharawild/ggtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mitchelloharawild/ggtime/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The ggtime package extends the capabilities of ‘ggplot2’ by providing
grammatical elements and plot helpers designed for visualizing time
series patterns. These functions use calendar structures implemented in
the mixtime package to help explore common time series patterns
including trend, seasonality, cycles, and holidays.

The plot helper functions make use of the tsibble data format in order
to quickly and easily produce common time series plots. These plots can
also be constructed with the underlying grammar elements, which allows
greater flexibility in producing custom time series visualisations.

## Installation

You can install the **stable** version from
[CRAN](https://cran.r-project.org/package=ggtime):

``` r
install.packages("ggtime")
```

You can install the development version of ggtime from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tidyverts/ggtime")
```

## Example

The simplest time series visualisation is the time plot, which can be
created with the `autoplot()` plot helper function on a tsibble object.

``` r
library(ggtime)
library(tsibble)
library(ggplot2)
tsibbledata::aus_production %>% 
  autoplot(Bricks)
```

<img src="man/figures/README-timeplot-1.png" style="width:100.0%" />

To view the shape of the annual seasonal pattern, a seasonal plot
created with `gg_season()` is commonly used. This makes it easier to
identify the peaks, troughts, and overall shape of the seasonality.

``` r
tsibbledata::aus_production %>% 
  gg_season(Bricks)
```

<img src="man/figures/README-seasonplot-1.png" style="width:100.0%" />
